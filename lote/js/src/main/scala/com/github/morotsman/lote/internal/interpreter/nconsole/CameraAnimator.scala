package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.{SlidePosition, WebGLConfig}
import org.scalajs.dom

/** Animates a Three.js perspective camera between slide positions in 3-D space.
  *
  * The animator manages a [[ThreePerspectiveCamera]] whose field-of-view and distance are computed so that each slide
  * fills the viewport exactly at rest. Transitions between slides use smooth ease-in-out interpolation of the camera
  * position, look-at target, and up vector, producing a fly-through effect with real parallax.
  *
  * ==Coordinate conventions==
  * Slides are placed in world space at their [[SlidePosition]] origin (top-left corner). The camera is positioned along
  * the slide's outward surface normal at a distance that frames the slide, looking at the slide center.
  *
  * ==Zoom==
  * Interactive zoom is handled via keyboard shortcuts (`+`/`-`/`0`) which adjust the camera field-of-view without
  * moving the camera, giving an optical zoom effect.
  *
  * ==Lifecycle==
  * Call [[initSpatialMode]] once before any navigation to set up the perspective camera. Use [[moveTo]] for animated
  * transitions, [[jumpTo]] for instant repositioning, and [[cleanup]] when the animator is no longer needed (removes
  * event listeners and cancels any running animation).
  *
  * ==Performance note==
  * This class deliberately uses mutable `var` fields rather than `cats.effect.Ref` for its internal state. The
  * `animate` callback inside [[animateSpatial]] is invoked by `requestAnimationFrame` at up to 60 fps; each frame reads
  * and writes several fields (camera position, animation flag, zoom scale). Direct field access avoids the per-frame
  * allocation overhead of `Ref` (new state case-class + `IO` thunks + `flatMap` closures) and the associated GC
  * pressure that can cause micro-stutters during animation. This is safe because JavaScript is single-threaded — no two
  * fibers can race on these fields — and all public methods are called from within `Sync[F].delay` blocks at the call
  * sites.
  *
  * @param scene
  *   the [[WebGLScene]] wrapper that owns the Three.js renderer and scene graph; used to trigger re-renders and to set
  *   the active camera reference.
  */
private[nconsole] class CameraAnimator(scene: WebGLScene, config: WebGLConfig = WebGLConfig()) {

  /** Conversion factor from degrees to radians. */
  private val Deg2Rad = Math.PI / 180.0

  /** ID returned by `requestAnimationFrame`, used to cancel in-flight animations. */
  private var animationId: Int = 0

  /** Whether a fly-through animation is currently in progress. */
  private var animating: Boolean = false

  // Current camera target position (where it's settled or heading)
  /** The slide position the camera is currently at (or transitioning toward). */
  private var currentPosition: SlidePosition = SlidePosition(0, 0, 0)

  /** Whether the camera has been positioned at least once. */
  private var initialized: Boolean = false

  // Spatial mode dimensions
  /** Width of a single slide in pixels (used for framing calculations). */
  private var slideWidth: Int = 0

  /** Height of a single slide in pixels (used for framing calculations). */
  private var slideHeight: Int = 0

  // Perspective camera
  /** The Three.js perspective camera instance managed by this animator. */
  private var perspCamera: ThreePerspectiveCamera = _

  /** Base vertical field-of-view in degrees (before zoom adjustment). */
  private var baseFov: Double = config.fieldOfView

  /** Distance from the camera to the slide surface that frames the slide exactly. */
  private var cameraDistance: Double = 2000.0

  /** Exposes the internal perspective camera reference for use by [[Scene3DRef]].
    *
    * @return
    *   the [[ThreePerspectiveCamera]] managed by this animator.
    */
  private[nconsole] def perspCameraRef: ThreePerspectiveCamera = perspCamera

  // zoom
  /** Current zoom level multiplier (1.0 = no zoom). Values > 1 zoom in, < 1 zoom out. */
  private var zoomScale: Double = 1.0

  /** Keyboard listener that handles interactive zoom controls.
    *
    * Supported keys:
    *   - `+` or `=`: zoom in (increase scale by 20%, clamped at 20×)
    *   - `-` or `_`: zoom out (decrease scale by 20%, clamped to keep FOV ≤ 160°)
    *   - `0`: reset zoom to 1× and reapply the current camera pose
    */
  private val zoomKeyListener: scalajs.js.Function1[dom.KeyboardEvent, Unit] = { (ev: dom.KeyboardEvent) =>
    ev.key match {
      case "+" | "=" =>
        zoomScale = (zoomScale * 1.2).min(config.maxZoom)
        applyZoom()
      case "-" | "_" =>
        val minScale = (baseFov / config.maxFieldOfView).max(0.02)
        zoomScale = (zoomScale / 1.2).max(minScale)
        applyZoom()
      case "0" =>
        zoomScale = 1.0
        applyCameraPose(currentPosition)
        scene.render()
      case _ =>
    }
  }
  dom.document.addEventListener("keydown", zoomKeyListener)

  /** Applies the current [[zoomScale]] by adjusting the camera FOV and re-rendering.
    *
    * Zoom is implemented as an optical zoom: the camera stays in place while the field-of-view narrows (zoom in) or
    * widens (zoom out).
    */
  private def applyZoom(): Unit = {
    perspCamera.fov = baseFov / zoomScale
    perspCamera.updateProjectionMatrix()
    scene.render()
  }

  /** Initializes spatial (3-D) mode by creating the perspective camera.
    *
    * This must be called once before any calls to [[moveTo]] or [[jumpTo]]. It computes the camera distance required to
    * frame a slide of the given dimensions at the base FOV, creates the [[ThreePerspectiveCamera]], and installs it as
    * the active camera on the scene.
    *
    * @param vpWidth
    *   the viewport / slide width in pixels.
    * @param vpHeight
    *   the viewport / slide height in pixels.
    */
  def initSpatialMode(vpWidth: Int, vpHeight: Int): Unit = {
    slideWidth = vpWidth
    slideHeight = vpHeight

    // Compute the camera distance that frames a slide exactly at the base FOV
    baseFov = config.fieldOfView
    cameraDistance = (slideHeight / 2.0) / Math.tan(baseFov * Deg2Rad / 2.0)

    val aspect = slideWidth.toDouble / slideHeight.toDouble
    perspCamera = new ThreePerspectiveCamera(baseFov, aspect, config.nearClip, config.farClip)

    // Switch the scene to use the perspective camera
    scene.activeCamera = perspCamera
  }

  /** Animates the camera from its current position to the given target slide.
    *
    * On the first call (before the camera has been positioned) the camera is placed at the target immediately without
    * animation. Subsequent calls trigger a smooth fly-through using [[animateSpatial]]. If an animation is already in
    * progress it is cancelled before starting the new one.
    *
    * If the target is extremely close to the current position (< 0.1 world units) the camera is snapped without
    * animation to avoid jitter.
    *
    * @param target
    *   the [[SlidePosition]] to navigate to.
    */
  def moveTo(target: SlidePosition): Unit = {
    if (!initialized) {
      initialized = true
      currentPosition = target
      applyCameraPose(target)
      scene.render()
      return
    }

    if (animating) {
      dom.window.cancelAnimationFrame(animationId)
      animating = false
    }

    val startPos = currentPosition
    val distance = euclidean(startPos, target)

    if (distance < 0.1) {
      currentPosition = target
      applyCameraPose(target)
      scene.render()
      return
    }

    animateSpatial(startPos, target, distance)
  }

  // ---- Camera animation: perspective camera flying through 3D space ----

  /** Performs an animated camera transition between two slide positions.
    *
    * The animation interpolates camera position, look-at target, and up vector independently using the smoothstep
    * ease-in-out function. Duration scales with the square root of the Euclidean distance to keep short hops fast while
    * long journeys remain watchable.
    *
    * The animation loop is driven by `requestAnimationFrame`. On completion the zoom scale is reset to 1× and
    * [[currentPosition]] is updated.
    *
    * @param startPos
    *   the [[SlidePosition]] the camera is departing from.
    * @param target
    *   the [[SlidePosition]] the camera will arrive at.
    * @param distance
    *   the pre-computed Euclidean distance between start and target.
    */
  private def animateSpatial(startPos: SlidePosition, target: SlidePosition, distance: Double): Unit = {
    val durationMs = transitionDuration(distance)
    val startTime = dom.window.performance.now()

    // Read actual camera state (the camera may have been manipulated by a scene-aware slide)
    val startCamX = perspCamera.position.x
    val startCamY = perspCamera.position.y
    val startCamZ = perspCamera.position.z
    val startUpX = perspCamera.up.x
    val startUpY = perspCamera.up.y
    val startUpZ = perspCamera.up.z

    // LookAt target: use the slide center (the camera was looking at this)
    val (startLookX, startLookY, startLookZ) = slideCenterFor(startPos)

    // Compute target camera pose
    val (targetCamX, targetCamY, targetCamZ) = cameraPositionFor(target)
    val (targetLookX, targetLookY, targetLookZ) = slideCenterFor(target)
    val (targetUpX, targetUpY, targetUpZ) = computeUp(target)

    animating = true

    def animate(now: Double): Unit = {
      val elapsed = now - startTime
      val t = (elapsed / durationMs).min(1.0)
      val eased = easeInOut(t)

      // Interpolate camera position
      val camX = lerp(startCamX, targetCamX, eased)
      val camY = lerp(startCamY, targetCamY, eased)
      val camZ = lerp(startCamZ, targetCamZ, eased)

      // Interpolate lookAt target
      val lookX = lerp(startLookX, targetLookX, eased)
      val lookY = lerp(startLookY, targetLookY, eased)
      val lookZ = lerp(startLookZ, targetLookZ, eased)

      // Interpolate up vector and normalize
      val rawUpX = lerp(startUpX, targetUpX, eased)
      val rawUpY = lerp(startUpY, targetUpY, eased)
      val rawUpZ = lerp(startUpZ, targetUpZ, eased)
      val upLen = Math.sqrt(rawUpX * rawUpX + rawUpY * rawUpY + rawUpZ * rawUpZ).max(0.0001)
      val upX = rawUpX / upLen
      val upY = rawUpY / upLen
      val upZ = rawUpZ / upLen

      perspCamera.position.set(camX, camY, camZ)
      perspCamera.up.set(upX, upY, upZ)
      perspCamera.lookAt(lookX, lookY, lookZ)
      perspCamera.updateProjectionMatrix()
      scene.render()

      if (t < 1.0) {
        animationId = dom.window.requestAnimationFrame(animate _)
      } else {
        animating = false
        currentPosition = target
        zoomScale = 1.0
      }
    }

    animationId = dom.window.requestAnimationFrame(animate _)
  }

  /** Positions the perspective camera to frame the given slide without animation.
    *
    * Sets the camera position along the slide's outward normal at [[cameraDistance]], orients the up vector, adjusts
    * FOV for the current zoom level, and calls `lookAt` on the slide center.
    *
    * @param pos
    *   the [[SlidePosition]] to frame.
    */
  private def applyCameraPose(pos: SlidePosition): Unit = {
    val (camX, camY, camZ) = cameraPositionFor(pos)
    val (lookX, lookY, lookZ) = slideCenterFor(pos)
    val (upX, upY, upZ) = computeUp(pos)

    // Apply debug zoom via FOV
    perspCamera.fov = baseFov / zoomScale
    perspCamera.position.set(camX, camY, camZ)
    perspCamera.up.set(upX, upY, upZ)
    perspCamera.lookAt(lookX, lookY, lookZ)
    perspCamera.updateProjectionMatrix()
  }

  /** Immediately repositions the camera to the given slide without animation.
    *
    * Any in-progress animation is cancelled. Unlike [[moveTo]], this method never triggers an animated transition
    * regardless of distance.
    *
    * @param target
    *   the [[SlidePosition]] to jump to.
    */
  def jumpTo(target: SlidePosition): Unit = {
    if (animating) {
      dom.window.cancelAnimationFrame(animationId)
      animating = false
    }
    initialized = true
    currentPosition = target
    applyCameraPose(target)
    scene.render()
  }

  /** Returns `true` if a fly-through animation is currently in progress.
    *
    * @return
    *   whether the camera is mid-transition.
    */
  def isAnimating: Boolean = animating

  /** Cleans up resources held by this animator.
    *
    * Cancels any running animation frame callback and removes the keyboard event listener for zoom. Call this when the
    * animator is being discarded to avoid memory leaks and stale event handlers.
    */
  def cleanup(): Unit = {
    if (animating) {
      dom.window.cancelAnimationFrame(animationId)
      animating = false
    }
    dom.document.removeEventListener("keydown", zoomKeyListener)
  }

  // --- Helpers ---

  /** Computes the center of a slide in world space.
    *
    * The slide's [[SlidePosition]] origin is at its top-left corner, so the center is offset by half the slide width
    * and height along x and y.
    *
    * @param pos
    *   the slide position (top-left origin).
    * @return
    *   a tuple `(cx, cy, cz)` representing the slide center in world coordinates.
    */
  private def slideCenterFor(pos: SlidePosition): (Double, Double, Double) =
    (pos.x + slideWidth / 2.0, pos.y + slideHeight / 2.0, pos.z)

  /** Computes the world-space camera position that frames the given slide.
    *
    * The camera is placed at the slide center offset along the outward surface normal by [[cameraDistance]].
    *
    * @param pos
    *   the slide position to frame.
    * @return
    *   a tuple `(x, y, z)` for the camera position in world coordinates.
    */
  private def cameraPositionFor(pos: SlidePosition): (Double, Double, Double) = {
    val (cx, cy, cz) = slideCenterFor(pos)
    val (nx, ny, nz) = computeNormal(pos)
    (cx + nx * cameraDistance, cy + ny * cameraDistance, cz + nz * cameraDistance)
  }

  /** Computes the outward-facing normal of a slide given its rotation.
    *
    * The slide's local +Z axis points outward. Applying the Euler XYZ rotation (R = Rx · Ry · Rz) transforms local Z
    * into world space. The result is the third column of the combined rotation matrix (only rotX and rotY affect it
    * because rotZ rotates around the local Z axis itself).
    *
    * @param pos
    *   the slide position whose rotation to use.
    * @return
    *   a unit vector `(nx, ny, nz)` pointing away from the slide surface.
    */
  private def computeNormal(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    // Third column of Rx * Ry * Rz: (sin(ry), -sin(rx)*cos(ry), cos(rx)*cos(ry))
    (s2, -s1 * c2, c1 * c2)
  }

  /** Computes the up vector of a slide given its rotation (Euler XYZ order).
    *
    * The up vector is the second column of the rotation matrix R = Rx · Ry · Rz, which transforms the local Y axis into
    * world space. All three rotation components (rotX, rotY, rotZ) contribute to the result.
    *
    * @param pos
    *   the slide position whose rotation to use.
    * @return
    *   a unit vector `(ux, uy, uz)` representing the slide's "up" in world space.
    */
  private def computeUp(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val rz = pos.rotZ * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    val c3 = Math.cos(rz); val s3 = Math.sin(rz)
    // Second column of Rx * Ry * Rz
    (-c2 * s3, c1 * c3 - s1 * s2 * s3, s1 * c3 + c1 * s2 * s3)
  }

  /** Computes the Euclidean distance between two slide positions in 3-D space.
    *
    * Only the translational components (x, y, z) are considered; rotations are ignored.
    *
    * @param a
    *   the first position.
    * @param b
    *   the second position.
    * @return
    *   the straight-line distance between `a` and `b`.
    */
  private def euclidean(a: SlidePosition, b: SlidePosition): Double = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    val dz = b.z - a.z
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  /** Computes the animation duration in milliseconds for a given travel distance.
    *
    * Uses a base duration plus a square-root scaling factor so that short hops complete quickly while longer journeys
    * take proportionally less extra time (sub-linear growth).
    *
    * @param distance
    *   the Euclidean distance to travel (world units).
    * @return
    *   duration in milliseconds.
    */
  private def transitionDuration(distance: Double): Double = {
    val baseMs = config.cameraTransitionBaseMs
    val kFactor = config.cameraTransitionFactor
    baseMs + kFactor * Math.sqrt(distance)
  }

  /** Smoothstep ease-in-out interpolation function.
    *
    * Maps an input `t` in [0, 1] to a smooth S-curve using the Hermite polynomial `3t² − 2t³`. Values outside [0, 1]
    * are clamped.
    *
    * @param t
    *   the raw interpolation parameter (typically elapsed / duration).
    * @return
    *   the eased value in [0, 1].
    */
  private def easeInOut(t: Double): Double = {
    val c = t.max(0.0).min(1.0)
    c * c * (3.0 - 2.0 * c)
  }

  /** Linearly interpolates between two values.
    *
    * @param a
    *   the start value (returned when `t` = 0).
    * @param b
    *   the end value (returned when `t` = 1).
    * @param t
    *   the interpolation parameter.
    * @return
    *   `a + (b - a) * t`.
    */
  private def lerp(a: Double, b: Double, t: Double): Double =
    a + (b - a) * t
}
