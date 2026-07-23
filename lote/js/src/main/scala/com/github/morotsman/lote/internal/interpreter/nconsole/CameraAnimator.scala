package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.{SlidePosition, WebGLConfig}
import org.scalajs.dom

/** Animates a perspective camera between slide positions in 3-D space.
  *
  * The animator manages a camera whose field-of-view and distance are computed so that each slide fills the viewport
  * exactly at rest. Transitions between slides use smooth ease-in-out interpolation of the camera position, look-at
  * target, and up vector, producing a fly-through effect with real parallax.
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
  * @param backend
  *   the [[SceneBackend]] that provides scene graph and camera operations; in production this is backed by WebGLScene +
  *   Three.js, in tests by [[StubSceneBackend]].
  * @param config
  *   rendering configuration (field-of-view, transition timing, zoom limits, etc.)
  * @param scheduler
  *   the [[AnimationScheduler]] for requestAnimationFrame; in tests use [[ManualScheduler]]
  * @param enableZoomListener
  *   if true (default), installs a DOM keydown listener for interactive zoom. Set to false in tests.
  */
private[nconsole] class CameraAnimator(
    backend: SceneBackend,
    config: WebGLConfig = WebGLConfig(),
    scheduler: AnimationScheduler = DomAnimationScheduler,
    enableZoomListener: Boolean = true
) {

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

  // Perspective camera reference (path-dependent type from backend)
  private var _cameraRef: backend.CameraRef = _

  /** Exposes the internal camera reference for use by [[Scene3DRef]] and [[SpatialState]]. */
  private[nconsole] def cameraRef: Any = _cameraRef

  /** Base vertical field-of-view in degrees (before zoom adjustment). */
  private var baseFov: Double = config.fieldOfView

  /** Distance from the camera to the slide surface that frames the slide exactly. */
  private var cameraDistance: Double = 2000.0

  // zoom
  /** Current zoom level multiplier (1.0 = no zoom). Values > 1 zoom in, < 1 zoom out. */
  private var zoomScale: Double = 1.0

  /** Handles a zoom key press. Exposed as package-private for testing without DOM events.
    *
    * Supported keys:
    *   - `+` or `=`: zoom in (increase scale by 20%, clamped at maxZoom)
    *   - `-` or `_`: zoom out (decrease scale by 20%, clamped to keep FOV ≤ maxFieldOfView)
    *   - `0`: reset zoom to 1× and reapply the current camera pose
    */
  private[nconsole] def handleZoomKey(key: String): Unit = {
    key match {
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
        backend.render(_cameraRef)
      case _ =>
    }
  }

  /** Keyboard listener that handles interactive zoom controls. */
  private val zoomKeyListener: scalajs.js.Function1[dom.KeyboardEvent, Unit] = { (ev: dom.KeyboardEvent) =>
    handleZoomKey(ev.key)
  }

  if (enableZoomListener) {
    dom.document.addEventListener("keydown", zoomKeyListener)
  }

  /** Applies the current [[zoomScale]] by adjusting the camera FOV and re-rendering. */
  private def applyZoom(): Unit = {
    backend.setCameraFov(_cameraRef, baseFov / zoomScale)
    backend.updateCameraProjection(_cameraRef)
    backend.render(_cameraRef)
  }

  /** Initializes spatial (3-D) mode by creating the perspective camera.
    *
    * This must be called once before any calls to [[moveTo]] or [[jumpTo]]. It computes the camera distance required to
    * frame a slide of the given dimensions at the base FOV, creates the camera via the backend, and stores the
    * reference.
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
    _cameraRef = backend.createCamera(baseFov, aspect, config.nearClip, config.farClip)
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
      backend.render(_cameraRef)
      return
    }

    if (animating) {
      scheduler.cancelFrame(animationId)
      animating = false
    }

    val startPos = currentPosition
    val distance = euclidean(startPos, target)

    if (distance < 0.1) {
      currentPosition = target
      applyCameraPose(target)
      backend.render(_cameraRef)
      return
    }

    animateSpatial(startPos, target, distance)
  }

  // ---- Camera animation: perspective camera flying through 3D space ----

  private def animateSpatial(startPos: SlidePosition, target: SlidePosition, distance: Double): Unit = {
    val durationMs = transitionDuration(distance)
    val startTime = scheduler.now()

    // Read actual camera state (the camera may have been manipulated by a scene-aware slide)
    val (startCamX, startCamY, startCamZ) = backend.getCameraPosition(_cameraRef)
    val (startUpX, startUpY, startUpZ) = backend.getCameraUp(_cameraRef)

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

      backend.setCameraPosition(_cameraRef, camX, camY, camZ)
      backend.setCameraUp(_cameraRef, upX, upY, upZ)
      backend.setCameraLookAt(_cameraRef, lookX, lookY, lookZ)
      backend.updateCameraProjection(_cameraRef)
      backend.render(_cameraRef)

      if (t < 1.0) {
        animationId = scheduler.requestFrame(animate _)
      } else {
        animating = false
        currentPosition = target
        zoomScale = 1.0
      }
    }

    animationId = scheduler.requestFrame(animate _)
  }

  /** Positions the perspective camera to frame the given slide without animation. */
  private def applyCameraPose(pos: SlidePosition): Unit = {
    val (camX, camY, camZ) = cameraPositionFor(pos)
    val (lookX, lookY, lookZ) = slideCenterFor(pos)
    val (upX, upY, upZ) = computeUp(pos)

    // Apply debug zoom via FOV
    backend.setCameraFov(_cameraRef, baseFov / zoomScale)
    backend.setCameraPosition(_cameraRef, camX, camY, camZ)
    backend.setCameraUp(_cameraRef, upX, upY, upZ)
    backend.setCameraLookAt(_cameraRef, lookX, lookY, lookZ)
    backend.updateCameraProjection(_cameraRef)
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
      scheduler.cancelFrame(animationId)
      animating = false
    }
    initialized = true
    currentPosition = target
    applyCameraPose(target)
    backend.render(_cameraRef)
  }

  /** Returns `true` if a fly-through animation is currently in progress. */
  def isAnimating: Boolean = animating

  /** Cleans up resources held by this animator.
    *
    * Cancels any running animation frame callback and removes the keyboard event listener for zoom.
    */
  def cleanup(): Unit = {
    if (animating) {
      scheduler.cancelFrame(animationId)
      animating = false
    }
    if (enableZoomListener) {
      dom.document.removeEventListener("keydown", zoomKeyListener)
    }
  }

  // --- Helpers (delegating to CameraMath for testability) ---

  private def slideCenterFor(pos: SlidePosition): (Double, Double, Double) =
    CameraMath.slideCenter(pos, slideWidth.toDouble, slideHeight.toDouble)

  private def cameraPositionFor(pos: SlidePosition): (Double, Double, Double) =
    CameraMath.cameraPositionFor(pos, slideWidth.toDouble, slideHeight.toDouble, cameraDistance)

  private def computeUp(pos: SlidePosition): (Double, Double, Double) =
    CameraMath.computeUp(pos)

  private def euclidean(a: SlidePosition, b: SlidePosition): Double =
    CameraMath.euclidean(a, b)

  private def transitionDuration(distance: Double): Double =
    CameraMath.transitionDuration(distance, config.cameraTransitionBaseMs, config.cameraTransitionFactor)

  private def easeInOut(t: Double): Double =
    CameraMath.easeInOut(t)

  private def lerp(a: Double, b: Double, t: Double): Double =
    CameraMath.lerp(a, b, t)
}
