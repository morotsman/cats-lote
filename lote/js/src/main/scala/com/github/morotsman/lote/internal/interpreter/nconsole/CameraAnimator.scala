package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.SlidePosition
import org.scalajs.dom

/** Animates the Three.js camera between slide positions in spatial mode.
  *
  * In spatial mode a **perspective camera** is used so that z-depth is visually meaningful:
  * slides closer to the camera appear larger, and the camera fly-through shows real parallax.
  * The camera is positioned along the slide's surface normal at a computed distance so
  * the slide fills the viewport, then animated with ease-in-out between poses.
  *
  * Non-spatial mode keeps the original orthographic camera with single-plane effects.
  */
private[nconsole] class CameraAnimator(scene: WebGLScene) {

  private val Deg2Rad = Math.PI / 180.0

  private var animationId: Int = 0
  private var animating: Boolean = false

  // Current camera target position (where it's settled or heading)
  private var currentPosition: SlidePosition = SlidePosition(0, 0, 0)
  private var initialized: Boolean = false

  // Spatial mode dimensions
  private var slideWidth: Int = 0
  private var slideHeight: Int = 0
  private var spatial: Boolean = false

  // Perspective camera for spatial mode
  private var perspCamera: ThreePerspectiveCamera = _
  private var baseFov: Double = 50.0
  private var cameraDistance: Double = 2000.0

  /** Exposes the perspective camera reference for Scene3DRef. */
  private[nconsole] def perspCameraRef: ThreePerspectiveCamera = perspCamera

  private val meshDyn = scene.plane.asInstanceOf[scalajs.js.Dynamic]

  // Debug zoom
  private var debugScale: Double = 1.0
  private val debugKeyListener: scalajs.js.Function1[dom.KeyboardEvent, Unit] = { (ev: dom.KeyboardEvent) =>
    ev.key match {
      case "+" | "=" =>
        debugScale = (debugScale * 1.2).min(20.0)
        applyDebugZoom()
      case "-" | "_" =>
        // Clamp so perspective FOV stays below 160° (baseFov / minScale < 160)
        val minScale = if (spatial) (baseFov / 160.0).max(0.02) else 0.02
        debugScale = (debugScale / 1.2).max(minScale)
        applyDebugZoom()
      case "0" =>
        debugScale = 1.0
        if (spatial) { applyCameraPose(currentPosition); scene.render() }
        else resetViewNonSpatial()
        scene.render()
      case _ =>
    }
  }
  dom.document.addEventListener("keydown", debugKeyListener)

  private def applyDebugZoom(): Unit = {
    if (spatial) {
      // Zoom by adjusting the perspective camera's field of view
      perspCamera.fov = baseFov / debugScale
      perspCamera.updateProjectionMatrix()
    } else {
      meshDyn.scale.set(debugScale, debugScale, 1.0)
      meshDyn.position.set(scene.centerX, scene.centerY, 0)
    }
    scene.render()
  }

  /** Called when spatial mode is initialized. */
  def initSpatialMode(vpWidth: Int, vpHeight: Int): Unit = {
    spatial = true
    slideWidth = vpWidth
    slideHeight = vpHeight

    // Compute the camera distance that frames a slide exactly at the base FOV
    baseFov = 50.0
    cameraDistance = (slideHeight / 2.0) / Math.tan(baseFov * Deg2Rad / 2.0)

    val aspect = slideWidth.toDouble / slideHeight.toDouble
    perspCamera = new ThreePerspectiveCamera(baseFov, aspect, 1.0, 100000.0)

    // Switch the scene to use the perspective camera
    scene.activeCamera = perspCamera
  }

  def moveTo(target: SlidePosition): Unit = {
    if (!initialized) {
      initialized = true
      currentPosition = target
      if (spatial) {
        applyCameraPose(target)
        scene.render()
      }
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
      if (spatial) { applyCameraPose(target); scene.render() }
      return
    }

    if (spatial) {
      animateSpatial(startPos, target, distance)
    } else {
      animateNonSpatial(startPos, target, distance)
    }
  }

  // ---- Spatial mode animation: perspective camera flying through 3D space ----

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
        debugScale = 1.0
      }
    }

    animationId = dom.window.requestAnimationFrame(animate _)
  }

  // ---- Non-spatial mode animation (single plane, arrival effect) ----

  private def animateNonSpatial(startPos: SlidePosition, target: SlidePosition, distance: Double): Unit = {
    val durationMs = transitionDuration(distance)
    val startTime = dom.window.performance.now()

    val dx = target.x - startPos.x
    val dy = target.y - startPos.y
    val dz = target.z - startPos.z
    val xyMag = Math.sqrt(dx * dx + dy * dy).max(1.0)
    val dirX = dx / xyMag
    val dirY = dy / xyMag

    val zFactor = (Math.abs(dz) / 300.0).min(1.0)
    val distFactor = (distance / 1500.0).min(1.0)
    val startScale = 0.15 + 0.35 * (1.0 - Math.max(zFactor, distFactor))
    val startOffsetX = dirX * scene.viewportWidth * 0.5
    val startOffsetY = -dirY * scene.viewportHeight * 0.5

    meshDyn.scale.set(startScale, startScale, 1.0)
    meshDyn.position.set(scene.centerX + startOffsetX, scene.centerY + startOffsetY, 0)
    scene.render()

    animating = true

    def animate(now: Double): Unit = {
      val elapsed = now - startTime
      val t = (elapsed / durationMs).min(1.0)
      val eased = easeOutCubic(t)

      val scale = startScale + (1.0 - startScale) * eased
      val offsetX = startOffsetX * (1.0 - eased)
      val offsetY = startOffsetY * (1.0 - eased)

      meshDyn.scale.set(scale, scale, 1.0)
      meshDyn.position.set(scene.centerX + offsetX, scene.centerY + offsetY, 0)
      scene.render()

      if (t < 1.0) {
        animationId = dom.window.requestAnimationFrame(animate _)
      } else {
        animating = false
        currentPosition = target
        resetViewNonSpatial()
        scene.render()
      }
    }

    animationId = dom.window.requestAnimationFrame(animate _)
  }

  /** Position the perspective camera to frame the given slide, facing it along its surface normal. */
  private def applyCameraPose(pos: SlidePosition): Unit = {
    val (camX, camY, camZ) = cameraPositionFor(pos)
    val (lookX, lookY, lookZ) = slideCenterFor(pos)
    val (upX, upY, upZ) = computeUp(pos)

    // Apply debug zoom via FOV
    perspCamera.fov = baseFov / debugScale
    perspCamera.position.set(camX, camY, camZ)
    perspCamera.up.set(upX, upY, upZ)
    perspCamera.lookAt(lookX, lookY, lookZ)
    perspCamera.updateProjectionMatrix()
  }

  def jumpTo(target: SlidePosition): Unit = {
    if (animating) {
      dom.window.cancelAnimationFrame(animationId)
      animating = false
    }
    initialized = true
    currentPosition = target
    if (spatial) {
      applyCameraPose(target)
      scene.render()
    } else {
      resetViewNonSpatial()
    }
  }

  def isAnimating: Boolean = animating

  def cleanup(): Unit = {
    if (animating) {
      dom.window.cancelAnimationFrame(animationId)
      animating = false
    }
    dom.document.removeEventListener("keydown", debugKeyListener)
  }

  // --- Helpers ---

  /** Compute the slide center in world space. */
  private def slideCenterFor(pos: SlidePosition): (Double, Double, Double) =
    (pos.x + slideWidth / 2.0, pos.y + slideHeight / 2.0, pos.z)

  /** Compute the camera position: slide center offset along the surface normal by cameraDistance. */
  private def cameraPositionFor(pos: SlidePosition): (Double, Double, Double) = {
    val (cx, cy, cz) = slideCenterFor(pos)
    val (nx, ny, nz) = computeNormal(pos)
    (cx + nx * cameraDistance, cy + ny * cameraDistance, cz + nz * cameraDistance)
  }

  /** Compute the outward-facing normal of a slide with the given rotation (Euler XYZ).
    *
    * Three.js Euler order 'XYZ' produces the rotation matrix R = Rx * Ry * Rz.
    * The normal is the third column of this matrix (transforms local Z → world).
    */
  private def computeNormal(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    // Third column of Rx * Ry * Rz: (sin(ry), -sin(rx)*cos(ry), cos(rx)*cos(ry))
    (s2, -s1 * c2, c1 * c2)
  }

  /** Compute the up vector of a slide with the given rotation (Euler XYZ).
    *
    * Three.js Euler order 'XYZ' produces the rotation matrix R = Rx * Ry * Rz.
    * The up vector is the second column of this matrix (transforms local Y → world).
    */
  private def computeUp(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val rz = pos.rotZ * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    val c3 = Math.cos(rz); val s3 = Math.sin(rz)
    // Second column of Rx * Ry * Rz: (-cos(ry)*sin(rz), cos(rx)*cos(rz)-sin(rx)*sin(ry)*sin(rz), sin(rx)*cos(rz)+cos(rx)*sin(ry)*sin(rz))
    (-c2 * s3, c1 * c3 - s1 * s2 * s3, s1 * c3 + c1 * s2 * s3)
  }

  private def resetViewNonSpatial(): Unit = {
    meshDyn.scale.set(1.0, 1.0, 1.0)
    meshDyn.position.set(scene.centerX, scene.centerY, 0)
    scene.camera.left = 0
    scene.camera.right = scene.viewportWidth
    scene.camera.top = scene.viewportHeight
    scene.camera.bottom = 0
    scene.camera.updateProjectionMatrix()
  }

  private def euclidean(a: SlidePosition, b: SlidePosition): Double = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    val dz = b.z - a.z
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  private def transitionDuration(distance: Double): Double = {
    val baseMs = 800.0
    val kFactor = 1.8
    baseMs + kFactor * Math.sqrt(distance)
  }

  private def easeInOut(t: Double): Double = {
    val c = t.max(0.0).min(1.0)
    c * c * (3.0 - 2.0 * c)
  }

  private def easeOutCubic(t: Double): Double = {
    val c = 1.0 - t.max(0.0).min(1.0)
    1.0 - c * c * c
  }

  private def lerp(a: Double, b: Double, t: Double): Double =
    a + (b - a) * t
}

