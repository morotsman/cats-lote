package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.{SlidePosition, WebGLConfig}
import com.github.morotsman.lote.internal.interpreter.nconsole.camera.{CameraAnimator, CameraMath}
import munit.FunSuite

/** Unit tests for [[CameraAnimator]] using [[StubSceneBackend]] and [[ManualScheduler]].
  *
  * These tests verify the full animation loop: moveTo, jumpTo, convergence, zoom, and cleanup — all without requiring a
  * browser or Three.js.
  */
class CameraAnimatorSpec extends FunSuite {

  private val epsilon = 1e-6

  private def mkAnimator(
      vpWidth: Int = 800,
      vpHeight: Int = 600,
      config: WebGLConfig = WebGLConfig()
  ): (CameraAnimator, StubSceneBackend, ManualScheduler) = {
    val backend = new StubSceneBackend(vpWidth, vpHeight)
    val scheduler = new ManualScheduler
    val animator = new CameraAnimator(backend, config, scheduler, enableZoomListener = false)
    animator.initSpatialMode(vpWidth, vpHeight)
    (animator, backend, scheduler)
  }

  // ── moveTo: first call places camera immediately ──

  test("moveTo — first call places camera immediately (no animation)") {
    val (animator, backend, scheduler) = mkAnimator()
    val target = SlidePosition(0, 0, 0)

    animator.moveTo(target)

    assert(!animator.isAnimating, "should not be animating after first moveTo")
    assert(scheduler.isEmpty, "no rAF callbacks should be pending")
    // Camera should have been positioned — check that SetCameraPosition was called
    val posOps = backend.ops.collect { case op: SceneOp.SetCameraPosition => op }
    assert(posOps.nonEmpty, "camera position should be set")
    // Render should have been called
    val renderOps = backend.ops.collect { case op: SceneOp.Render => op }
    assert(renderOps.nonEmpty, "render should be called")
  }

  // ── moveTo: same position is a no-op ──

  test("moveTo — to same position is a no-op (no animation)") {
    val (animator, backend, scheduler) = mkAnimator()
    val target = SlidePosition(100, 200, 0)

    animator.moveTo(target) // first call — immediate placement
    backend.clearOps()

    animator.moveTo(target) // same position — should snap, no animation

    assert(!animator.isAnimating)
    assert(scheduler.isEmpty, "no rAF callbacks for zero-distance move")
  }

  // ── moveTo: triggers animateSpatial which schedules rAF callbacks ──

  test("moveTo — triggers rAF callbacks for distant target") {
    val (animator, backend, scheduler) = mkAnimator()
    val start = SlidePosition(0, 0, 0)
    val target = SlidePosition(1800, 0, 0)

    animator.moveTo(start) // first call — immediate
    backend.clearOps()

    animator.moveTo(target) // should trigger animation

    assert(animator.isAnimating, "should be animating")
    assert(scheduler.pendingCount > 0, "should have scheduled a frame callback")
  }

  // ── Animation converges to target position over simulated frames ──

  test("moveTo — animation converges to target position over simulated frames") {
    val (animator, backend, scheduler) = mkAnimator()
    val start = SlidePosition(0, 0, 0)
    val target = SlidePosition(1800, 0, 0)

    animator.moveTo(start)
    animator.moveTo(target)

    // Run the animation to completion by advancing time well past the duration
    // CameraMath.transitionDuration(1800, 800, 1.8) = 800 + 1.8 * sqrt(1800) ≈ 876ms
    var frames = 0
    while (animator.isAnimating && frames < 200) {
      scheduler.tickAt(16.0) // ~60fps
      frames += 1
    }

    assert(!animator.isAnimating, s"animation should have completed after $frames frames")

    // Verify the camera ended up at the target position
    val finalPos = backend.getCameraPosition(1) // camera ID is 1
    val expectedPos =
      CameraMath.cameraPositionFor(target, 800.0, 600.0, (600.0 / 2.0) / Math.tan(50.0 * Math.PI / 180.0 / 2.0))
    assertEqualsDouble(finalPos._1, expectedPos._1, 1.0)
    assertEqualsDouble(finalPos._2, expectedPos._2, 1.0)
    assertEqualsDouble(finalPos._3, expectedPos._3, 1.0)
  }

  // ── jumpTo cancels running animation and snaps immediately ──

  test("jumpTo — cancels running animation and snaps immediately") {
    val (animator, backend, scheduler) = mkAnimator()
    val start = SlidePosition(0, 0, 0)
    val animTarget = SlidePosition(1800, 0, 0)
    val jumpTarget = SlidePosition(3600, 0, 0)

    animator.moveTo(start)
    animator.moveTo(animTarget) // starts animation
    assert(animator.isAnimating)

    // Advance a few frames
    scheduler.tickAt(50.0)
    assert(animator.isAnimating, "still animating after 50ms")

    // Jump to a new position
    animator.jumpTo(jumpTarget)

    assert(!animator.isAnimating, "animation should be cancelled after jumpTo")
    assert(scheduler.cancelledFrameIds.nonEmpty, "should have cancelled a frame")

    // Verify camera is at the jump target
    val finalPos = backend.getCameraPosition(1)
    val expectedPos =
      CameraMath.cameraPositionFor(jumpTarget, 800.0, 600.0, (600.0 / 2.0) / Math.tan(50.0 * Math.PI / 180.0 / 2.0))
    assertEqualsDouble(finalPos._1, expectedPos._1, 1.0)
    assertEqualsDouble(finalPos._2, expectedPos._2, 1.0)
    assertEqualsDouble(finalPos._3, expectedPos._3, 1.0)
  }

  // ── isAnimating returns true during animation, false after completion ──

  test("isAnimating — returns true during animation, false after completion") {
    val (animator, _, scheduler) = mkAnimator()

    animator.moveTo(SlidePosition(0, 0, 0))
    assert(!animator.isAnimating, "not animating before second moveTo")

    animator.moveTo(SlidePosition(2000, 0, 0))
    assert(animator.isAnimating, "animating after distant moveTo")

    // Run to completion
    var safety = 0
    while (animator.isAnimating && safety < 300) {
      scheduler.tickAt(16.0)
      safety += 1
    }
    assert(!animator.isAnimating, "not animating after completion")
  }

  // ── Zoom in adjusts FOV correctly ──

  test("zoom in — adjusts FOV correctly") {
    val config = WebGLConfig(fieldOfView = 50.0, maxZoom = 20.0)
    val (animator, backend, _) = mkAnimator(config = config)

    animator.moveTo(SlidePosition(0, 0, 0))
    backend.clearOps()

    // Zoom in with "+"
    animator.handleZoomKey("+")

    // After zoom in, FOV should be baseFov / (1.0 * 1.2) = 50 / 1.2 ≈ 41.67
    val fovOps = backend.ops.collect { case SceneOp.SetCameraFov(_, fov) => fov }
    assert(fovOps.nonEmpty, "should have set camera FOV")
    val expectedFov = 50.0 / 1.2
    assertEqualsDouble(fovOps.last, expectedFov, 0.01)
  }

  // ── Zoom out adjusts FOV correctly ──

  test("zoom out — adjusts FOV correctly") {
    val config = WebGLConfig(fieldOfView = 50.0, maxFieldOfView = 160.0)
    val (animator, backend, _) = mkAnimator(config = config)

    animator.moveTo(SlidePosition(0, 0, 0))
    backend.clearOps()

    // Zoom out with "-"
    animator.handleZoomKey("-")

    // After zoom out, FOV should be baseFov / (1.0 / 1.2) = 50 * 1.2 = 60.0
    val fovOps = backend.ops.collect { case SceneOp.SetCameraFov(_, fov) => fov }
    assert(fovOps.nonEmpty, "should have set camera FOV")
    val expectedFov = 50.0 / (1.0 / 1.2)
    assertEqualsDouble(fovOps.last, expectedFov, 0.01)
  }

  // ── Zoom reset restores base FOV ──

  test("zoom reset — '0' restores base FOV") {
    val config = WebGLConfig(fieldOfView = 50.0)
    val (animator, backend, _) = mkAnimator(config = config)

    animator.moveTo(SlidePosition(0, 0, 0))
    animator.handleZoomKey("+") // zoom in
    animator.handleZoomKey("+") // zoom in more
    backend.clearOps()

    animator.handleZoomKey("0") // reset

    val fovOps = backend.ops.collect { case SceneOp.SetCameraFov(_, fov) => fov }
    assert(fovOps.nonEmpty)
    // After reset, zoom scale is 1.0 so FOV = baseFov / 1.0 = 50.0
    assertEqualsDouble(fovOps.head, 50.0, 0.01)
  }

  // ── Zoom is clamped at maxZoom ──

  test("zoom in — clamped at maxZoom") {
    val config = WebGLConfig(fieldOfView = 50.0, maxZoom = 2.0)
    val (animator, backend, _) = mkAnimator(config = config)

    animator.moveTo(SlidePosition(0, 0, 0))

    // Zoom in many times
    for (_ <- 1 to 20) animator.handleZoomKey("+")

    backend.clearOps()
    animator.handleZoomKey("+") // one more — should be at cap

    val fovOps = backend.ops.collect { case SceneOp.SetCameraFov(_, fov) => fov }
    // FOV should not go below baseFov / maxZoom = 50.0 / 2.0 = 25.0
    fovOps.foreach { fov =>
      assert(fov >= 50.0 / 2.0 - 0.01, s"FOV $fov should not be below ${50.0 / 2.0}")
    }
  }

  // ── cleanup cancels pending animation and removes listener ──

  test("cleanup — cancels pending animation") {
    val (animator, _, scheduler) = mkAnimator()

    animator.moveTo(SlidePosition(0, 0, 0))
    animator.moveTo(SlidePosition(5000, 0, 0)) // start animation
    assert(animator.isAnimating)

    animator.cleanup()

    assert(!animator.isAnimating, "animation should be cancelled after cleanup")
    assert(scheduler.cancelledFrameIds.nonEmpty, "should have cancelled frame")
  }

  // ── moveTo cancels previous animation ──

  test("moveTo — cancels previous animation before starting new one") {
    val (animator, _, scheduler) = mkAnimator()

    animator.moveTo(SlidePosition(0, 0, 0))
    animator.moveTo(SlidePosition(1800, 0, 0)) // first animation
    assert(animator.isAnimating)

    scheduler.tickAt(16.0) // advance one frame
    val cancelledBefore = scheduler.cancelledFrameIds.length

    animator.moveTo(SlidePosition(3600, 0, 0)) // new target — cancels previous

    assert(scheduler.cancelledFrameIds.length > cancelledBefore, "should cancel previous animation frame")
    assert(animator.isAnimating, "new animation should be in progress")
  }

  // ── initSpatialMode creates camera with correct parameters ──

  test("initSpatialMode — creates camera with correct FOV and aspect") {
    val backend = new StubSceneBackend(1024, 768)
    val scheduler = new ManualScheduler
    val config = WebGLConfig(fieldOfView = 60.0, nearClip = 0.5, farClip = 50000.0)
    val animator = new CameraAnimator(backend, config, scheduler, enableZoomListener = false)

    animator.initSpatialMode(1024, 768)

    val cameraOps = backend.ops.collect { case op: SceneOp.CreateCamera => op }
    assertEquals(cameraOps.length, 1)
    assertEqualsDouble(cameraOps.head.fov, 60.0, epsilon)
    assertEqualsDouble(cameraOps.head.aspect, 1024.0 / 768.0, epsilon)
    assertEqualsDouble(cameraOps.head.near, 0.5, epsilon)
    assertEqualsDouble(cameraOps.head.far, 50000.0, epsilon)
  }

  // ── Animation intermediate frames show partial progress ──

  test("moveTo — intermediate frames show partial progress") {
    val (animator, backend, scheduler) = mkAnimator()
    val start = SlidePosition(0, 0, 0)
    val target = SlidePosition(1800, 0, 0)

    animator.moveTo(start)
    animator.moveTo(target)

    // Fire one frame at a small time offset
    scheduler.tickAt(50.0)

    // Camera should be between start and target (not at either endpoint yet)
    val pos = backend.getCameraPosition(1)
    val startCamPos =
      CameraMath.cameraPositionFor(start, 800.0, 600.0, (600.0 / 2.0) / Math.tan(50.0 * Math.PI / 180.0 / 2.0))
    val targetCamPos =
      CameraMath.cameraPositionFor(target, 800.0, 600.0, (600.0 / 2.0) / Math.tan(50.0 * Math.PI / 180.0 / 2.0))

    // X should be between start and target X
    assert(pos._1 > startCamPos._1 + 1.0, s"camera X ${pos._1} should have moved from start ${startCamPos._1}")
    assert(pos._1 < targetCamPos._1 - 1.0, s"camera X ${pos._1} should not have reached target ${targetCamPos._1}")
  }
}
