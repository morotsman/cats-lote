package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.{SlidePosition, WebGLConfig}
import munit.FunSuite

/** Unit tests for [[SpatialState]] — init, layer creation, activation, and dispose lifecycle using
  * [[StubSceneBackend]], [[StubCanvasFactory]], and [[ManualScheduler]].
  *
  * These tests verify the full spatial mode initialization pipeline without DOM or Three.js.
  */
class SpatialStateSpec extends FunSuite {

  private val epsilon = 1e-9

  private def mkState(
      vpWidth: Int = 800,
      vpHeight: Int = 600,
      cellWidth: Int = 10,
      cellHeight: Int = 20,
      dpr: Double = 1.0
  ): (SpatialState, StubSceneBackend, StubCanvasFactory, CameraAnimator) = {
    val backend = new StubSceneBackend(vpWidth, vpHeight)
    val canvasFactory = new StubCanvasFactory(dpr)
    val scheduler = new ManualScheduler
    val animator = new CameraAnimator(backend, WebGLConfig(), scheduler, enableZoomListener = false)
    val state = new SpatialState(
      glScene = null, // not needed when backend is provided
      backend = backend,
      cameraAnimator = animator,
      cellWidth = cellWidth,
      cellHeight = cellHeight,
      canvasFactory = canvasFactory
    )
    (state, backend, canvasFactory, animator)
  }

  // ── init: basic layer creation ──

  test("init creates one SlideLayer per unique position") {
    val (state, backend, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0)),
        Some(SlidePosition(1600, 0, 0))
      )
    )
    assertEquals(state.slideLayers.length, 3)
  }

  test("init deduplicates layers sharing the same position") {
    val (state, _, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(0, 0, 0)), // same position
        Some(SlidePosition(800, 0, 0))
      )
    )
    assertEquals(state.slideLayers.length, 2) // only 2 unique positions
  }

  test("init adds each layer mesh to the scene") {
    val (state, backend, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0))
      )
    )
    val addOps = backend.ops.collect { case op: SceneOp.AddToScene => op }
    assertEquals(addOps.length, 2)
  }

  test("init creates meshes with correct viewport dimensions") {
    val (state, backend, _, _) = mkState(vpWidth = 1024, vpHeight = 768)
    state.init(Vector(Some(SlidePosition(0, 0, 0))))
    val createOps = backend.ops.collect { case op: SceneOp.CreatePlaneMesh => op }
    assertEquals(createOps.head, SceneOp.CreatePlaneMesh(1024.0, 768.0))
  }

  test("init positions mesh at world offset + half viewport") {
    val (state, backend, _, _) = mkState(vpWidth = 800, vpHeight = 600)
    state.init(Vector(Some(SlidePosition(100, 200, 50))))
    val posOps = backend.ops.collect { case op: SceneOp.SetMeshPosition => op }
    assertEqualsDouble(posOps.head.x, 500.0, epsilon) // 100 + 400
    assertEqualsDouble(posOps.head.y, 500.0, epsilon) // 200 + 300
    assertEqualsDouble(posOps.head.z, 50.0, epsilon)
  }

  test("init applies rotation from SlidePosition") {
    val (state, backend, _, _) = mkState()
    state.init(Vector(Some(SlidePosition(0, 0, 0, rotX = 90, rotY = 45, rotZ = 0))))
    val rotOps = backend.ops.collect { case op: SceneOp.SetMeshRotation => op }
    assertEqualsDouble(rotOps.head.rx, Math.PI / 2.0, epsilon)
    assertEqualsDouble(rotOps.head.ry, Math.PI / 4.0, epsilon)
  }

  test("init with transparentBackground sets depthWrite false") {
    val (state, backend, _, _) = mkState()
    state.init(Vector(Some(SlidePosition(0, 0, 0, transparentBackground = true))))
    val dwOps = backend.ops.collect { case op: SceneOp.SetMeshDepthWrite => op }
    assertEquals(dwOps.length, 1)
    assertEquals(dwOps.head.depthWrite, false)
  }

  // ── init: canvas creation ──

  test("init creates offscreen canvases for each layer") {
    val (state, _, canvasFactory, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0))
      )
    )
    val createOps = canvasFactory.ops.collect { case op: CanvasOp.CreateCanvas => op }
    assertEquals(createOps.length, 2)
  }

  test("init canvas dimensions scaled by DPR") {
    val (state, _, canvasFactory, _) =
      mkState(vpWidth = 800, vpHeight = 600, cellWidth = 10, cellHeight = 20, dpr = 2.0)
    state.init(Vector(Some(SlidePosition(0, 0, 0))))
    val createOps = canvasFactory.ops.collect { case op: CanvasOp.CreateCanvas => op }
    assertEquals(createOps.head.width, 1600) // 80 * 10 * 2
    assertEquals(createOps.head.height, 1200) // 30 * 20 * 2
  }

  // ── init: camera initialization ──

  test("init calls initSpatialMode on CameraAnimator") {
    val (state, backend, _, _) = mkState()
    state.init(Vector(Some(SlidePosition(0, 0, 0))))
    // CameraAnimator.initSpatialMode creates a camera
    val camOps = backend.ops.collect { case op: SceneOp.CreateCamera => op }
    assertEquals(camOps.length, 1)
  }

  // ── activateLayer ──

  test("activateLayer selects the correct layer") {
    val (state, _, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0))
      )
    )
    state.activateLayer(1)
    assert(state.activeLayer.isDefined)
    assertEquals(state.activeLayer.get.index, 1)
  }

  test("activateLayer with deduplicated slides maps to same layer") {
    val (state, _, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(0, 0, 0)), // same as first
        Some(SlidePosition(800, 0, 0))
      )
    )
    state.activateLayer(0)
    val layer0 = state.activeLayer
    state.activateLayer(1)
    val layer1 = state.activeLayer
    // Slides 0 and 1 share the same layer
    assertEquals(layer0.get.index, layer1.get.index)
  }

  test("activateLayer with no glScene leaves sharedSceneRef as None") {
    val (state, backend, _, _) = mkState(vpWidth = 800, vpHeight = 600)
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0))
      )
    )
    state.activateLayer(1)
    // When glScene is null (test mode), Scene3DRef is not created
    assertEquals(state.sharedSceneRef, None)
  }

  // ── activeLayer before init ──

  test("activeLayer is None before init") {
    val (state, _, _, _) = mkState()
    assertEquals(state.activeLayer, None)
  }

  test("slideLayers is empty before init") {
    val (state, _, _, _) = mkState()
    assertEquals(state.slideLayers, Vector.empty)
  }

  // ── dispose ──

  test("dispose calls disposeMesh on all layers") {
    val (state, backend, _, _) = mkState()
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        Some(SlidePosition(800, 0, 0)),
        Some(SlidePosition(1600, 0, 0))
      )
    )
    backend.clearOps()
    state.dispose()
    val disposeOps = backend.ops.collect { case op: SceneOp.DisposeMesh => op }
    assertEquals(disposeOps.length, 3)
  }

  // ── Position resolution (inherited positions) ──

  test("init resolves None positions by inheriting from previous slide") {
    val (state, backend, _, _) = mkState(vpWidth = 800, vpHeight = 600)
    state.init(
      Vector(
        Some(SlidePosition(0, 0, 0)),
        None, // inherits (0, 0, 0)
        Some(SlidePosition(800, 0, 0))
      )
    )
    // First two slides share position, so 2 unique layers
    assertEquals(state.slideLayers.length, 2)
  }
}
