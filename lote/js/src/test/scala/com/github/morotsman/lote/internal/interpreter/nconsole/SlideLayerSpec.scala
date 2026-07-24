package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.internal.interpreter.nconsole.spatial.SlideLayer
import munit.FunSuite

/** Unit tests for [[SlideLayer]] — full construction, mesh lifecycle, and dispose behavior using [[StubSceneBackend]]
  * and [[StubCanvasFactory]].
  *
  * These tests verify that SlideLayer correctly delegates to SceneBackend for mesh creation, positioning, rotation, and
  * disposal — without requiring DOM or Three.js.
  */
class SlideLayerSpec extends FunSuite {

  private val epsilon = 1e-9

  private def makeLayer(
      index: Int = 0,
      worldX: Double = 0,
      worldY: Double = 0,
      worldZ: Double = 0,
      rotXDeg: Double = 0,
      rotYDeg: Double = 0,
      rotZDeg: Double = 0,
      viewportWidth: Int = 800,
      viewportHeight: Int = 600,
      cellWidth: Int = 10,
      cellHeight: Int = 20,
      transparentBg: Boolean = false,
      dpr: Double = 1.0
  ): (SlideLayer, StubSceneBackend, StubCanvasFactory) = {
    val backend = new StubSceneBackend(viewportWidth, viewportHeight)
    val canvas = new StubCanvasFactory(dpr)
    val layer = new SlideLayer(
      index = index,
      worldX = worldX,
      worldY = worldY,
      worldZ = worldZ,
      rotXDeg = rotXDeg,
      rotYDeg = rotYDeg,
      rotZDeg = rotZDeg,
      viewportWidth = viewportWidth,
      viewportHeight = viewportHeight,
      cellWidth = cellWidth,
      cellHeight = cellHeight,
      transparentBg = transparentBg,
      canvasFactory = canvas,
      backend = backend
    )
    (layer, backend, canvas)
  }

  // ── Construction: mesh creation ──

  test("construction creates a plane mesh with correct dimensions") {
    val (_, backend, _) = makeLayer(viewportWidth = 800, viewportHeight = 600)
    val createOps = backend.ops.collect { case op: SceneOp.CreatePlaneMesh => op }
    assertEquals(createOps.length, 1)
    assertEquals(createOps.head, SceneOp.CreatePlaneMesh(800.0, 600.0))
  }

  test("construction sets mesh position at world offset + half viewport") {
    val (layer, backend, _) = makeLayer(worldX = 100, worldY = 200, worldZ = 50)
    val posOps = backend.ops.collect { case op: SceneOp.SetMeshPosition => op }
    assertEquals(posOps.length, 1)
    assertEqualsDouble(posOps.head.x, 500.0, epsilon) // 100 + 800/2
    assertEqualsDouble(posOps.head.y, 500.0, epsilon) // 200 + 600/2
    assertEqualsDouble(posOps.head.z, 50.0, epsilon)
  }

  test("construction sets mesh rotation converting degrees to radians") {
    val (_, backend, _) = makeLayer(rotXDeg = 90, rotYDeg = 45, rotZDeg = 180)
    val rotOps = backend.ops.collect { case op: SceneOp.SetMeshRotation => op }
    assertEquals(rotOps.length, 1)
    assertEqualsDouble(rotOps.head.rx, Math.PI / 2.0, epsilon)
    assertEqualsDouble(rotOps.head.ry, Math.PI / 4.0, epsilon)
    assertEqualsDouble(rotOps.head.rz, Math.PI, epsilon)
  }

  test("construction with zero rotation sets zero radians") {
    val (_, backend, _) = makeLayer(rotXDeg = 0, rotYDeg = 0, rotZDeg = 0)
    val rotOps = backend.ops.collect { case op: SceneOp.SetMeshRotation => op }
    assertEqualsDouble(rotOps.head.rx, 0.0, epsilon)
    assertEqualsDouble(rotOps.head.ry, 0.0, epsilon)
    assertEqualsDouble(rotOps.head.rz, 0.0, epsilon)
  }

  // ── Construction: transparent background ──

  test("transparent background disables depth write on the mesh") {
    val (_, backend, _) = makeLayer(transparentBg = true)
    val dwOps = backend.ops.collect { case op: SceneOp.SetMeshDepthWrite => op }
    assertEquals(dwOps.length, 1)
    assertEquals(dwOps.head.depthWrite, false)
  }

  test("non-transparent background does NOT disable depth write") {
    val (_, backend, _) = makeLayer(transparentBg = false)
    val dwOps = backend.ops.collect { case op: SceneOp.SetMeshDepthWrite => op }
    assertEquals(dwOps.length, 0)
  }

  // ── Construction: canvas creation ──

  test("construction creates offscreen canvas with correct physical dimensions (1x DPR)") {
    val (_, _, canvas) =
      makeLayer(viewportWidth = 800, viewportHeight = 600, cellWidth = 10, cellHeight = 20, dpr = 1.0)
    val createOps = canvas.ops.collect { case op: CanvasOp.CreateCanvas => op }
    assertEquals(createOps.length, 1)
    assertEquals(createOps.head.width, 800) // 80 cols * 10 * 1.0
    assertEquals(createOps.head.height, 600) // 30 rows * 20 * 1.0
  }

  test("construction creates offscreen canvas scaled by DPR (2x)") {
    val (_, _, canvas) =
      makeLayer(viewportWidth = 800, viewportHeight = 600, cellWidth = 10, cellHeight = 20, dpr = 2.0)
    val createOps = canvas.ops.collect { case op: CanvasOp.CreateCanvas => op }
    assertEquals(createOps.head.width, 1600) // 80 * 10 * 2.0
    assertEquals(createOps.head.height, 1200) // 30 * 20 * 2.0
  }

  test("construction obtains 2D context from canvas") {
    val (_, _, canvas) = makeLayer()
    val ctxOps = canvas.ops.collect { case op: CanvasOp.GetContext2D => op }
    assertEquals(ctxOps.length, 1)
  }

  // ── Grid dimensions ──

  test("cols computed correctly from viewport and cell width") {
    val (layer, _, _) = makeLayer(viewportWidth = 800, cellWidth = 10)
    assertEquals(layer.cols, 80)
  }

  test("rows computed correctly from viewport and cell height") {
    val (layer, _, _) = makeLayer(viewportHeight = 600, cellHeight = 20)
    assertEquals(layer.rows, 30)
  }

  // ── Computed mesh position fields ──

  test("meshX/meshY/meshZ reflect computed position") {
    val (layer, _, _) = makeLayer(worldX = 200, worldY = 100, worldZ = -50)
    assertEqualsDouble(layer.meshX, 600.0, epsilon) // 200 + 400
    assertEqualsDouble(layer.meshY, 400.0, epsilon) // 100 + 300
    assertEqualsDouble(layer.meshZ, -50.0, epsilon)
  }

  test("meshRotX/Y/Z reflect computed rotation in radians") {
    val (layer, _, _) = makeLayer(rotXDeg = 45, rotYDeg = -90, rotZDeg = 360)
    assertEqualsDouble(layer.meshRotX, Math.PI / 4.0, epsilon)
    assertEqualsDouble(layer.meshRotY, -Math.PI / 2.0, epsilon)
    assertEqualsDouble(layer.meshRotZ, 2.0 * Math.PI, epsilon)
  }

  // ── Dispose ──

  test("dispose calls backend.disposeMesh with the mesh reference") {
    val (layer, backend, _) = makeLayer()
    backend.clearOps()
    layer.dispose()
    val disposeOps = backend.ops.collect { case op: SceneOp.DisposeMesh => op }
    assertEquals(disposeOps.length, 1)
  }

  test("dispose targets the correct mesh ID") {
    val (layer, backend, _) = makeLayer()
    val meshId = layer.meshRef.asInstanceOf[Int]
    backend.clearOps()
    layer.dispose()
    val disposeOps = backend.ops.collect { case op: SceneOp.DisposeMesh => op }
    assertEquals(disposeOps.head.meshId, meshId)
  }

  // ── Multiple layers ──

  test("multiple layers create distinct mesh IDs") {
    val backend = new StubSceneBackend(800, 600)
    val canvas = new StubCanvasFactory(1.0)
    val layer1 = new SlideLayer(0, 0, 0, 0, 0, 0, 0, 800, 600, 10, 20, canvasFactory = canvas, backend = backend)
    val layer2 = new SlideLayer(1, 800, 0, 0, 0, 0, 0, 800, 600, 10, 20, canvasFactory = canvas, backend = backend)
    assert(layer1.meshRef != layer2.meshRef)
  }

  // ── Operation ordering ──

  test("construction operations are in correct order: create → position → rotation") {
    val (_, backend, _) = makeLayer(rotXDeg = 45)
    val relevantOps = backend.ops.filter {
      case _: SceneOp.CreatePlaneMesh => true
      case _: SceneOp.SetMeshPosition => true
      case _: SceneOp.SetMeshRotation => true
      case _                          => false
    }
    assert(relevantOps.head.isInstanceOf[SceneOp.CreatePlaneMesh])
    assert(relevantOps(1).isInstanceOf[SceneOp.SetMeshPosition])
    assert(relevantOps(2).isInstanceOf[SceneOp.SetMeshRotation])
  }

  test("transparent bg: depthWrite is set after rotation") {
    val (_, backend, _) = makeLayer(transparentBg = true, rotXDeg = 10)
    val relevantOps = backend.ops.filter {
      case _: SceneOp.SetMeshRotation   => true
      case _: SceneOp.SetMeshDepthWrite => true
      case _                            => false
    }
    assert(relevantOps.head.isInstanceOf[SceneOp.SetMeshRotation])
    assert(relevantOps(1).isInstanceOf[SceneOp.SetMeshDepthWrite])
  }

  // ── Frame state ──

  test("previousFrame is initially empty") {
    val (layer, _, _) = makeLayer()
    assertEquals(layer.previousFrame, Vector.empty[String])
  }

  test("canvasOffsetX/Y default to 0.0") {
    val (layer, _, _) = makeLayer()
    assertEqualsDouble(layer.canvasOffsetX, 0.0, epsilon)
    assertEqualsDouble(layer.canvasOffsetY, 0.0, epsilon)
  }

  test("fixedRows is initially empty") {
    val (layer, _, _) = makeLayer()
    assertEquals(layer.fixedRows, Set.empty[Int])
  }

  // ── Index ──

  test("index is preserved") {
    val (layer, _, _) = makeLayer(index = 7)
    assertEquals(layer.index, 7)
  }

  test("transparentBg flag is preserved") {
    val (layer, _, _) = makeLayer(transparentBg = true)
    assertEquals(layer.transparentBg, true)
  }
}
