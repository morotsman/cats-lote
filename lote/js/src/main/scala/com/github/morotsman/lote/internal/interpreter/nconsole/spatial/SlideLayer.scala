package com.github.morotsman.lote.internal.interpreter.nconsole.spatial

import com.github.morotsman.lote.internal.interpreter.nconsole.canvas.{CanvasFactory, DomCanvasFactory}
import com.github.morotsman.lote.internal.interpreter.nconsole.facade._
import com.github.morotsman.lote.internal.interpreter.nconsole.scene.SceneBackend

/** A single slide's rendering surface in spatial mode.
  *
  * Each `SlideLayer` owns an offscreen canvas (for ANSI text rendering) and a mesh positioned at the slide's
  * world-space coordinates. Mesh creation and canvas creation are delegated to the provided abstractions, allowing full
  * unit testing without DOM or Three.js.
  *
  * @param canvasFactory
  *   abstraction over DOM canvas creation; in production uses [[DomCanvasFactory]], in tests [[StubCanvasFactory]]
  * @param backend
  *   abstraction over the Three.js scene graph; in production uses [[WebGLSceneBackend]], in tests [[StubSceneBackend]]
  */
private[nconsole] class SlideLayer(
    val index: Int,
    worldX: Double,
    worldY: Double,
    worldZ: Double,
    rotXDeg: Double,
    rotYDeg: Double,
    rotZDeg: Double,
    viewportWidth: Int,
    viewportHeight: Int,
    cellWidth: Int,
    cellHeight: Int,
    val transparentBg: Boolean = false,
    canvasFactory: CanvasFactory = DomCanvasFactory,
    backend: SceneBackend = null
) {

  val cols: Int = viewportWidth / cellWidth
  val rows: Int = viewportHeight / cellHeight

  // Offscreen canvas for text rendering — scaled by devicePixelRatio for crisp
  // rendering on HiDPI displays.
  private val dpr: Double = canvasFactory.devicePixelRatio.max(1.0)

  private val canvasWidth: Int = (cols * cellWidth * dpr).toInt
  private val canvasHeight: Int = (rows * cellHeight * dpr).toInt

  /** The offscreen canvas reference. Type depends on the CanvasFactory in use: `HTMLCanvasElement` in production, `Int`
    * in test stubs.
    */
  private val _offscreenRef: Any = canvasFactory.createCanvas(canvasWidth, canvasHeight)

  /** The 2D rendering context reference. */
  private val _ctxRef: Any = {
    val c = canvasFactory.getContext2D(_offscreenRef.asInstanceOf[canvasFactory.CanvasRef])
    // Apply DPR scaling and initial background via the factory abstraction
    canvasFactory.initContext(c, dpr, cols * cellWidth, rows * cellHeight, transparentBg)
    c
  }

  // ---- Backward-compatible accessors for production code that still needs DOM types ----
  // These casts are safe in production (where CanvasRef = HTMLCanvasElement).

  /** Returns the offscreen canvas as an HTMLCanvasElement. Only safe in production (DomCanvasFactory). */
  def offscreen: org.scalajs.dom.HTMLCanvasElement =
    _offscreenRef.asInstanceOf[org.scalajs.dom.HTMLCanvasElement]

  /** Returns the 2D context as CanvasRenderingContext2D. Only safe in production (DomCanvasFactory). */
  def ctx: org.scalajs.dom.CanvasRenderingContext2D =
    _ctxRef.asInstanceOf[org.scalajs.dom.CanvasRenderingContext2D]

  // ---- Mesh creation via SceneBackend ----

  /** The computed mesh X position (world coordinates). */
  val meshX: Double = worldX + viewportWidth / 2.0

  /** The computed mesh Y position (world coordinates). */
  val meshY: Double = worldY + viewportHeight / 2.0

  /** The computed mesh Z position (world coordinates). */
  val meshZ: Double = worldZ

  /** The computed mesh rotation in radians. */
  private val Deg2Rad = Math.PI / 180.0
  val meshRotX: Double = rotXDeg * Deg2Rad
  val meshRotY: Double = rotYDeg * Deg2Rad
  val meshRotZ: Double = rotZDeg * Deg2Rad

  /** The mesh reference. When `backend` is provided, this is a `backend.MeshRef` created through the abstraction. When
    * `backend` is null (legacy usage), falls back to direct Three.js construction.
    */
  val meshRef: Any = if (backend != null) {
    val m = backend.createPlaneMesh(viewportWidth, viewportHeight, _offscreenRef)
    backend.setMeshPosition(m, meshX, meshY, meshZ)
    backend.setMeshRotation(m, meshRotX, meshRotY, meshRotZ)
    if (transparentBg) {
      backend.setMeshDepthWrite(m, false)
    }
    m
  } else {
    // Legacy path: direct Three.js construction (for backward compat during migration)
    val texture = new ThreeCanvasTexture(offscreen)
    texture.minFilter = ThreeLinearFilter
    texture.magFilter = ThreeNearestFilter
    val geo = new ThreePlaneGeometry(viewportWidth, viewportHeight)
    val mat = new ThreeMeshBasicMaterial(
      ThreeMaterialOptions(map = texture, transparent = true, side = 2)
    )
    if (transparentBg) {
      mat.depthWrite = false
    }
    val m = new ThreeMesh(geo, mat)
    m.position.set(meshX, meshY, meshZ)
    m.rotation.set(meshRotX, meshRotY, meshRotZ)
    m
  }

  /** Returns the mesh as a ThreeMesh (only safe in production). */
  def mesh: ThreeMesh = meshRef.asInstanceOf[ThreeMesh]

  /** Returns the mesh's texture (only safe in production, where mesh is a ThreeMesh). */
  def texture: ThreeCanvasTexture = mesh.material.map.asInstanceOf[ThreeCanvasTexture]

  // Frame state for dirty tracking
  var previousFrame: Vector[String] = Vector.empty

  // Sub-pixel canvas drawing offset (in CSS pixels). Applied as ctx.translate() before rendering rows.
  // This shifts the character grid content within the texture without moving the mesh.
  var canvasOffsetX: Double = 0.0
  var canvasOffsetY: Double = 0.0

  // Rows that should NOT be affected by canvas offset (e.g. timer, progress bar).
  // When a canvas offset is active, these rows are re-rendered in a second pass without the translate.
  var fixedRows: Set[Int] = Set.empty

  def dispose(): Unit = {
    if (backend != null) {
      backend.disposeMesh(meshRef.asInstanceOf[backend.MeshRef])
    } else {
      val m = mesh
      m.material.map.asInstanceOf[ThreeCanvasTexture].dispose()
      m.material.dispose()
      m.geometry.dispose()
    }
  }
}
