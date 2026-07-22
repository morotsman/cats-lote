package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}

/** A single slide's rendering surface in spatial mode.
  *
  * Each `SlideLayer` owns an offscreen canvas (for ANSI text rendering), a Three.js texture, and a mesh positioned at
  * the slide's world-space coordinates.
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
    val transparentBg: Boolean = false
) {

  val cols: Int = viewportWidth / cellWidth
  val rows: Int = viewportHeight / cellHeight

  // Offscreen canvas for text rendering — scaled by devicePixelRatio for crisp
  // rendering on HiDPI displays.
  private val dpr: Double = dom.window.devicePixelRatio.max(1.0)
  val offscreen: HTMLCanvasElement =
    dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  offscreen.width = (cols * cellWidth * dpr).toInt
  offscreen.height = (rows * cellHeight * dpr).toInt

  val ctx: CanvasRenderingContext2D =
    offscreen.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  ctx.scale(dpr, dpr)

  // Clear initially — transparent or black
  if (transparentBg) {
    ctx.clearRect(0, 0, cols * cellWidth, rows * cellHeight)
  } else {
    ctx.fillStyle = "#000000"
    ctx.fillRect(0, 0, cols * cellWidth, rows * cellHeight)
  }

  // Three.js texture and mesh
  val texture = new ThreeCanvasTexture(offscreen)
  texture.minFilter = ThreeLinearFilter
  texture.magFilter = ThreeNearestFilter

  private val geo = new ThreePlaneGeometry(viewportWidth, viewportHeight)
  private val mat = new ThreeMeshBasicMaterial(
    ThreeMaterialOptions(map = texture, transparent = true, side = 2)
  )
  // Transparent-background slides must not write to the depth buffer,
  // otherwise their invisible plane occludes 3D geometry behind them.
  if (transparentBg) {
    mat.depthWrite = false
  }
  val mesh = new ThreeMesh(geo, mat)

  // Position the mesh at world coordinates
  mesh.position.set(
    worldX + viewportWidth / 2.0,
    worldY + viewportHeight / 2.0,
    worldZ
  )

  // Apply rotation (convert degrees to radians)
  private val Deg2Rad = Math.PI / 180.0
  mesh.rotation.set(rotXDeg * Deg2Rad, rotYDeg * Deg2Rad, rotZDeg * Deg2Rad)

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
    texture.dispose()
    mat.dispose()
    geo.dispose()
  }
}
