package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}

/** A single slide's rendering surface in spatial mode.
  *
  * Each `SlideLayer` owns an offscreen canvas (for ANSI text rendering), a Three.js texture,
  * and a mesh positioned at the slide's world-space coordinates.
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
    cellHeight: Int
) {

  val cols: Int = viewportWidth / cellWidth
  val rows: Int = viewportHeight / cellHeight

  // Offscreen canvas for text rendering
  val offscreen: HTMLCanvasElement =
    dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  offscreen.width = cols * cellWidth
  offscreen.height = rows * cellHeight

  val ctx: CanvasRenderingContext2D =
    offscreen.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  // Clear to black initially
  ctx.fillStyle = "#000000"
  ctx.fillRect(0, 0, offscreen.width, offscreen.height)

  // Three.js texture and mesh
  val texture = new ThreeCanvasTexture(offscreen)
  texture.minFilter = ThreeLinearFilter
  texture.magFilter = ThreeLinearFilter

  private val geo = new ThreePlaneGeometry(viewportWidth, viewportHeight)
  private val mat = new ThreeMeshBasicMaterial(
    scalajs.js.Dynamic.literal(map = texture, transparent = true, side = 2)
      .asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]]
  )
  val mesh = new ThreeMesh(geo, mat)

  // Position the mesh at world coordinates
  // The mesh center is at (worldX + viewportWidth/2, worldY + viewportHeight/2, worldZ)
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

  def dispose(): Unit = {
    texture.dispose()
    mat.dispose()
    geo.dispose()
  }
}

