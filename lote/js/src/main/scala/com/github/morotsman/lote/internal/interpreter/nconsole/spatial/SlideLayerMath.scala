package com.github.morotsman.lote.internal.interpreter.nconsole.spatial

/** Pure mathematical computations for [[SlideLayer]] construction.
  *
  * Extracted to allow unit testing of mesh positioning, rotation conversion, and grid dimension calculations without
  * requiring DOM or Three.js.
  */
private[nconsole] object SlideLayerMath {

  /** Conversion factor from degrees to radians. */
  val Deg2Rad: Double = Math.PI / 180.0

  /** Computes the number of character columns that fit in the viewport.
    *
    * @param viewportWidth
    *   total viewport width in pixels
    * @param cellWidth
    *   width of a single character cell in pixels
    * @return
    *   number of columns
    */
  def computeCols(viewportWidth: Int, cellWidth: Int): Int =
    viewportWidth / cellWidth

  /** Computes the number of character rows that fit in the viewport.
    *
    * @param viewportHeight
    *   total viewport height in pixels
    * @param cellHeight
    *   height of a single character cell in pixels
    * @return
    *   number of rows
    */
  def computeRows(viewportHeight: Int, cellHeight: Int): Int =
    viewportHeight / cellHeight

  /** Computes the mesh center position in world space.
    *
    * The slide's origin is at its top-left corner. The mesh is centered, so we add half the dimensions.
    *
    * @return
    *   (x, y, z) mesh center position
    */
  def meshPosition(
      worldX: Double,
      worldY: Double,
      worldZ: Double,
      viewportWidth: Int,
      viewportHeight: Int
  ): (Double, Double, Double) =
    (worldX + viewportWidth / 2.0, worldY + viewportHeight / 2.0, worldZ)

  /** Converts Euler rotation angles from degrees to radians.
    *
    * @return
    *   (rx, ry, rz) in radians
    */
  def meshRotation(rotXDeg: Double, rotYDeg: Double, rotZDeg: Double): (Double, Double, Double) =
    (rotXDeg * Deg2Rad, rotYDeg * Deg2Rad, rotZDeg * Deg2Rad)

  /** Computes the offscreen canvas dimensions in physical pixels (accounting for device pixel ratio).
    *
    * @param cols
    *   number of character columns
    * @param rows
    *   number of character rows
    * @param cellWidth
    *   width of a single cell in CSS pixels
    * @param cellHeight
    *   height of a single cell in CSS pixels
    * @param dpr
    *   device pixel ratio (≥ 1.0)
    * @return
    *   (canvasWidth, canvasHeight) in physical pixels
    */
  def canvasDimensions(cols: Int, rows: Int, cellWidth: Int, cellHeight: Int, dpr: Double): (Int, Int) =
    ((cols * cellWidth * dpr).toInt, (rows * cellHeight * dpr).toInt)
}
