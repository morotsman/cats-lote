package com.github.morotsman.lote.internal.interpreter.nconsole.canvas

/** Abstracts over DOM canvas creation to decouple rendering components from browser APIs.
  *
  * Removes the direct dependency on `dom.document.createElement("canvas")`, `canvas.getContext("2d")`, and
  * `dom.window.devicePixelRatio` from [[SlideLayer]], [[WebGLEffectRenderer]], and [[ThreeJsTerminal]].
  *
  * In production, [[DomCanvasFactory]] delegates to the real DOM APIs. In tests, a stub can return no-op references or
  * record calls.
  */
private[nconsole] trait CanvasFactory {
  type CanvasRef
  type ContextRef

  /** Creates an offscreen canvas with the given dimensions.
    *
    * @param width
    *   canvas width in pixels
    * @param height
    *   canvas height in pixels
    * @return
    *   a reference to the created canvas
    */
  def createCanvas(width: Int, height: Int): CanvasRef

  /** Obtains a 2D rendering context from the given canvas.
    *
    * @param canvas
    *   the canvas to get the context from
    * @return
    *   a reference to the 2D rendering context
    */
  def getContext2D(canvas: CanvasRef): ContextRef

  /** Returns the device pixel ratio (e.g. `2.0` on Retina displays).
    *
    * Equivalent to `window.devicePixelRatio` in a browser context.
    */
  def devicePixelRatio: Double

  /** Initializes a 2D context with DPR scaling and an initial background clear.
    *
    * In production, applies `ctx.scale(dpr, dpr)` and fills/clears the background. In test stubs, this is a no-op.
    *
    * @param ctx
    *   the context reference returned by [[getContext2D]]
    * @param dpr
    *   device pixel ratio to scale by
    * @param cssWidth
    *   logical width in CSS pixels (cols * cellWidth)
    * @param cssHeight
    *   logical height in CSS pixels (rows * cellHeight)
    * @param transparentBg
    *   if true, clears to transparent; otherwise fills with black
    */
  def initContext(ctx: ContextRef, dpr: Double, cssWidth: Int, cssHeight: Int, transparentBg: Boolean): Unit
}
