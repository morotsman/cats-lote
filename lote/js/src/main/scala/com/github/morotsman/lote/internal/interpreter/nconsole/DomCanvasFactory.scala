package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}

/** Production [[CanvasFactory]] that delegates to the real DOM APIs.
  *
  * Creates actual `<canvas>` elements via `document.createElement` and reads the real `devicePixelRatio`.
  */
private[nconsole] object DomCanvasFactory extends CanvasFactory {
  type CanvasRef = HTMLCanvasElement
  type ContextRef = CanvasRenderingContext2D

  override def createCanvas(width: Int, height: Int): HTMLCanvasElement = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.width = width
    canvas.height = height
    canvas
  }

  override def getContext2D(canvas: HTMLCanvasElement): CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  override def devicePixelRatio: Double =
    dom.window.devicePixelRatio.max(1.0)

  override def initContext(
      ctx: CanvasRenderingContext2D,
      dpr: Double,
      cssWidth: Int,
      cssHeight: Int,
      transparentBg: Boolean
  ): Unit = {
    ctx.scale(dpr, dpr)
    if (transparentBg) {
      ctx.clearRect(0, 0, cssWidth, cssHeight)
    } else {
      ctx.fillStyle = "#000000"
      ctx.fillRect(0, 0, cssWidth, cssHeight)
    }
  }
}
