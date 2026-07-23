package com.github.morotsman.lote.internal.interpreter.nconsole

/** Sealed trait for recording canvas factory operations. */
sealed trait CanvasOp

object CanvasOp {
  case class CreateCanvas(width: Int, height: Int) extends CanvasOp
  case class GetContext2D(canvasId: Int) extends CanvasOp
}

/** Test stub for [[CanvasFactory]] that records all operations and returns integer IDs as references.
  *
  * @param _devicePixelRatio
  *   the device pixel ratio to report
  */
class StubCanvasFactory(_devicePixelRatio: Double = 1.0) extends CanvasFactory {
  type CanvasRef = Int
  type ContextRef = Int

  private var _nextCanvasId = 1
  private var _nextContextId = 1

  /** Mutable log of all operations performed on this factory. */
  var ops: List[CanvasOp] = Nil

  private def log(op: CanvasOp): Unit = ops = ops :+ op

  override def createCanvas(width: Int, height: Int): Int = {
    val id = _nextCanvasId; _nextCanvasId += 1
    log(CanvasOp.CreateCanvas(width, height))
    id
  }

  override def getContext2D(canvas: Int): Int = {
    val id = _nextContextId; _nextContextId += 1
    log(CanvasOp.GetContext2D(canvas))
    id
  }

  override def devicePixelRatio: Double = _devicePixelRatio

  override def initContext(ctx: Int, dpr: Double, cssWidth: Int, cssHeight: Int, transparentBg: Boolean): Unit = ()

  /** Clears the operation log. */
  def clearOps(): Unit = ops = Nil
}
