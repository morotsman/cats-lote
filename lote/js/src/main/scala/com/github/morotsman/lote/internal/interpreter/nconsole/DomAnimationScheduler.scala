package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom

/** Production [[AnimationScheduler]] that delegates to the real DOM APIs.
  *
  * Uses `dom.window.requestAnimationFrame`, `dom.window.cancelAnimationFrame`, and `dom.window.performance.now()`.
  */
private[nconsole] object DomAnimationScheduler extends AnimationScheduler {

  override def requestFrame(callback: Double => Unit): Int =
    dom.window.requestAnimationFrame(callback)

  override def cancelFrame(id: Int): Unit =
    dom.window.cancelAnimationFrame(id)

  override def now(): Double =
    dom.window.performance.now()
}
