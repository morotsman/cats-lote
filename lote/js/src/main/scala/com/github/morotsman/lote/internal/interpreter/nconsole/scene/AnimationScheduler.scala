package com.github.morotsman.lote.internal.interpreter.nconsole.scene

/** Abstracts over `requestAnimationFrame`, `cancelAnimationFrame`, and `performance.now` to make animation-loop code
  * testable without a browser environment.
  *
  * In production, [[DomAnimationScheduler]] delegates to the real DOM APIs. In tests, a manual scheduler can be used to
  * advance time and fire frame callbacks deterministically.
  *
  * @see
  *   [[CameraAnimator]] for the primary consumer of this trait
  */
private[nconsole] trait AnimationScheduler {

  /** Schedules `callback` to be called before the next repaint.
    *
    * @param callback
    *   a function receiving the current timestamp in milliseconds (same as `performance.now()`)
    * @return
    *   an opaque ID that can be passed to [[cancelFrame]] to cancel the pending callback
    */
  def requestFrame(callback: Double => Unit): Int

  /** Cancels a previously scheduled frame callback.
    *
    * @param id
    *   the ID returned by [[requestFrame]]
    */
  def cancelFrame(id: Int): Unit

  /** Returns the current high-resolution timestamp in milliseconds.
    *
    * Equivalent to `performance.now()` in a browser context.
    */
  def now(): Double
}
