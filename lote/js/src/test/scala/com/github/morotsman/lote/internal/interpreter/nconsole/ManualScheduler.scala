package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.internal.interpreter.nconsole.scene.AnimationScheduler

/** Test stub for [[AnimationScheduler]] that allows manual control of time and frame callbacks.
  *
  * Pending callbacks are collected and can be fired deterministically via [[tick]]. Time is advanced manually via
  * [[advanceTime]], giving tests full control over animation timing.
  *
  * Similar to `TestTicker` but for the browser animation loop.
  */
private[nconsole] class ManualScheduler extends AnimationScheduler {

  private var _now: Double = 0.0
  private var _nextId: Int = 1
  private var _pending: List[(Int, Double => Unit)] = Nil
  private var _cancelledIds: Set[Int] = Set.empty

  /** All frame IDs that have been requested (for assertion). */
  var requestedFrameIds: List[Int] = Nil

  /** All frame IDs that have been cancelled (for assertion). */
  var cancelledFrameIds: List[Int] = Nil

  override def requestFrame(callback: Double => Unit): Int = {
    val id = _nextId
    _nextId += 1
    _pending = _pending :+ (id, callback)
    requestedFrameIds = requestedFrameIds :+ id
    id
  }

  override def cancelFrame(id: Int): Unit = {
    _cancelledIds += id
    _pending = _pending.filterNot(_._1 == id)
    cancelledFrameIds = cancelledFrameIds :+ id
  }

  override def now(): Double = _now

  /** Advances the internal clock by `deltaMs` milliseconds. */
  def advanceTime(deltaMs: Double): Unit = {
    _now += deltaMs
  }

  /** Fires all pending frame callbacks with the current timestamp, then clears the pending list.
    *
    * Note: callbacks may schedule new frames; those will appear in the pending list after this call.
    *
    * @return
    *   the number of callbacks that were fired
    */
  def tick(): Int = {
    val toFire = _pending.filterNot(e => _cancelledIds.contains(e._1))
    _pending = Nil
    toFire.foreach { case (_, cb) => cb(_now) }
    toFire.length
  }

  /** Advances time by `deltaMs` and then fires all pending callbacks. Convenience for the common pattern. */
  def tickAt(deltaMs: Double): Int = {
    advanceTime(deltaMs)
    tick()
  }

  /** Returns the number of currently pending (unfired, uncancelled) frame callbacks. */
  def pendingCount: Int = _pending.count(e => !_cancelledIds.contains(e._1))

  /** Returns `true` if there are no pending callbacks. */
  def isEmpty: Boolean = pendingCount == 0
}
