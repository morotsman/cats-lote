package com.github.morotsman.lote.api.spi

import scala.annotation.implicitNotFound

/** A centralized tick source that drives render loops. Components subscribe to receive periodic tick notifications
  * instead of each maintaining their own sleep-based loop.
  */
@implicitNotFound("No implicit Ticker[${F}] found. A Ticker instance is provided by SlideContext inside slide/transition/overlay builders, or by SlideTestHarness in tests.")
trait Ticker[F[_]] {

  /** Subscribe a callback to be invoked on each tick. Returns an unsubscribe token.
    */
  def subscribe(callback: F[Unit]): F[TickerSubscription[F]]

  /** Start the ticker loop (idempotent - calling multiple times has no effect).
    */
  def start: F[Unit]

  /** Stop the ticker loop and cancel all subscriptions. */
  def stop: F[Unit]
}

object Ticker {
  @inline def apply[F[_]](implicit instance: Ticker[F]): Ticker[F] = instance
}

trait TickerSubscription[F[_]] {

  /** Unsubscribe this callback from the ticker. */
  def cancel: F[Unit]
}
