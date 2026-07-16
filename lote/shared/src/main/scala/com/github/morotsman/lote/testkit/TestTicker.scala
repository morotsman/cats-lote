package com.github.morotsman.lote.testkit

import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.spi.{Ticker, TickerSubscription}

import scala.concurrent.duration.FiniteDuration

/** A test implementation of `Ticker[F]` that fires subscriber callbacks only when you call `tick()`.
  *
  * Each tick advances the `SimulatedClock` by `step` duration, making `FixedStep.consumeSteps` produce simulation steps
  * instantly and deterministically — no real wall-clock delays.
  *
  * For components that block on completion (e.g. transitions using `Deferred`), use `SlideTestHarness.runWithTicking`
  * to fork the blocking task and tick alongside it.
  *
  * Example:
  * {{{
  * for {
  *   harness <- SlideTestHarness.make[IO](tickStep = 16.millis)
  *   _       <- harness.ticker.subscribe(myCallback)
  *   _       <- harness.tick()       // advances clock by 16ms, fires callbacks
  *   _       <- harness.tick(5)      // advances 5×16ms = 80ms total, fires 5 times
  * } yield ()
  * }}}
  */
final class TestTicker[F[_]: Temporal] private (
    private val subscribersRef: Ref[F, Vector[(Long, F[Unit])]],
    private val nextIdRef: Ref[F, Long],
    private val runningRef: Ref[F, Boolean],
    val clock: SimulatedClock[F],
    val step: FiniteDuration
) extends Ticker[F] {

  override def subscribe(callback: F[Unit]): F[TickerSubscription[F]] =
    for {
      id <- nextIdRef.getAndUpdate(_ + 1)
      _ <- subscribersRef.update(_ :+ (id -> callback))
    } yield new TickerSubscription[F] {
      override def cancel: F[Unit] =
        subscribersRef.update(_.filterNot(_._1 == id))
    }

  override def start: F[Unit] =
    runningRef.set(true)

  override def stop: F[Unit] =
    runningRef.set(false) *>
      subscribersRef.set(Vector.empty)

  /** Manually fire ticks — advances the simulated clock and invokes all subscriber callbacks.
    *
    * No real sleeping occurs. Each tick advances `clock` by `step`, so `FixedStep.consumeSteps` sees elapsed time
    * instantly.
    */
  def tick(times: Int = 1): F[Unit] =
    (0 until times).toList.traverse_ { _ =>
      clock.advance(step) *> subscribersRef.get.flatMap(_.toList.traverse_(_._2))
    }

  /** Returns the current number of active subscribers. */
  def subscriberCount: F[Int] = subscribersRef.get.map(_.size)

  /** Returns whether `start` has been called (and `stop` hasn't). */
  def isRunning: F[Boolean] = runningRef.get
}

object TestTicker {

  /** Creates a new `TestTicker` with its own `SimulatedClock`.
    *
    * @param step
    *   duration per tick (default 16ms)
    */
  def make[F[_]: Temporal: Ref.Make](
      step: FiniteDuration = scala.concurrent.duration.DurationInt(16).millis
  ): F[TestTicker[F]] =
    for {
      clock <- SimulatedClock.make[F]()
      subscribersRef <- Ref[F].of(Vector.empty[(Long, F[Unit])])
      nextIdRef <- Ref[F].of(0L)
      runningRef <- Ref[F].of(false)
    } yield new TestTicker[F](subscribersRef, nextIdRef, runningRef, clock, step)
}
