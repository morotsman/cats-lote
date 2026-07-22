package com.github.morotsman.lote.testkit

import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.support.AnimationClock

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** A simulated clock for testing animations without wall-clock delays.
  *
  * Time only advances when you call `advance(duration)`. This makes `FixedStep.consumeSteps` produce simulation steps
  * instantly and deterministically, without needing `IO.sleep`.
  *
  * Example:
  * {{{
  * for {
  *   clock    <- SimulatedClock.make[IO]()
  *   stepper  <- FixedStep.makeRef[IO](clock, implicitly)
  *   _        <- clock.advance(40.millis)
  *   steps    <- FixedStep.consumeSteps(stepper, 16.millis)(clock)
  * } yield assertEquals(steps, 2)  // 40ms / 16ms = 2 steps
  * }}}
  */
final class SimulatedClock[F[_]] private (
    private val timeRef: Ref[F, FiniteDuration]
)(implicit F: Temporal[F])
    extends AnimationClock[F] {

  override def monotonic: F[FiniteDuration] = timeRef.get

  /** Advance the simulated time by the given duration. */
  def advance(duration: FiniteDuration): F[Unit] =
    timeRef.update(_ + duration)

  /** Set the simulated time to an absolute value. */
  def set(time: FiniteDuration): F[Unit] =
    timeRef.set(time)

  /** Returns the current simulated time. */
  def currentTime: F[FiniteDuration] = timeRef.get
}

object SimulatedClock {

  /** Creates a new `SimulatedClock` starting at time zero. */
  def make[F[_]: Temporal: Ref.Make](
      startTime: FiniteDuration = FiniteDuration(0, NANOSECONDS)
  ): F[SimulatedClock[F]] =
    Ref[F].of(startTime).map(new SimulatedClock[F](_))
}
