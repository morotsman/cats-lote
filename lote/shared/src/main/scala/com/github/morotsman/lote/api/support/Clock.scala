package com.github.morotsman.lote.api.support

import cats.effect.Temporal

import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration

/** A time source used by `FixedStep` to measure elapsed time between invocations.
  *
  * In production, this delegates to `Temporal[F].monotonic` (wall-clock monotonic time). In tests, you can replace it
  * with a `SimulatedClock` that advances only when you tell it to, making animation tests instantaneous and
  * deterministic.
  *
  * Named `AnimationClock` (rather than just `Clock`) to avoid confusion with `cats.effect.Clock`, which is a different
  * abstraction from the Cats Effect library.
  */
@implicitNotFound(
  "No implicit AnimationClock[${F}] found. An AnimationClock instance is derived automatically from Temporal[${F}], or you can provide a SimulatedClock in tests via SlideTestHarness."
)
trait AnimationClock[F[_]] {

  /** Returns the current monotonic time. */
  def monotonic: F[FiniteDuration]
}

object AnimationClock extends AnimationClockLowPriority {
  @inline def apply[F[_]](implicit instance: AnimationClock[F]): AnimationClock[F] = instance
}

/** Low-priority implicit derivation so that an explicit `AnimationClock[F]` instance (e.g., `SimulatedClock`) takes
  * priority over this derived one.
  */
trait AnimationClockLowPriority {

  /** Derives an `AnimationClock[F]` from `Temporal[F].monotonic` (real wall-clock time). */
  implicit def fromTemporal[F[_]: Temporal]: AnimationClock[F] =
    new AnimationClock[F] {
      override def monotonic: F[FiniteDuration] = Temporal[F].monotonic
    }
}
