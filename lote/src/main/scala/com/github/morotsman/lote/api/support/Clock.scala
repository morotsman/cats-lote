package com.github.morotsman.lote.api.support

import cats.effect.Temporal

import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration

/** A time source used by `FixedStep` to measure elapsed time between invocations.
  *
  * In production, this delegates to `Temporal[F].monotonic` (wall-clock monotonic time). In tests, you can replace it
  * with a `SimulatedClock` that advances only when you tell it to, making animation tests instantaneous and
  * deterministic.
  */
@implicitNotFound("No implicit Clock[${F}] found. A Clock instance is derived automatically from Temporal[${F}], or you can provide a SimulatedClock in tests via SlideTestHarness.")
trait Clock[F[_]] {

  /** Returns the current monotonic time. */
  def monotonic: F[FiniteDuration]
}

object Clock extends ClockLowPriority {
  @inline def apply[F[_]](implicit instance: Clock[F]): Clock[F] = instance
}

/** Low-priority implicit derivation so that an explicit `Clock[F]` instance (e.g., `SimulatedClock`) takes priority
  * over this derived one.
  */
trait ClockLowPriority {

  /** Derives a `Clock[F]` from `Temporal[F].monotonic` (real wall-clock time). */
  implicit def fromTemporal[F[_]: Temporal]: Clock[F] =
    new Clock[F] {
      override def monotonic: F[FiniteDuration] = Temporal[F].monotonic
    }
}
