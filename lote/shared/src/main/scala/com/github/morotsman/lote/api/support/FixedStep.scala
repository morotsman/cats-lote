package com.github.morotsman.lote.api.support

import cats.Monad
import cats.syntax.all._
import cats.effect.kernel.Ref
import com.github.morotsman.lote.api.AnimationSettings

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

final case class FixedStepState(
    lastUpdateAt: FiniteDuration,
    accumulated: FiniteDuration
)

object FixedStep {
  val DefaultAnimationStep: FiniteDuration = AnimationSettings.DefaultStep

  def initialState[F[_]](implicit clock: AnimationClock[F], monad: Monad[F]): F[FixedStepState] =
    clock.monotonic.map(now => FixedStepState(now, FiniteDuration(0, NANOSECONDS)))

  def makeRef[F[_]: Monad: Ref.Make](implicit clock: AnimationClock[F]): F[Ref[F, FixedStepState]] =
    initialState[F].flatMap(Ref[F].of)

  def reset[F[_]: Monad](stateRef: Ref[F, FixedStepState])(implicit clock: AnimationClock[F]): F[Unit] =
    initialState[F].flatMap(stateRef.set)

  /** Consume accumulated simulation steps and return fractional progress toward the next step.
    *
    * Reads the clock, computes how many complete `step`-sized intervals have elapsed since the last call, and returns
    * both the count and the fractional progress (0.0–1.0) toward the next step.
    *
    * The progress value is useful for smooth interpolation between discrete simulation steps. For example, a transition
    * can use `progress` to position elements at sub-cell offsets, creating smooth motion at the display's native
    * refresh rate even when the simulation runs at a lower rate.
    *
    * @return
    *   `(steps, progress)` where `steps` is the number of complete steps consumed and `progress` is the fraction of the
    *   next step that has accumulated (0.0 = step just fired, approaching 1.0 = about to fire).
    */
  def consumeSteps[F[_]: Monad](
      stateRef: Ref[F, FixedStepState]
  )(implicit animationSettings: AnimationSettings, clock: AnimationClock[F]): F[(Int, Double)] =
    consumeSteps(stateRef, animationSettings.step)

  def consumeSteps[F[_]: Monad](
      stateRef: Ref[F, FixedStepState],
      step: FiniteDuration = DefaultAnimationStep
  )(implicit clock: AnimationClock[F]): F[(Int, Double)] =
    clock.monotonic.flatMap { now =>
      stateRef.modify { state =>
        val elapsedNanos = Math.max(0L, (now - state.lastUpdateAt).toNanos)
        val totalNanos = state.accumulated.toNanos + elapsedNanos
        val stepNanos = Math.max(1L, step.toNanos)
        val steps = Math.min(Int.MaxValue.toLong, totalNanos / stepNanos).toInt
        val remainder = totalNanos % stepNanos
        val progress = remainder.toDouble / stepNanos.toDouble

        (
          state.copy(
            lastUpdateAt = now,
            accumulated = FiniteDuration(remainder, NANOSECONDS)
          ),
          (steps, progress)
        )
      }
    }
}
