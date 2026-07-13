package com.github.morotsman.lote.api.support

import cats.Monad
import cats.syntax.all._
import cats.effect.kernel.{Ref, Temporal}
import com.github.morotsman.lote.api.AnimationSettings

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

final case class FixedStepState(
    lastUpdateAt: FiniteDuration,
    accumulated: FiniteDuration
)

object FixedStep {
  val DefaultAnimationStep: FiniteDuration = AnimationSettings.DefaultStep

  def initialState[F[_]](implicit clock: Clock[F], monad: Monad[F]): F[FixedStepState] =
    clock.monotonic.map(now => FixedStepState(now, FiniteDuration(0, NANOSECONDS)))

  def makeRef[F[_]: Monad: Ref.Make](implicit clock: Clock[F]): F[Ref[F, FixedStepState]] =
    initialState[F].flatMap(Ref[F].of)

  def reset[F[_]: Monad](stateRef: Ref[F, FixedStepState])(implicit clock: Clock[F]): F[Unit] =
    initialState[F].flatMap(stateRef.set)

  def consumeSteps[F[_]: Monad](
      stateRef: Ref[F, FixedStepState]
  )(implicit animationSettings: AnimationSettings, clock: Clock[F]): F[Int] =
    consumeSteps(stateRef, animationSettings.step)

  def consumeSteps[F[_]: Monad](
      stateRef: Ref[F, FixedStepState],
      step: FiniteDuration = DefaultAnimationStep
  )(implicit clock: Clock[F]): F[Int] =
    clock.monotonic.flatMap { now =>
      stateRef.modify { state =>
        val elapsedNanos = Math.max(0L, (now - state.lastUpdateAt).toNanos)
        val totalNanos = state.accumulated.toNanos + elapsedNanos
        val stepNanos = Math.max(1L, step.toNanos)
        val steps = Math.min(Int.MaxValue.toLong, totalNanos / stepNanos).toInt
        val remainder = totalNanos % stepNanos

        (
          state.copy(
            lastUpdateAt = now,
            accumulated = FiniteDuration(remainder, NANOSECONDS)
          ),
          steps
        )
      }
    }
}
