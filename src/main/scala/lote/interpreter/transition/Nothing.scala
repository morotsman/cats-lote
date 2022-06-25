package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.Monad
import com.github.morotsman.lote.algebra.{Slide, Transition}

object Nothing {
  def apply[F[_]: Temporal](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      Monad[F].unit
  }
}
