package com.github.morotsman.lote.interpreter.transition


import cats.implicits._
import cats.effect.kernel.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.NConsole

import scala.concurrent.duration.DurationInt

object TextTransition {
  def apply[F[_]: Temporal: NConsole](toWrite: String): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      NConsole[F].writeStringCenterAligned(toWrite) >> Temporal[F].sleep(1.seconds)
  }
}
