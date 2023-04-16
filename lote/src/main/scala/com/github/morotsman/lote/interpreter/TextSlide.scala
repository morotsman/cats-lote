package com.github.morotsman
package lote.interpreter

import cats.effect.Sync
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}

object TextSlide {
  def apply[F[_] : Sync : NConsole](slideContent: String, alignment: Alignment): Slide[F] =
    new Slide[F] {
      override def content: F[ScreenAdjusted] =
        NConsole[F].alignText(slideContent, alignment)

      override def startShow: F[Unit] =
        content >>= (c => NConsole[F].writeString(c))

      override def stopShow: F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_] : Sync : NConsole](): Slide[F] =
      TextSlide[F](s, Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
  }

}
