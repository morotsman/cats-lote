package com.github.morotsman
package lote.interpreter

import cats.effect.Sync
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}

object TextSlide {
  def apply[F[_] : Sync](slideContent: String, alignment: Alignment)(implicit console: NConsole[F]): Slide[F] =
    new Slide[F] {
      override def content: F[ScreenAdjusted] =
        console.alignText(slideContent, alignment)

      override def startShow: F[Unit] =
        content >>= (c => console.writeString(c))

      override def stopShow: F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_] : Sync]()(implicit console: NConsole[F]): Slide[F] =
      TextSlide[F](s, Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
  }

}
