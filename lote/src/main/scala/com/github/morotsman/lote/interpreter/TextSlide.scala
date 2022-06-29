package com.github.morotsman
package lote.interpreter

import cats.implicits._
import lote.algebra.{NConsole, Slide}
import lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}
import cats.effect.Sync
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

object TextSlide {
  def apply[F[_] : Sync](console: NConsole[F], slideContent: String, alignment: Alignment): Slide[F] =
    new Slide[F] {
      override def content: F[ScreenAdjusted] =
        console.alignText(slideContent, alignment)

      override def startShow(): F[Unit] =
        content >>= (c => console.writeString(c))

      override def stopShow(): F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_] : Sync](console: NConsole[F]): Slide[F] =
      TextSlide[F](console, s, Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
  }

}
