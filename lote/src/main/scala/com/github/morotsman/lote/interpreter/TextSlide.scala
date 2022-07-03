package com.github.morotsman
package lote.interpreter

import cats.implicits._
import lote.algebra.{NConsole, Slide}
import lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}
import cats.effect.Sync
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

object TextSlide {
  def apply[F[_] : Sync](slideContent: String, alignment: Alignment): Slide[F] =
    new Slide[F] {
      override def content: NConsole[F] => F[ScreenAdjusted] =
        console => console.alignText(slideContent, alignment)

      override def startShow: NConsole[F] => F[Unit] =
        console => content(console) >>= (c => console.writeString(c))

      override def stopShow: F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_] : Sync](): Slide[F] =
      TextSlide[F](s, Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
  }

}
