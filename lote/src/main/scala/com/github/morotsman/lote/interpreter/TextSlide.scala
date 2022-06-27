package com.github.morotsman
package lote.interpreter

import cats.implicits._
import lote.algebra.{NConsole, Slide}
import lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}
import cats.effect.Sync

object TextSlide {
  def apply[F[_] : Sync : NConsole](slideContent: String, alignment: Alignment): Slide[F] =
    new Slide[F] {
      override def content: F[String] =
        NConsole[F].alignText(slideContent, alignment)

      override def startShow(): F[Unit] =
        content >>= (NConsole[F].writeString(_))

      override def stopShow(): F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_] : Sync : NConsole]: Slide[F] =
      TextSlide[F](s, Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
  }

}
