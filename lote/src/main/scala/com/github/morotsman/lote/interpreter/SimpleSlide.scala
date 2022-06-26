package com.github.morotsman
package lote.interpreter

import cats.implicits._
import lote.algebra.{NConsole, Slide}
import lote.model.UserInput

import cats.effect.Sync

object SimpleSlide {
  def apply[F[_] : Sync : NConsole](slideContent: String): Slide[F] =
    new Slide[F] {
      override def content: F[String] =
        Sync[F].pure(slideContent)

      override def startShow(): F[Unit] =
        content >>= (NConsole[F].writeStringCenterAligned(_))

      override def stopShow(): F[Unit] =
        Sync[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Sync[F].unit
    }

  implicit class ToSimpleSlide(val s: String) {
    def toSlide[F[_] : Sync : NConsole]: Slide[F] = SimpleSlide[F](s)
  }

}
