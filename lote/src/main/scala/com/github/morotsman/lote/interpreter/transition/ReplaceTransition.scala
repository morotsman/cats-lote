package com.github.morotsman
package lote.interpreter.transition

import cats.Monad
import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.interpreter.nconcole.NConsole
import lote.algebra.{NConsole, Slide, Transition}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object ReplaceTransition {
  def apply[F[_] : Temporal : NConsole](replace: Char): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      def distort(distortionRate: Double, text: String): F[Unit] = {
        if (distortionRate > 10) {
          Monad[F].unit
        } else {
          val distortedText = distortTheText(distortionRate, text, replace)
          NConsole[F].clear() >>
            NConsole[F].writeString(distortedText) >>
            Temporal[F].sleep(150.milli) >>
            distort(distortionRate * 2, distortedText)
        }
      }

      for {
        slide1 <- from.content
        _ <- NConsole[F].writeString(slide1)  >> distort(0.01, slide1) >> Temporal[F].sleep(1.seconds)
      } yield ()
    }
  }

  private def distortTheText(distortionRate: Double, text: String, replace: Char): String = {
    val number = (text.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.length)).toSet
    text.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      replace
    } else c
    }.mkString("")
  }

}
