package com.github.morotsman.lote.interpreter.transition

import cats.Monad
import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.NConsole

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
        from <- NConsole[F].centerAlignText(slide1)
        _ <- NConsole[F].writeString(from)  >> distort(0.01, from) >> Temporal[F].sleep(1.seconds)
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
