package com.github.morotsman
package lote.interpreter.transition

import cats.Monad
import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import lote.algebra.{NConsole, Slide, Transition}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object ReplaceTransition {
  def apply[F[_] : Temporal](replace: Char): Transition[F] = new Transition[F] {

    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {
      def distort(distortionRate: Double, text: ScreenAdjusted): F[Unit] = {
        if (distortionRate > 10) {
          Monad[F].unit
        } else {
          val distortedText = distortTheText(distortionRate, text, replace)
          console.clear() >>
            console.writeString(distortedText) >>
            Temporal[F].sleep(150.milli) >>
            distort(distortionRate * 2, distortedText)
        }
      }

      for {
        slide1 <- from.content
        _ <- console.writeString(slide1)  >> distort(0.01, slide1) >> Temporal[F].sleep(200.milli)
      } yield ()
    }
  }

  private def distortTheText(distortionRate: Double, text: ScreenAdjusted, replace: Char): ScreenAdjusted = {
    val number = (text.content.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.content.length)).toSet
    val distorted = text.content.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      replace
    } else c
    }.mkString("")
    ScreenAdjusted(distorted)
  }

}
