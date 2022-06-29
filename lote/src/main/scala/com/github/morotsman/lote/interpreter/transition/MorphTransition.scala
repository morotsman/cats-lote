package com.github.morotsman
package lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import lote.algebra.{NConsole, Slide, Transition}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object MorphTransition {
  def apply[F[_] : Temporal](console: NConsole[F]): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def morph(distortionRate: Double, from: ScreenAdjusted, to: ScreenAdjusted): F[Unit] = {
        if (distortionRate > 2) {
          console.clear()
        } else {
          val morphedText = morphTheText(distortionRate, from, to)
          console.clear() >>
            console.writeString(morphedText) >>
            Temporal[F].sleep(100.milli) >>
            morph(distortionRate * 1.7, morphedText, to)
        }
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        _ <- console.writeString(slide1) >> morph(0.01, slide1, slide2)
      } yield ()

    }
  }

  private def morphTheText(distortionRate: Double, from: ScreenAdjusted, to: ScreenAdjusted): ScreenAdjusted = {
    val number = (from.content.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(from.content.length)).toSet
    val morphed = from.content.zipWithIndex.map { case (c, index) => if (numbers.contains(index))
      to.content.charAt(index)
    else c
    }.mkString("")
    ScreenAdjusted(morphed)
  }

}
