package com.github.morotsman
package lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.NConsole
import lote.algebra.{NConsole, Slide, Transition}
import lote.interpreter.NConsole

import scala.concurrent.duration.DurationInt
import scala.util.Random

object MorphTransition {
  def apply[F[_] : Temporal : NConsole](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def morph(distortionRate: Double, from: String, to: String): F[Unit] = {
        if (distortionRate > 2) {
          NConsole[F].clear()
        } else {
          val morphedText = morphTheText(distortionRate, from, to)
          NConsole[F].clear() >>
            NConsole[F].writeString(morphedText) >>
            Temporal[F].sleep(100.milli) >>
            morph(distortionRate * 1.7, morphedText, to)
        }
      }

      for {
        slide1 <- from.content
        from <- NConsole[F].centerAlignText(slide1)
        slide2 <- to.content
        to <- NConsole[F].centerAlignText(slide2)
        _ <- NConsole[F].writeString(from) >> morph(0.01, from, to)
      } yield ()

    }
  }

  private def morphTheText(distortionRate: Double, from: String, to: String): String = {
    val number = (from.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(from.length)).toSet
    from.zipWithIndex.map { case (c, index) => if (numbers.contains(index))
      to.charAt(index)
    else c
    }.mkString("")
  }

}
