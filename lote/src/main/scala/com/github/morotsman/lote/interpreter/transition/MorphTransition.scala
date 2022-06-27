package com.github.morotsman
package lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import lote.algebra.{NConsole, Slide, Transition}

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
        slide2 <- to.content
        _ <- NConsole[F].writeString(slide1) >> morph(0.01, slide1, slide2)
      } yield ()

    }
  }

  private def morphTheText(distortionRate: Double, from: String, to: String): String = {
    println(from.length)
    println(from.split("\n").length)
    println(to.length)
    println(to.split("\n").length)
    val number = (from.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(from.length)).toSet
    from.zipWithIndex.map { case (c, index) => if (numbers.contains(index))
      to.charAt(index)
    else c
    }.mkString("")
  }

}
