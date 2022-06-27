package com.github.morotsman.lote.examples.slides

import cats.implicits._
import cats.Monad
import cats.effect.kernel.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconcole.NConsole
import com.github.morotsman.lote.model.UserInput

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Bye[F[_] : NConsole : Temporal]() extends Slide[F] {
  private val text =
    """
      |
      |
      |
      |
      |
      |
      |
      |                                                       /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$  /$$     /$$ /$$$$$$$$ /$$
      |                                                      /$$__  $$ /$$__  $$ /$$__  $$| $$__  $$| $$__  $$|  $$   /$$/| $$_____/| $$
      |                                                     | $$  \__/| $$  \ $$| $$  \ $$| $$  \ $$| $$  \ $$ \  $$ /$$/ | $$      | $$
      |                                                     | $$ /$$$$| $$  | $$| $$  | $$| $$  | $$| $$$$$$$   \  $$$$/  | $$$$$   | $$
      |                                                     | $$|_  $$| $$  | $$| $$  | $$| $$  | $$| $$__  $$   \  $$/   | $$__/   |__/
      |                                                     | $$  \ $$| $$  | $$| $$  | $$| $$  | $$| $$  \ $$    | $$    | $$
      |                                                     |  $$$$$$/|  $$$$$$/|  $$$$$$/| $$$$$$$/| $$$$$$$/    | $$    | $$$$$$$$ /$$
      |                                                      \______/  \______/  \______/ |_______/ |_______/     |__/    |________/|__/
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |""".stripMargin

  override def startShow(): F[Unit] = {

    def distort(distortionRate: Double, text: String): F[Unit] = {
      if (distortionRate > 2) {
        NConsole[F].clear()
      } else {
        val distortedText = distortTheText(distortionRate, text)
        NConsole[F].clear() >>
          NConsole[F].writeString(distortedText) >>
          Temporal[F].sleep(200.milli) >>
          distort(distortionRate * 2, distortedText)
      }
    }

    NConsole[F].writeString(text) >>
      Temporal[F].sleep(2.seconds) >>
      distort(0.01, text) >>
      Temporal[F].sleep(500.milli) >>
      NConsole[F].clear()
  }

  private def distortTheText(distortionRate: Double, text: String): String = {
    val number = (text.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.length))
    text.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      ' '
    } else c
    }.mkString("")
  }


  override def userInput(input: UserInput): F[Unit] = Monad[F].unit

  override def stopShow(): F[Unit] = Monad[F].unit

  override def content: F[String] = Temporal[F].pure(text)
}
