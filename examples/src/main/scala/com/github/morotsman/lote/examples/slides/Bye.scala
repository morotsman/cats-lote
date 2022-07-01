package com.github.morotsman.lote.examples.slides

import cats.implicits._
import cats.Monad
import cats.effect.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Bye[F[_]: Temporal]() extends Slide[F] {
  private val text =
    """
      |
      |
      |
      |
      |
      |
      |
      |  /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$  /$$     /$$ /$$$$$$$$ /$$
      | /$$__  $$ /$$__  $$ /$$__  $$| $$__  $$| $$__  $$|  $$   /$$/| $$_____/| $$
      || $$  \__/| $$  \ $$| $$  \ $$| $$  \ $$| $$  \ $$ \  $$ /$$/ | $$      | $$
      || $$ /$$$$| $$  | $$| $$  | $$| $$  | $$| $$$$$$$   \  $$$$/  | $$$$$   | $$
      || $$|_  $$| $$  | $$| $$  | $$| $$  | $$| $$__  $$   \  $$/   | $$__/   |__/
      || $$  \ $$| $$  | $$| $$  | $$| $$  | $$| $$  \ $$    | $$    | $$
      ||  $$$$$$/|  $$$$$$/|  $$$$$$/| $$$$$$$/| $$$$$$$/    | $$    | $$$$$$$$ /$$
      | \______/  \______/  \______/ |_______/ |_______/     |__/    |________/|__/
      |""".stripMargin

  override def startShow: NConsole[F] => F[Unit] = console => {

    def distort(distortionRate: Double, text: ScreenAdjusted): F[Unit] = {
      if (distortionRate > 10) {
        Monad[F].unit
      } else {
        val distortedText = distortTheText(distortionRate, text)
        console.clear() >>
          console.writeString(distortedText) >>
          Temporal[F].sleep(200.milli) >>
          distort(distortionRate * 2, distortedText)
      }
    }

    for {
      adjustedText <- content(console)
      _ <- console.writeString(adjustedText)
      _ <- Temporal[F].sleep(1.seconds)
      _ <- distort(0.01, adjustedText)
      _ <- Temporal[F].sleep(500.milli)
      _ <- console.clear()
    } yield ()

  }

  private def distortTheText(distortionRate: Double, text: ScreenAdjusted): ScreenAdjusted = {
    val number = (text.content.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.content.length))
    val result = text.content.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      ' '
    } else c
    }.mkString("")
    ScreenAdjusted(result)
  }


  override def userInput(input: UserInput): F[Unit] = Monad[F].unit

  override def stopShow: F[Unit] = Monad[F].unit

  override def content: NConsole[F] => F[ScreenAdjusted] = console => {
    console.alignText(text, Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
  }
}
