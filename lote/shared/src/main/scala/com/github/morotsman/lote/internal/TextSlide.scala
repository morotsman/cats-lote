package com.github.morotsman
package lote.internal

import cats.Monad
import cats.effect.Temporal
import cats.implicits._
import com.github.morotsman.lote.api.{
  Alignment,
  Character,
  HorizontalAlignment,
  Key,
  ScreenAdjusted,
  SpecialKey,
  UserInput,
  VerticalAlignment
}
import com.github.morotsman.lote.api.spi.{NConsole, Slide}

import java.util.concurrent.atomic.AtomicInteger

object TextSlide {
  private[lote] val DefaultStepHint: String = "[press any key to continue]"

  private[lote] val DefaultStepSeparator: String = "\n"

  private def shouldAdvance(input: UserInput): Boolean = input match {
    case Character(_)                      => true
    case Key(k) if k != SpecialKey.Timeout => true
    case _                                 => false
  }

  def apply[F[_]: Monad: NConsole](
      slideContent: String,
      alignment: Alignment
  ): Slide[F] =
    new Slide[F] {
      override def content: F[Option[ScreenAdjusted]] =
        NConsole[F].alignText(slideContent, alignment).map(Some(_))

      override def startShow: F[Unit] =
        content >>= (_.traverse_(c => NConsole[F].writeString(c)))

      override def stopShow: F[Unit] =
        Monad[F].unit

      override def userInput(input: UserInput): F[Unit] =
        Monad[F].unit
    }

  private[internal] def stepped[F[_]: Monad: NConsole](
      stages: Vector[String],
      alignment: Alignment,
      hint: Option[String]
  ): Slide[F] = {
    val normalizedStages = if (stages.nonEmpty) stages else Vector("")
    val stagesWithHint = normalizedStages.zipWithIndex.map { case (stage, index) =>
      if (index < normalizedStages.size - 1) {
        hint.filter(_.nonEmpty).fold(stage) { value =>
          stage + "\n\n" + value
        }
      } else {
        stage
      }
    }
    val stageIndex = new AtomicInteger(0)

    def renderStage(index: Int): F[ScreenAdjusted] =
      NConsole[F].alignText(stagesWithHint(index), alignment)

    new Slide[F] {
      override def content: F[Option[ScreenAdjusted]] =
        renderStage(stageIndex.get()).map(Some(_))

      override def startShow: F[Unit] =
        for {
          _ <- Monad[F].pure(stageIndex.set(0))
          current <- renderStage(0)
          _ <- NConsole[F].writeString(current)
        } yield ()

      override def stopShow: F[Unit] =
        Monad[F].pure(stageIndex.set(0))

      override def userInput(input: UserInput): F[Unit] =
        if (shouldAdvance(input)) {
          for {
            nextStage <- Monad[F].pure {
              val current = stageIndex.get()
              val next = math.min(current + 1, stagesWithHint.size - 1)
              stageIndex.set(next)
              next
            }
            aligned <- renderStage(nextStage)
            _ <- NConsole[F].writeString(aligned)
          } yield ()
        } else {
          Monad[F].unit
        }
    }
  }

  def staged[F[_]: Monad: NConsole](
      initialContent: String,
      steps: Vector[String],
      alignment: Alignment,
      separator: String = DefaultStepSeparator,
      hint: Option[String] = Some(DefaultStepHint)
  ): Slide[F] = {
    val stages = steps.scanLeft(initialContent) { case (current, nextStep) =>
      current + separator + nextStep
    }
    stepped(stages, alignment, hint)
  }

  implicit class ToTextSlide(val s: String) {
    def toSlide[F[_]: Temporal: NConsole](): Slide[F] =
      TextSlide[F](
        s,
        Alignment(VerticalAlignment.Up, HorizontalAlignment.Center)
      )
  }

}
