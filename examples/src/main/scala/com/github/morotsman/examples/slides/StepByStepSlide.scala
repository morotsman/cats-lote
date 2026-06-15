package com.github.morotsman.examples.slides

import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.model._

object StepByStepSlide {

  def make[F[_]: Temporal: NConsole](stages: Vector[String]): F[Slide[F]] = {
    val hint = "                              [press any key to continue]"
    val stagesWithHint = stages.zipWithIndex.map { case (s, i) =>
      val withLeadingLine = "\n" + s
      if (i < stages.size - 1) withLeadingLine + "\n" + hint
      else withLeadingLine
    }
    Ref[F].of(0).map { stageRef =>
      new Slide[F] {
        override def content: F[ScreenAdjusted] =
          for {
            stage <- stageRef.get
            aligned <- NConsole[F].alignText(
              stagesWithHint(stage),
              Alignment(
                VerticalAlignment.Up,
                HorizontalAlignment.Left
              )
            )
          } yield aligned

        override def startShow: F[Unit] =
          for {
            _ <- stageRef.set(0)
            c <- content
            _ <- NConsole[F].writeString(c)
          } yield ()

        override def stopShow: F[Unit] =
          Temporal[F].unit

        override def userInput(input: UserInput): F[Unit] = input match {
          case Key(k) if k == SpecialKey.Left || k == SpecialKey.Right =>
            Temporal[F].unit
          case _ =>
            for {
              stage <- stageRef.get
              nextStage = math.min(stage + 1, stagesWithHint.size - 1)
              _ <- stageRef.set(nextStage)
              aligned <- NConsole[F].alignText(
                stagesWithHint(nextStage),
                Alignment(
                  VerticalAlignment.Up,
                  HorizontalAlignment.Left
                )
              )
              _ <- NConsole[F].writeString(aligned)
            } yield ()
        }
      }
    }
  }
}
