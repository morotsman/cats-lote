package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{Clock, FixedStep}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, TickerSubscription, Transition}

object SweepRightTransition {

  def contextual[F[_]: Temporal: Ref.Make](columnsPerStep: Int = 3): Contextual[F, Transition[F]] =
    Contextual { ctx =>
      create[F](columnsPerStep, ctx.console, ctx.ticker, ctx.animationSettings)
    }

  def create[F[_]: Temporal: Ref.Make](
      columnsPerStep: Int = 3,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: Clock[F]): Transition[F] = {
    require(columnsPerStep > 0, "columnsPerStep must be greater than 0")

    def normalizeLine(line: String, width: Int): String = {
      val truncated = line.take(width)
      truncated + (" " * (width - truncated.length))
    }

    def renderFrame(
        from: ScreenAdjusted,
        to: ScreenAdjusted,
        revealColumns: Int,
        screenWidth: Int,
        screenHeight: Int
    ): ScreenAdjusted = {
      val fromLines = from.content.split("\n", -1).toVector
      val toLines = to.content.split("\n", -1).toVector
      val clampedReveal = math.max(0, math.min(revealColumns, screenWidth))

      ScreenAdjusted(
        (0 until screenHeight)
          .map { row =>
            val fromLine = normalizeLine(fromLines.lift(row).getOrElse(""), screenWidth)
            val toLine = normalizeLine(toLines.lift(row).getOrElse(""), screenWidth)
            toLine.take(clampedReveal) + fromLine.drop(clampedReveal)
          }
          .mkString("\n")
      )
    }

    new Transition[F] {
      override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
        for {
          screen <- console.context
          fromContent <- from.content
          toContent <- to.content
          contentWidth = {
            val fromMax = fromContent.content.split("\n", -1).map(_.stripTrailing().length).maxOption.getOrElse(0)
            val toMax = toContent.content.split("\n", -1).map(_.stripTrailing().length).maxOption.getOrElse(0)
            math.min(math.max(fromMax, toMax), screen.screenWidth)
          }
          revealRef <- Ref[F].of(0)
          stepperRef <- FixedStep.makeRef[F]
          done <- Deferred[F, Unit]
          onTick =
            FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap { nrOfSteps =>
              if (nrOfSteps <= 0) {
                Monad[F].unit
              } else {
                revealRef
                  .modify { currentReveal =>
                    val nextReveal = math.min(
                      contentWidth,
                      currentReveal + (nrOfSteps * columnsPerStep)
                    )
                    (nextReveal, nextReveal)
                  }
                  .flatMap { revealColumns =>
                    val frame = renderFrame(
                      fromContent,
                      toContent,
                      revealColumns,
                      screen.screenWidth,
                      screen.screenHeight
                    )

                    console.clear() *>
                      console.writeString(frame) *>
                      (if (revealColumns >= contentWidth)
                         done.complete(()).attempt.void
                       else Monad[F].unit)
                  }
              }
            }
          maybeSub <-
            if (contentWidth <= 0) Monad[F].pure(Option.empty[TickerSubscription[F]])
            else ticker.subscribe(onTick).map(Option(_))
          _ <- maybeSub.traverse_(_ => ticker.start)
          _ <-
            if (contentWidth <= 0) Monad[F].unit
            else done.get.guarantee(maybeSub.traverse_(_.cancel))
          _ <- console.clear() *> console.writeString(toContent)
        } yield ()

      override def userInput(input: UserInput): F[Unit] = {
        val _ = input
        Monad[F].unit
      }
    }
  }
}
