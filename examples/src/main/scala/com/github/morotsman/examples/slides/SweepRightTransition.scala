package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.interpreter.animation.FixedStep
import com.github.morotsman.lote.model.{ScreenAdjusted, UserInput}

object SweepRightTransition {

  /** Example custom `Transition[F]` that reveals the next slide from left to right.
    *
    * The important teaching points are:
    *   - `transition(from, to)` is where the animation runs
    *   - `userInput(...)` is optional and can be a no-op for simple transitions
    *   - the shared `Ticker[F]` drives render cadence
    *   - `AnimationSettings.step` controls how quickly the wipe advances
    *
    * `columnsPerStep` is the simulation increment: larger values make the wipe finish in fewer updates.
    */

  def apply[F[_]: Temporal: Ref.Make: Ticker](
      columnsPerStep: Int = 3
  )(implicit console: NConsole[F], animationSettings: AnimationSettings): Transition[F] = {
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
          revealRef <- Ref[F].of(0)
          stepperRef <- FixedStep.makeRef[F]
          done <- Deferred[F, Unit]
          onTick =
            FixedStep.consumeSteps(stepperRef).flatMap { nrOfSteps =>
              if (nrOfSteps <= 0) {
                Monad[F].unit
              } else {
                revealRef
                  .modify { currentReveal =>
                    val nextReveal = math.min(
                      screen.screenWidth,
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
                      (if (revealColumns >= screen.screenWidth)
                         done.complete(()).attempt.void
                       else Monad[F].unit)
                  }
              }
            }
          maybeSub <-
            if (screen.screenWidth <= 0) Monad[F].pure(Option.empty[com.github.morotsman.lote.algebra.TickerSubscription[F]])
            else Ticker[F].subscribe(onTick).map(sub => Option(sub))
          _ <- maybeSub.traverse_(_ => Ticker[F].start)
          _ <-
            if (screen.screenWidth <= 0) Monad[F].unit
            else done.get.guarantee(maybeSub.traverse_(_.cancel))
          _ <- console.clear() *> console.writeString(toContent)
        } yield ()

      override def userInput(input: UserInput): F[Unit] = {
        val _ = input
        Temporal[F].unit
      }
    }
  }
}


