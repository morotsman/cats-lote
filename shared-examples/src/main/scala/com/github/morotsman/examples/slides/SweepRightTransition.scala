package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.syntax.all._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, GlideLayer, SmoothChar, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

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

  def contextual[F[_]: Temporal: Ref.Make](columnsPerStep: Int = 3): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create[F](columnsPerStep, builder)
    }

  def create[F[_]: Temporal: Ref.Make](
      columnsPerStep: Int = 3,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] =
    create(columnsPerStep, TickedTransition(console, ticker, animationSettings))

  def create[F[_]: Temporal: Ref.Make](
      columnsPerStep: Int,
      builder: TickedTransition.Builder[F]
  )(implicit clock: AnimationClock[F]): Transition[F] = {
    require(columnsPerStep > 0, "columnsPerStep must be greater than 0")

    builder
      .withGlideLayer(wrapThreshold = columnsPerStep)
      .buildWithSetup { (fromContent, toContent, complete) =>
        val console = builder.console
        for {
          screen <- console.context
          contentWidth = {
            val fromMax = fromContent.content.split("\n", -1).map(_.stripTrailing().length).maxOption.getOrElse(0)
            val toMax = toContent.content.split("\n", -1).map(_.stripTrailing().length).maxOption.getOrElse(0)
            math.min(math.max(fromMax, toMax), screen.screenWidth)
          }
          gridLayer <- GlideLayer.make[F](console, builder.animationSettings.step, wrapThreshold = columnsPerStep)
          revealRef <- Ref[F].of(0)
          toLines = toContent.content.split("\n", -1).toVector
        } yield TickedTransition.TickHandler(
          onTick = (nrOfSteps: Int, progress: Double) => {
            for {
              baseReveal <- revealRef.modify { currentReveal =>
                val nextReveal =
                  if (nrOfSteps > 0) math.min(contentWidth, currentReveal + (nrOfSteps * columnsPerStep))
                  else currentReveal
                (nextReveal, nextReveal)
              }
              smoothReveal = math.min(contentWidth, baseReveal + (progress * columnsPerStep).toInt)
              frame = renderFrame(fromContent, toContent, smoothReveal, screen.screenWidth, screen.screenHeight)
              rendered <-
                if (smoothReveal < contentWidth)
                  gridLayer.renderOnto(
                    frame,
                    sweepEdgeChars(toLines, smoothReveal, screen.screenWidth, screen.screenHeight)
                  )
                else
                  gridLayer.clear().as(frame)
              _ <- console.clear()
              _ <- console.writeString(rendered)
              _ <- if (smoothReveal >= contentWidth) complete else Monad[F].unit
            } yield ()
          },
          cleanup = gridLayer.clear()
        )
      }
  }

  private def normalizeLine(line: String, width: Int): String = {
    val truncated = line.take(width)
    truncated + (" " * (width - truncated.length))
  }

  private def renderFrame(
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

  private def sweepEdgeChars(
      toLines: Vector[String],
      revealCol: Int,
      screenWidth: Int,
      screenHeight: Int
  ): Vector[SmoothChar] =
    (0 until screenHeight).map { row =>
      val toLine = normalizeLine(toLines.lift(row).getOrElse(""), screenWidth)
      val col = math.max(0, math.min(revealCol, screenWidth - 1))
      val ch = if (col >= 0 && col < toLine.length) toLine.charAt(col) else ' '
      SmoothChar(ch, col, row)
    }.toVector
}
