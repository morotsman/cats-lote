package com.github.morotsman.examples.slides

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, Easing, TickedTransition}
import com.github.morotsman.lote.api.support.TickedTransition.ProgressResult
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

import scala.concurrent.duration._

/** Demonstrates `TickedTransition.withEasing` — the same wipe logic as
  * `SimpleWipeTransition` but with an ease-in-out-cubic curve applied.
  *
  * The wipe starts slowly, accelerates through the middle, and decelerates
  * at the end — producing a more polished feel with zero extra rendering code.
  *
  * == Key concept ==
  * {{{
  * builder
  *   .withEasing(Easing.easeInOutCubic)
  *   .buildProgress(duration) { (from, to, ctx) =>
  *     val frame = renderFrame(from, to, ctx.progress, ctx.screenHeight)
  *     if (visuallyDone) ProgressResult.done(frame)
  *     else              ProgressResult.continue(frame)
  *   }
  * }}}
  *
  * == Usage ==
  * {{{
  * .transition(SimpleEasedWipeTransition.contextual[F]())
  * }}}
  */
object SimpleEasedWipeTransition {

  def contextual[F[_]: Temporal: Ref.Make](duration: FiniteDuration = 5000.millis): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create[F](duration, builder)
    }

  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] =
    create(duration, TickedTransition(console, ticker, animationSettings))

  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration,
      builder: TickedTransition.Builder[F]
  )(implicit clock: AnimationClock[F]): Transition[F] =
    builder
      .withEasing(Easing.easeInOutCubic)
      .buildProgress(duration) { (from, to, ctx) =>
        // progress maps to the FULL screen height (duration = time for full-screen wipe).
        // This means short content finishes proportionally earlier.
        val clearedRows = (ctx.progress * ctx.screenHeight).toInt
        val frame = renderFrame(from, to, clearedRows, ctx.screenHeight)
        // Find the last row that actually differs between from and to.
        // Once the wipe passes that row, nothing visible changes — we're done.
        val lastDiff = lastDifferingRow(from, to, ctx.screenHeight)
        if (clearedRows > lastDiff) ProgressResult.done(frame)
        else ProgressResult.continue(frame)
      }

  /** Returns the index of the last row where from and to visually differ,
    * or -1 if they are identical (transition completes immediately).
    */
  private def lastDifferingRow(
      from: ScreenAdjusted,
      to: ScreenAdjusted,
      screenHeight: Int
  ): Int = {
    val fromLines = from.content.split("\n", -1).toVector
    val toLines   = to.content.split("\n", -1).toVector
    val height = math.min(math.max(fromLines.length, toLines.length), screenHeight)
    (0 until height).reverse
      .find { row =>
        fromLines.lift(row).getOrElse("") != toLines.lift(row).getOrElse("")
      }
      .getOrElse(-1)
  }

  private def renderFrame(
      from: ScreenAdjusted,
      to: ScreenAdjusted,
      clearedRows: Int,
      screenHeight: Int
  ): ScreenAdjusted = {
    val fromLines = from.content.split("\n", -1).toVector
    val toLines   = to.content.split("\n", -1).toVector
    val contentHt = math.max(fromLines.length, toLines.length)
    val height    = math.min(contentHt, screenHeight)
    val width = math.max(
      fromLines.map(_.length).maxOption.getOrElse(0),
      toLines.map(_.length).maxOption.getOrElse(0)
    )

    ScreenAdjusted(
      (0 until height)
        .map { row =>
          if (row < clearedRows)
            padLine(toLines.lift(row).getOrElse(""), width)
          else
            padLine(fromLines.lift(row).getOrElse(""), width)
        }
        .mkString("\n")
    )
  }

  private def padLine(line: String, width: Int): String = {
    val truncated = line.take(width)
    truncated + (" " * (width - truncated.length))
  }
}

