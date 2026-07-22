package com.github.morotsman.examples.slides

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.api.support.TickedTransition.ProgressResult
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

import scala.concurrent.duration._

/** A minimal custom transition that replaces each character with a space, one
  * row at a time from top to bottom, then reveals the next slide.
  *
  * This version uses `TickedTransition` to eliminate the manual ticker
  * subscribe/unsubscribe, FixedStep, and Deferred boilerplate.
  *
  * == How it works ==
  *
  *  1. The helper reads both slides' content and drives a progress value
  *     from 0.0 to 1.0 over `totalSteps` simulation steps.
  *
  *  2. Our `renderFrame` function maps progress to "how many rows from the
  *     top show the new slide vs. the old slide".
  *
  *  3. When progress reaches 1.0, the helper automatically completes the
  *     transition and shows the final content.
  *
  * == Usage ==
  * {{{
  * // In a SessionBuilder:
  * .addTextSlide {
  *   _.content("Hello!")
  *     .transition(SimpleWipeTransition.contextual[F]())
  * }
  *
  * // Or with a custom speed:
  * .transition(SimpleWipeTransition.contextual[F](rowsPerStep = 3))
  * }}}
  */
object SimpleWipeTransition {

  /** Wraps construction in a `Contextual` so the transition receives the shared
    * console, ticker, and animation settings from the presentation framework.
    *
    * @param duration
    *   How long the wipe animation should take. Defaults to 400ms.
    */
  def contextual[F[_]: Temporal: Ref.Make](duration: FiniteDuration = 400.millis): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create[F](duration, builder)
    }

  /** Creates the transition directly, useful for standalone examples or tests. */
  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] =
    create(duration, TickedTransition(console, ticker, animationSettings))

  /** Creates the transition from a `TickedTransition.Builder`. */
  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration,
      builder: TickedTransition.Builder[F]
  )(implicit clock: AnimationClock[F]): Transition[F] =
    builder.buildProgress(duration) { (from, to, ctx) =>
      val clearedRows = (ctx.progress * ctx.screenHeight).toInt
      val frame = renderFrame(from, to, clearedRows, ctx.screenHeight)
      val lastDiff = lastDifferingRow(from, to, ctx.screenHeight)
      if (clearedRows > lastDiff) ProgressResult.done(frame)
      else ProgressResult.continue(frame)
    }

  // ── Pure rendering logic ──────────────────────────────────────────

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
    val contentHeight = math.max(fromLines.length, toLines.length)
    val height    = math.min(contentHeight, screenHeight)
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
