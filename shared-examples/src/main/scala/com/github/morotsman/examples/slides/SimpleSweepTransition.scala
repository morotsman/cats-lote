package com.github.morotsman.examples.slides

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.api.support.TickedTransition.ProgressResult
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

import scala.concurrent.duration._

/** Simplified version of `SweepRightTransition` using `TickedTransition`.
  *
  * Compare this (~40 lines) with the original SweepRightTransition (~147 lines). The ticker subscribe/unsubscribe,
  * FixedStep, Deferred, and lifecycle management are all handled by the helper.
  */
object SimpleSweepTransition {

  def contextual[F[_]: Temporal: Ref.Make](duration: FiniteDuration = 500.millis): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create(duration, builder)
    }

  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration = 500.millis,
      builder: TickedTransition.Builder[F]
  )(implicit clock: AnimationClock[F]): Transition[F] =
    builder.buildProgress(duration) { (from, to, ctx) =>
      val revealColumns = (ctx.progress * ctx.screenWidth).toInt
      val frame = blendFrame(from, to, revealColumns, ctx.screenWidth)
      val lastDiff = lastDifferingColumn(from, to, ctx.screenWidth)
      if (revealColumns > lastDiff) ProgressResult.done(frame)
      else ProgressResult.continue(frame)
    }

  def create[F[_]: Temporal: Ref.Make](
      duration: FiniteDuration,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] =
    create(duration, TickedTransition(console, ticker, animationSettings))

  /** Returns the index of the last column where from and to differ on any row, or -1 if they are identical.
    */
  private def lastDifferingColumn(
      from: ScreenAdjusted,
      to: ScreenAdjusted,
      screenWidth: Int
  ): Int = {
    val fromLines = from.content.split("\n", -1).toVector
    val toLines = to.content.split("\n", -1).toVector
    val height = math.max(fromLines.length, toLines.length)
    val width = math.min(
      math.max(
        fromLines.map(_.length).maxOption.getOrElse(0),
        toLines.map(_.length).maxOption.getOrElse(0)
      ),
      screenWidth
    )
    (0 until width).reverse
      .find { col =>
        (0 until height).exists { row =>
          val fc = fromLines.lift(row).flatMap(l => l.lift(col))
          val tc = toLines.lift(row).flatMap(l => l.lift(col))
          fc != tc
        }
      }
      .getOrElse(-1)
  }

  private def blendFrame(
      from: ScreenAdjusted,
      to: ScreenAdjusted,
      revealColumns: Int,
      screenWidth: Int
  ): ScreenAdjusted = {
    val fromLines = from.content.split("\n", -1).toVector
    val toLines = to.content.split("\n", -1).toVector
    val height = math.max(fromLines.length, toLines.length)
    val contentWidth = math.max(
      fromLines.map(_.length).maxOption.getOrElse(0),
      toLines.map(_.length).maxOption.getOrElse(0)
    )
    val width = math.min(contentWidth, screenWidth)

    ScreenAdjusted(
      (0 until height)
        .map { row =>
          val fromLine = padLine(fromLines.lift(row).getOrElse(""), width)
          val toLine = padLine(toLines.lift(row).getOrElse(""), width)
          toLine.take(revealColumns) + fromLine.drop(revealColumns)
        }
        .mkString("\n")
    )
  }

  private def padLine(line: String, width: Int): String = {
    val truncated = line.take(width)
    truncated + (" " * (width - truncated.length))
  }
}
