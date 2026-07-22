package com.github.morotsman.examples.slides

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, SmoothChar, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

/** Demonstrates `TickedTransition.buildProgressWithGlide` — a transition that uses a `GlideLayer.Overlay` for smooth
  * sub-pixel character animation at the transition edge.
  *
  * This is a left-to-right sweep (like `SimpleSweepTransition`) but with the leading edge characters rendered via the
  * GlideLayer overlay. On WebGL this produces buttery-smooth interpolation; on terminal it gracefully falls back to
  * integer grid positions.
  *
  * ==Key concept==
  * {{{
  * builder
  *   .withGlideLayer(wrapThreshold = columnsPerStep)
  *   .buildProgressWithGlide(totalSteps) { (from, to, progress, glide) =>
  *     val frame = renderBackground(from, to, progress)
  *     // Render edge chars via glide overlay for sub-pixel smoothness
  *     glide.renderOnto(frame, edgeChars)
  *   }
  * }}}
  *
  * ==Usage==
  * {{{
  * .transition(SimpleSweepGlideTransition.contextual[F]())
  * }}}
  */
object SimpleSweepGlideTransition {

  def contextual[F[_]: Temporal: Ref.Make](columnsPerStep: Int = 3): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create[F](columnsPerStep, builder)
    }

  def create[F[_]: Temporal: Ref.Make](
      columnsPerStep: Int,
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

    // Use buildWithSetup so we can compute totalSteps from actual content width
    builder
      .withGlideLayer(wrapThreshold = columnsPerStep)
      .buildWithSetup { (from, to, complete) =>
        import cats.syntax.all._
        val fromLines = from.content.split("\n", -1).toVector
        val toLines = to.content.split("\n", -1).toVector
        val height = math.max(fromLines.length, toLines.length)
        val contentWidth = math.max(
          fromLines.map(_.length).maxOption.getOrElse(0),
          toLines.map(_.length).maxOption.getOrElse(0)
        )

        for {
          glide <- com.github.morotsman.lote.api.support.GlideLayer.make[F](
            builder.console,
            builder.animationSettings.step,
            columnsPerStep
          )
          revealRef <- cats.effect.Ref[F].of(0)
        } yield TickedTransition.TickHandler(
          onTick = (nrOfSteps: Int, _: Double) => {
            for {
              revealCol <- revealRef.updateAndGet(cur => math.min(contentWidth, cur + nrOfSteps * columnsPerStep))
              backgroundFrame = ScreenAdjusted(
                (0 until height)
                  .map { row =>
                    val fromLine = padLine(fromLines.lift(row).getOrElse(""), contentWidth)
                    val toLine = padLine(toLines.lift(row).getOrElse(""), contentWidth)
                    toLine.take(revealCol) + fromLine.drop(revealCol)
                  }
                  .mkString("\n")
              )
              edgeChars =
                if (revealCol < contentWidth) {
                  (0 until height).map { row =>
                    val toLine = padLine(toLines.lift(row).getOrElse(""), contentWidth)
                    val col = math.min(revealCol, contentWidth - 1)
                    val ch = if (col < toLine.length) toLine.charAt(col) else ' '
                    SmoothChar(ch, col, row)
                  }.toVector
                } else Vector.empty
              rendered <-
                if (edgeChars.nonEmpty) glide.renderOnto(backgroundFrame, edgeChars)
                else glide.clear().as(backgroundFrame)
              _ <- builder.console.clear()
              _ <- builder.console.writeString(rendered)
              _ <- if (revealCol >= contentWidth) complete else cats.Monad[F].unit
            } yield ()
          },
          cleanup = glide.clear()
        )
      }
  }

  private def padLine(line: String, width: Int): String = {
    val truncated = line.take(width)
    truncated + (" " * (width - truncated.length))
  }
}
