package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Character, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{AnimationClock, SmoothChar, TickedSlide}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

/** A minimal interactive slide that demonstrates the `GlideLayer` overlay.
  *
  * A single `@` character bounces around the screen. Press `w`/`a`/`s`/`d` to
  * change its direction. On WebGL backends the character glides smoothly
  * between integer cell positions at sub-pixel precision; on terminal backends
  * it moves cell-by-cell (same logic, just no interpolation).
  *
  * This is the simplest possible example of using `GlideLayer` and
  * `SmoothChar`. It adds two things on top of what `SimpleCounterSlide` showed:
  *
  *   - `GlideLayer` — an overlay that automatically interpolates character
  *     positions between simulation steps, giving smooth motion on WebGL.
  *   - `FixedStep` — turns wall-clock time into discrete simulation steps so
  *     the ball moves at a consistent speed regardless of frame rate.
  *
  * == How it works ==
  *
  *  1. On each tick, `FixedStep.consumeSteps` returns how many simulation
  *     steps have elapsed. Each step moves the ball by one cell in its
  *     current direction.
  *
  *  2. The ball's position is stored as integer grid coordinates in a `Ref`.
  *
  *  3. A background frame is rendered as a plain character grid (just a
  *     border and instructions). The ball itself is ''not'' part of this
  *     grid — it is passed to `GlideLayer.renderOnto` as a `SmoothChar`.
  *
  *  4. `renderOnto` handles the platform difference:
  *     - On WebGL: the `SmoothChar` is drawn on a floating overlay with
  *       sub-pixel interpolation; the background frame is returned unchanged.
  *     - On terminal: the `SmoothChar` is composited at integer positions
  *       directly onto the background frame.
  *
  *  5. The resulting frame is written to the console.
  *
  * == Usage ==
  * {{{
  * SessionBuilder[F]()
  *   .addSlideF {
  *     _.addSlideF(SimpleGlideSlide.contextual[F]())
  *       .map(_.title("Glide Demo"))
  *   }
  * }}}
  */
object SimpleGlideSlide {

  /** Wraps construction in a `ContextualF` so the slide receives the shared
    * console, ticker, and animation settings from the presentation framework.
    */
  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      create[F](builder.console, builder.ticker, builder.animationSettings)
    }

  // ── Ball state ───────────────────────────────────────────────────

  private case class Ball(col: Int, row: Int, dCol: Int, dRow: Int)

  /** Creates the slide given console, ticker, and animation settings.
    *
    * Separated from `contextual` so it can be used standalone in tests.
    */
  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): F[Slide[F]] =
    for {
      ballRef <- Ref[F].of(Ball(col = 5, row = 3, dCol = 1, dRow = 1))
      slide <- TickedSlide[F](console, ticker, animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = { (nrOfSteps, glide) =>
            for {
              screen <- console.context
              w = screen.screenWidth
              h = screen.screenHeight
              // Advance the ball by nrOfSteps simulation steps
              _ <- ballRef.update { ball =>
                (0 until nrOfSteps).foldLeft(ball)((b, _) => step(b, w, h))
              }
              ball <- ballRef.get
              // Render the background (without the ball)
              bg = renderBackground(w, h)
              // The ball as a SmoothChar — GlideLayer handles interpolation
              ballChar = Vector(SmoothChar('@', ball.col, ball.row))
              // renderOnto composites the ball onto the background
              frame <- glide.renderOnto(bg, ballChar)
              _ <- console.writeString(frame)
            } yield ()
          },
          onInput = {
            case Character(c) if c == 'w' => ballRef.update(b => b.copy(dRow = clamp(b.dRow - 1)))
            case Character(c) if c == 's' => ballRef.update(b => b.copy(dRow = clamp(b.dRow + 1)))
            case Character(c) if c == 'a' => ballRef.update(b => b.copy(dCol = clamp(b.dCol - 1)))
            case Character(c) if c == 'd' => ballRef.update(b => b.copy(dCol = clamp(b.dCol + 1)))
            case _                        => Monad[F].unit
          },
          onStart = ballRef.set(Ball(col = 5, row = 3, dCol = 1, dRow = 1))
        )
    } yield slide

  private def clamp(v: Int): Int = math.max(-1, math.min(1, v))

  // ── Rendering ────────────────────────────────────────────────────

  /** Render the background grid: a dotted border + instructions. */
  private def renderBackground(width: Int, height: Int): ScreenAdjusted = {
    val lines = (0 until height).map { row =>
      if (row == 0 || row == height - 1) {
        "." * width
      } else if (row == 1) {
        val title = " Simple GlideLayer Demo "
        val pad = math.max(0, (width - title.length) / 2)
        ("." + " " * (pad - 1) + title + " " * math.max(0, width - pad - title.length - 1) + ".").take(width)
      } else if (row == height - 5) {
        val help = " [W/A/S/D] steer ball "
        val pad = math.max(0, (width - help.length) / 2)
        ("." + " " * (pad - 1) + help + " " * math.max(0, width - pad - help.length - 1) + ".").take(width)
      } else {
        "." + " " * math.max(0, width - 2) + "."
      }
    }
    ScreenAdjusted(lines.mkString("\n"))
  }

  // ── Simulation ───────────────────────────────────────────────────

  /** Move the ball one step, bouncing off the borders. */
  private def step(ball: Ball, width: Int, height: Int): Ball = {
    val nextCol = ball.col + ball.dCol
    val nextRow = ball.row + ball.dRow

    val (col2, dCol2) =
      if (nextCol <= 1) (1, 1)
      else if (nextCol >= width - 2) (width - 2, -1)
      else (nextCol, ball.dCol)
    val (row2, dRow2) =
      if (nextRow <= 1) (1, 1)
      else if (nextRow >= height - 2) (height - 2, -1)
      else (nextRow, ball.dRow)

    Ball(col2, row2, dCol2, dRow2)
  }
}

