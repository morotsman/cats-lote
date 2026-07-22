package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{Alignment, Character, HorizontalAlignment, VerticalAlignment}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.TickedSlide
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

/** A minimal interactive slide that keeps a counter on screen.
  *
  * Press `w` to increment and `s` to decrement. The counter re-renders on every
  * ticker tick so the display stays responsive. This is the simplest possible
  * example of a custom `Slide[F]` — no GlideLayer, no FixedStep, no animation
  * loop — just:
  *
  *   - a `Ref` holding the counter value
  *   - a `Ticker` subscription that re-renders when the value changes
  *   - `userInput` to handle key presses
  *   - `NConsole.alignText` to center content (no manual padding math)
  *
  * == How it works ==
  *
  *  1. `content` returns `None` so the framework does not paint a static
  *     background. The slide paints itself.
  *
  *  2. `startShow` subscribes to the shared `Ticker`. On each tick the
  *     slide reads the current count, uses `console.alignText` to center
  *     the text, and writes it to the console.
  *
  *  3. `stopShow` cancels the ticker subscription and resets the state.
  *
  *  4. `userInput` pattern-matches on `Character('w')` and
  *     `Character('s')` to update the counter.
  *
  * == Usage ==
  * {{{
  * SessionBuilder[F]()
  *   .addSlideF {
  *     _.addSlideF(SimpleCounterSlide.contextual[F]())
  *       .map(_.title("Counter Demo"))
  *   }
  * }}}
  */
object SimpleCounterSlide {

  /** Wraps construction in a `ContextualF` so the slide receives the shared
    * console, ticker, and animation settings from the presentation framework.
    */
  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      for {
        countRef <- Ref[F].of(0)
        slide <- builder.build(
          onTick = {
            val centerAlignment = Alignment(
              VerticalAlignment.Center,
              HorizontalAlignment.Center
            )
            countRef.get.flatMap { count =>
              val text =
                s"""Simple Counter Slide
                   |
                   |Count: $count
                   |
                   |[W] increment   [S] decrement""".stripMargin
              builder.console.alignText(text, centerAlignment).flatMap(builder.console.writeString)
            }
          },
          onInput = {
            case Character(c) if c == 'w' => countRef.update(_ + 1)
            case Character(c) if c == 's' => countRef.update(_ - 1)
            case _                        => Monad[F].unit
          },
          onStart = countRef.set(0)
        )
      } yield slide
    }

  /** Creates the slide given an `NConsole` and a `Ticker`.
    *
    * Separated from `contextual` so it can also be used standalone in tests or
    * single-slide runners without the full `SessionBuilder` machinery.
    */
  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F]
  ): F[Slide[F]] =
    for {
      countRef <- Ref[F].of(0)
      slide <- TickedSlide[F](console, ticker).build(
        onTick = {
          val centerAlignment = Alignment(
            VerticalAlignment.Center,
            HorizontalAlignment.Center
          )
          countRef.get.flatMap { count =>
            val text =
              s"""Simple Counter Slide
                 |
                 |Count: $count
                 |
                 |[W] increment   [S] decrement""".stripMargin
            console.alignText(text, centerAlignment).flatMap(console.writeString)
          }
        },
        onInput = {
          case Character(c) if c == 'w' => countRef.update(_ + 1)
          case Character(c) if c == 's' => countRef.update(_ - 1)
          case _                        => Monad[F].unit
        },
        onStart = countRef.set(0)
      )
    } yield slide
}
