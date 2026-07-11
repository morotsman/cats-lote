package com.github.morotsman.examples.slides

import cats.{Applicative, Monad}
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.model.{Character, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}

final case class InputStatus(count: Int, lastInput: String)

object InputStatusOverlay {

  /** Example effectfully-constructed `Overlay[F]`.
    *
    * `make` returns `F[Overlay[F]]` instead of `Overlay[F]` because the overlay allocates a `Ref` to keep state. The
    * state is updated from `onUserInput(...)` and then rendered into the top line from `applyOverlay(...)`.
    */

  def make[F[_]: Monad: Ref.Make](): F[Overlay[F]] =
    Ref[F].of(InputStatus(count = 0, lastInput = "none")).map { stateRef =>
      new Overlay[F] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): F[ScreenAdjusted] = {
          val _ = originalContent

          def fit(line: String, width: Int): String = {
            val truncated = line.take(width)
            truncated + (" " * (width - truncated.length))
          }

          for {
            state <- stateRef.get
          } yield {
            val width = context.screenWidth
            val height = context.screenHeight
            val statusLine = fit(
              s"[input overlay] keys seen: ${state.count} | last input: ${state.lastInput}",
              width
            )
            val lines = screenAdjusted.content.split("\n", -1).toVector
            val paddedLines =
              if (lines.length >= height) lines.take(height)
              else lines ++ Vector.fill(height - lines.length)(" " * width)

            screenAdjusted.copy(content = paddedLines.updated(0, statusLine).mkString("\n"))
          }
        }

        override def onUserInput(userInput: UserInput)(implicit F: Applicative[F]): F[Unit] = {
          val _ = F
          val label = userInput match {
            case Character(c)          => s"character '$c'"
            case Key(SpecialKey.Enter) => "Enter"
            case Key(SpecialKey.Up)    => "Up"
            case Key(SpecialKey.Down)  => "Down"
            case Key(SpecialKey.Left)  => "Left"
            case Key(SpecialKey.Right) => "Right"
            case Key(SpecialKey.Space) => "Space"
            case Key(SpecialKey.Esc)   => "Esc"
            case Key(SpecialKey.Timeout) => "Timeout"
            case Key(other)            => other.toString
            case _                     => userInput.toString
          }

          stateRef.update(state =>
            state.copy(
              count = state.count + 1,
              lastInput = label
            )
          )
        }
      }
    }
}

