package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder

object AlignmentExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide {
        _.content(
          """Alignment controls where slide content is placed on the screen.
            |
            |This first slide is aligned to the top-left,
            |which works well for notes, bullet lists, and other situations where you expect people to read on purpose.""".stripMargin
        )
          .title("Top Left")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(
          """Centered content works well for splash screens,
            |section breaks, and short messages.
            |
            |It keeps the focus on a small amount of text,
            |which is fortunate when the slide has only one job.""".stripMargin
        )
          .title("Centered")
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(
          """Bottom-right alignment can be useful for punch lines,
            |quiet status text, or intentionally off-center layouts.
            |
            |It is less common, but it can add variety when used on purpose rather than by accident.""".stripMargin
        )
          .title("Bottom Right")
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content(
          """The main idea is simple:
            |
            |choose the placement that matches how you want the slide to feel,
            |instead of leaving the terminal to make creative decisions on your behalf.
            |
            |Next, try TransitionsExample to animate the move between slides, because apparently static text was not enough.""".stripMargin
        )
          .title("How To Choose")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .run()
}
