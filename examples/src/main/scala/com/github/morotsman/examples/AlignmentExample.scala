package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder

object AlignmentExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide {
        _.content(
          """Alignment controls where slide content lands on the screen.
            |
            |This first slide is aligned to the top-left,
            |which works well for bullet lists, notes, and other content that assumes people read on purpose.""".stripMargin
        )
          .title("Top Left")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(
          """Centered content works well for splash screens,
            |section breaks, and short messages that want to feel important.
            |
            |It keeps the focus on a small amount of text,
            |which is convenient when your slide's entire job is one sentence and a dramatic pause.""".stripMargin
        )
          .title("Centered")
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(
          """Bottom-right alignment is for punch lines,
            |quiet status text, or intentionally off-center layouts.
            |
            |Less common, but it can add variety
            |when used on purpose rather than because someone forgot to set alignment at all.""".stripMargin
        )
          .title("Bottom Right")
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content(
          """The main idea:
            |
            |pick the placement that matches how you want the slide to feel,
            |instead of letting the terminal make creative decisions on your behalf.
            |
            |Next, try TransitionsExample to animate between slides,
            |because apparently static text sitting in the right place was not enough.""".stripMargin
        )
          .title("How To Choose")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .run()
}
