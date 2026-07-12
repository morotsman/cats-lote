package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.SessionBuilder
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object AlignmentExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide { _ =>
        _.content(
          """Alignment controls where slide content is placed on the screen.
            |
            |This first slide is aligned to the top-left,
            |which works well for notes, bullet lists, and reading-heavy slides.""".stripMargin
        )
          .title("Top Left")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide { _ =>
        _.content(
          """Centered content works well for splash screens,
            |section breaks, and short messages.
            |
            |It keeps the focus on a small amount of text.""".stripMargin
        )
          .title("Centered")
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addTextSlide { _ =>
        _.content(
          """Bottom-right alignment can be useful for punch lines,
            |quiet status text, or intentionally off-center layouts.
            |
            |It is less common, but it can add variety when used on purpose.""".stripMargin
        )
          .title("Bottom Right")
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
      }
      .addTextSlide { _ =>
        _.content(
          """The main idea is simple:
            |
            |choose the placement that matches how you want the slide to feel.
            |
            |Next, try TransitionsExample to animate the move between slides.""".stripMargin
        )
          .title("How To Choose")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .run()
}
