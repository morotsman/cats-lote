package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.SweepRightTransition
import com.github.morotsman.lote.builders.SessionBuilder
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object CustomTransitionExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withFrameRate(60)
      .withAnimationFrameRate(30)
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """This example introduces a custom transition.
            |
            |A custom transition is just a `Transition[F]` value
            |attached to a slide with `.transition(...)`.
            |
            |Leave this slide to watch a simple wipe reveal the next slide
            |from left to right.""".stripMargin
        ).title("What It Is")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SweepRightTransition())
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """Using one looks almost exactly like using a built-in transition:
            |
            |1. create a `Transition[F]`
            |2. attach it with `.transition(...)`
            |3. let the session timing settings drive the animation
            |
            |This slide leaves with the same custom wipe again.""".stripMargin
        ).title("How To Use It")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SweepRightTransition(columnsPerStep = 5))
      }
      .addTextSlide { _ =>
        _.content(
          """The `Transition[F]` interface has two jobs:
            |
            |1. `transition(from, to)` renders the animation between slides
            |2. `userInput(input)` can react to input while the transition runs
            |
            |This wipe keeps `userInput` as a no-op,
            |so all of the work happens in `transition(...)`.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("Transition Interface")
      }
      .addTextSlide { _ =>
        _.content(
          """The wipe itself works in three small steps:
            |
            |1. read the current slide content and the next slide content
            |2. build a frame that mixes both slides row by row
            |3. reveal a few more columns on each animation step until the next slide fills the screen
            |
            |That is often enough for a polished text transition.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("How The Code Works")
      }
      .addTextSlide { _ =>
        _.content(
          """This transition uses the same session timing model as the built-in ones:
            |
            |- `withFrameRate(...)` changes how often frames are rendered
            |- `withAnimationFrameRate(...)` changes how quickly the wipe advances
            |- `columnsPerStep` changes how much is revealed per update
            |
            |Those three values give you most of the feel of the animation.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("What To Tweak")
      }
      .run()
}
