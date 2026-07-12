package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.SweepRightTransition
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder

object CustomTransitionExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withFrameRate(60)
      .withAnimationFrameRate(30)
      .addTextSlide {
        _.content(
          """This example introduces a custom transition.
            |
            |A custom transition is just a `Transition[F]` value
            |attached to a slide with `.transition(...)`.
            |
            |Leave this slide to watch a simple wipe reveal the next slide
            |from left to right, because horizontal motion still counts as spectacle in a terminal.""".stripMargin
        ).title("What It Is")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SweepRightTransition.contextual[IO]())
      }
      .addTextSlide {
        _.content(
          """Using one looks almost like using a built-in transition:
            |
            |1. create a `Transition[F]`
            |2. attach it with `.transition(...)`
            |3. let the session timing settings drive the animation
            |
            |This slide leaves with the same custom wipe again,
            |because one dramatic entrance is rarely enough.""".stripMargin
        ).title("How To Use It")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SweepRightTransition.contextual[IO](columnsPerStep = 5))
      }
      .addTextSlide {
        _.content(
          """The `Transition[F]` interface has two jobs:
            |
            |1. `transition(from, to)` renders the animation between slides
            |2. `userInput(input)` can react to input while the transition runs
            |
            |This wipe keeps `userInput` as a no-op,
            |so all of the work happens in `transition(...)`, where the actual showing off lives.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("Transition Interface")
      }
      .addTextSlide {
        _.content(
          """The wipe works in three steps:
            |
            |1. read the current slide content and the next slide content
            |2. build a frame that mixes both slides row by row
            |3. reveal a few more columns on each animation step until the next slide fills the screen
            |
            |That's often enough for a polished text transition,
            |or at least enough to look suspiciously deliberate.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("How The Code Works")
      }
      .addTextSlide {
        _.content(
          """This transition uses the same timing model as the built-in ones:
            |
            |- `withFrameRate(...)` controls how often frames are rendered
            |- `withAnimationFrameRate(...)` controls how quickly the wipe advances
            |- `columnsPerStep` controls how much is revealed per update
            |
            |Those three knobs give you most of the feel of the animation,
            |which is a polite way of saying you can make it tasteful or ridiculous.""".stripMargin
        )
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .title("What To Tweak")
      }
      .run()
}
