package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.ExampleInteractiveSlide
import com.github.morotsman.lote.api.builders.SessionBuilder

object InteractiveSlideExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withFrameRate(60)
      .withAnimationFrameRate(25)
      .addTextSlide {
        _.content(
          """This example focuses on one custom interactive slide.
            |
            |Unlike a text slide, it keeps its own state and reacts to input
            |while it stays on screen, which is how you know things have escalated.
            |
            |When you reach the interactive slide, try this:
            |  w = up
            |  a = left
            |  s = down
            |  d = right""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide {
        _.content(
          """A custom `Slide[F]` is added with `addSlide(...)` or `addSlideF(...)`.
            |
            |Use `addSlideF(...)` when building the slide requires effects —
            |allocating state, queues, or helper components.
            |
            |This example uses that pattern to create the animator first,
            |because stateful behavior rarely assembles itself out of goodwill.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide {
        _.content(
          """A custom `Slide[F]` has four moving parts:
            |
            |- `content` for the rendered output
            |- `startShow` runs when the slide becomes active
            |- `stopShow` runs when the slide is left
            |- `userInput` handles keys and other events
            |
            |In other words, the slide gets a lifecycle,
            |which is a very serious thing to give a terminal toy.""".stripMargin
        ).title("Slide Interface")
      }
      .addTextSlide {
        _.content(
          """This particular example is a complete interactive demo.
            |
            |The slide forwards WASD input to an animator,
            |and the animator owns the game state and rendering loop.
            |
            |Collect the `?` characters and try not to run into yourself,
            |which remains good advice even outside software.""".stripMargin
        ).title("How The Code Works")
      }
      .addSlideF { builder =>
        builder
          .addSlideF(ExampleInteractiveSlide.contextual[IO]())
          .map(_.title("WASD Demo"))
      }
      .addTextSlide {
        _.content(
          """If you want to experiment with the feel of the game:
            |
            |- change the frame rate settings at the top of this file
            |- adjust the animator logic in `ExampleInteractiveSlide`
            |- try replacing the game with your own stateful behavior
            |
            |AdvancedExample shows the same idea inside a larger presentation,
            |because apparently one tiny terminal game was only the beginning.""".stripMargin
        ).title("What To Tweak")
      }
      .run()
}
