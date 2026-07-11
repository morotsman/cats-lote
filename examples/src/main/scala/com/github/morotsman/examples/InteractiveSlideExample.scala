package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.{Animator, ExampleInteractiveSlide}
import com.github.morotsman.lote.builders.{SessionBuilder, SlideBuilder}

object InteractiveSlideExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withFrameRate(60)
      .withAnimationFrameRate(25)
      .addTextSlide { _ =>
        _.content(
          """This example focuses on one custom interactive slide.
            |
            |Unlike a text slide, it keeps its own state and reacts to input
            |while it stays on screen.
            |
            |When you reach the interactive slide, try this:
            |  w = up
            |  a = left
            |  s = down
            |  d = right""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide { _ =>
        _.content(
          """A custom `Slide[F]` is added with `addSlide(...)` or `addSlideF(...)`.
            |
             |Use `addSlideF(...)` when building the slide requires effects,
            |such as allocating state, queues, or helper components.
            |
            |This example uses that pattern to create the animator first.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide { _ =>
        _.content(
          """A custom `Slide[F]` usually has four moving parts:
            |
            |- `content` for the initial rendered content
             |- `startShow` runs when the slide becomes active
             |- `stopShow` runs when the slide is left
            |- `userInput` for keys and other events while it is visible""".stripMargin
        ).title("Slide Interface")
      }
      .addTextSlide { _ =>
        _.content(
          """This particular example is a complete interactive demo.
            |
            |The slide forwards WASD input to an animator,
            |and the animator owns the game state and rendering loop.
            |
             |Collect the `?` characters and try not to run into yourself.""".stripMargin
        ).title("How The Code Works")
      }
      .addSlideF { implicit ctx =>
        import ctx._
        Animator.make[IO]().map { animator =>
          (builder: SlideBuilder[IO, SlideBuilder.WithoutSlide]) =>
            builder
              .addSlide(ExampleInteractiveSlide.make[IO](animator))
              .title("WASD Demo")
        }
      }
      .addTextSlide { _ =>
        _.content(
          """If you want to experiment with the feel of the game:
            |
             |- change the frame rate settings at the top of this file
            |- adjust the animator logic in `ExampleInteractiveSlide`
            |- try replacing the game with your own stateful behavior
            |
            |AdvancedExample shows the same idea inside a larger presentation.""".stripMargin
        ).title("What To Tweak")
      }
      .run()
}
