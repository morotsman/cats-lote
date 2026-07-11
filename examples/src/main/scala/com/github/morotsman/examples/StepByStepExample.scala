package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.{SessionBuilder, SlideBuilder}
import com.github.morotsman.lote.interpreter.StepByStepSlide

object StepByStepExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide { _ =>
        _.content(
          """This example shows a slide that reveals content in stages.
            |
            |That is useful when you want to control pacing
            |without splitting one idea across many separate slides.""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide { _ =>
        _.content(
          """The example is attached with `addSlideF(...)` instead of `addTextSlide(...)`.
            |
            |Use that form when building the slide needs effects
            |or when you want to insert a custom `Slide[F]`.""".stripMargin
        ).title("How To Use It")
      }
      .addSlideF { implicit ctx =>
        import ctx._
        StepByStepSlide.make[IO](
          Vector(
            "Step 1: Explain the problem you want to solve.",
            "Step 1: Explain the problem you want to solve.\nStep 2: Show the smallest useful API.",
            "Step 1: Explain the problem you want to solve.\nStep 2: Show the smallest useful API.\nStep 3: Add more detail only when the audience is ready."
          )
        ).map { slide =>
          (builder: SlideBuilder[IO, SlideBuilder.WithoutSlide]) =>
            builder.addSlide(slide).title("Step By Step")
        }
      }
      .addTextSlide { _ =>
        _.content(
          """The code works by giving `StepByStepSlide.make(...)`
            |a vector of stages.
            |
            |Each input event advances to the next stage,
            |so the audience sees one idea at a time.""".stripMargin
        ).title("How The Code Works")
      }
      .addTextSlide { _ =>
        _.content(
          """This pattern works well for staged reveals, outlines, and teaching slides.
            |
            |AdvancedExample goes one step further with a fully interactive slide.""".stripMargin
        ).title("What To Try Next")
      }
      .run()
}
