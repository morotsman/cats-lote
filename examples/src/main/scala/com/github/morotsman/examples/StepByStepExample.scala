package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.api.builders.SessionBuilder

object StepByStepExample extends IOApp.Simple {

  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      SessionBuilder[IO]()
      .addTextSlide {
        _.content(
          """This example shows a slide that reveals content in stages.
            |
            |That is useful when you want to control pacing
            |without splitting one idea across many separate slides and calling that a narrative structure.""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide {
        _.content(
          """Step-by-step slides use the same builder as ordinary text slides.
            |
            |Start with `content(...)`, then add `step(...)`
            |for each reveal you want to show next, because duplicating whole slides builds character but not efficiency.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide {
        _.content("Step 1: Explain the problem you want to solve.")
          .separator("\n")
          .step("Step 2: Show the smallest useful API.")
          .step("Step 3: Add more detail only when the audience is ready.")
          .hint("[press any key to continue]")
          .title("Step By Step")
      }
      .addTextSlide {
        _.content(
          """The code combines the initial `content(...)`
            |with each later `step(...)`.
            |
            |`separator(...)` controls how the pieces join,
            |and `hint(...)` sets the continue prompt,
            |in case the audience needs help noticing there's more slide coming.""".stripMargin
        ).title("How The Code Works")
      }
      .addTextSlide {
        _.content(
          """This pattern works well for staged reveals, outlines, and teaching slides.
            |
            |AdvancedExample goes one step further with a fully interactive slide, because eventually plain text starts asking for a hobby.""".stripMargin
        ).title("What To Try Next")
      }
      .run()
    }}
