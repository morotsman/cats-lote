package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.slides.{Animator, Bye, ExampleInteractiveSlide, Start, SweepRightTransition}
import com.github.morotsman.lote.builders.{SessionBuilder, SlideBuilder}
import com.github.morotsman.lote.interpreter.StepByStepSlide
import com.github.morotsman.lote.interpreter.transition.{
  FallingCharactersTransition,
  GrabTransition,
  MorphTransition,
  ReplaceTransition
}
import com.github.morotsman.lote.interpreter.middleware.Milestone
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

import scala.concurrent.duration.DurationInt

object AdvancedExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    SessionBuilder[IO]()
      .withTimer(30.minutes)
      .withProgressBar(List(
        Milestone("Start", 0),
        Milestone("Overlays", 1),
        Milestone("Titles", 6),
        Milestone("Alignment", 7),
        Milestone("Transitions", 8),
        Milestone("Custom", 12),
        Milestone("Interactive", 15),
        Milestone("Bye", 17)
      ))
      .withQuickNavigation()
      .withIdleAnimation(idleTimeout = 5.minutes)
      .withFrameRate(60)
      .withAnimationFrameRate(25)
      .addTextSlide { _ =>
        _.content(Start())
          .title("Start")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |An overlay adds information around the slide content
            |without changing the slide itself.
            |
            |That makes overlays a good fit for things like:
            |- timing
            |- navigation
            |- progress indicators
            |- decorations or status information
            |
            |This deck uses several built-in overlays,
            |and the next few slides explain them one by one.""".stripMargin
        ).title("Overlays")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |The timer overlay shows how much presentation time is left.
            |
            |It is useful when:
            |- you have a fixed speaking slot
            |- you want a steady pace without checking a separate clock
            |
            |In this deck, the timer is enabled once on the session
            |and stays visible across all slides.""".stripMargin
        ).title("Timer Overlay")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |The progress bar overlay shows where you are in the deck.
            |
            |Milestones add section labels to that bar,
            |so the audience can tell both:
            |- how far through the presentation you are
            |- which section you are currently in
            |
            |This is especially helpful in longer presentations.""".stripMargin
        ).title("Progress Bar Overlay")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |Quick navigation gives you a menu of slides
            |so you can jump directly to another part of the deck.
            |
            |Press `N` to open it.
            |Use `↑` / `↓` to move, `Enter` to jump,
            |and `N` again to close it.
            |
            |It becomes much more useful once your slides have titles,
            |which is the next feature in this walkthrough.""".stripMargin
        ).title("Quick Navigation Overlay")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |Idle animation adds motion when the presentation sits still
            |for a while without input.
            |
            |It can help a paused deck feel less frozen,
            |which is useful during demos, questions, or breaks.
            |
            |In this deck, it starts after 5 minutes of inactivity.""".stripMargin
        ).title("Idle Animation Overlay")
      }
      .addTextSlide { _ =>
        _.content(
          """
            |Titles give each slide a stable name.
            |They are especially useful when you enable quick navigation,
            |because the menu becomes much easier to scan.
            |
            |Press `N` to open quick navigation.
            |Use `↑` / `↓` to move, `Enter` to jump,
            |and `N` again to close the menu.""".stripMargin
        ).title("Titles")
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """
            |Alignment controls where content appears on the screen.
            |That gives you an easy way to make a splash screen,
            |a note, or a summary feel deliberately placed.
            |
            |This is also the first slide that defines a transition,
            |so the next slide will be the first one where you see a transition in action.""".stripMargin
        ).title("Alignment")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(ReplaceTransition(' '))
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """
            |The transition that brought you here was `ReplaceTransition`.
            |
            |Use it when you want a direct change from one slide to the next.
            |
            |Leave this slide to see `MorphTransition` next.""".stripMargin
        ).title("ReplaceTransition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(MorphTransition())
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """
            |The transition that brought you here was `MorphTransition`.
            |
            |Use it when the next slide builds naturally on the current one
            |and you want the text to change more smoothly.
            |
            |Leave this slide to see `FallingCharactersTransition` next.""".stripMargin
        ).title("MorphTransition")
          .transition(FallingCharactersTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """
            |The transition that brought you here was `FallingCharactersTransition`.
            |
            |Use it when you want a playful or dramatic way to clear the screen.
            |
            |Leave this slide to see `GrabTransition` next.""".stripMargin
        ).title("FallingCharactersTransition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(GrabTransition())
      }
      .addTextSlide { _ =>
        _.content(
          """
            |The transition that brought you here was `GrabTransition`.
            |
            |Use it when you want a transition with a strong visual personality.
            |
            |That completes the built-in transition tour.""".stripMargin
        ).title("GrabTransition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide { _ =>
        _.content(
          """
          |You can also create custom overlays:

            |- In `CustomOverlayExample` we show a simple overlay that adds the same decoration to every slide
            |
            |- In `EffectfulOverlayExample` we show a stateful overlay that updates while the slide stays on screen
            """.stripMargin
        ).title("Custom Overlays")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """
          |You can also create your own transitions.

            |This slide leaves with a custom left-to-right wipe,
            |showing that your own transitions can plug in just like the built-in ones.
            |
            |If you want to study it in isolation, run `CustomTransitionExample`.""".stripMargin
        ).title("Custom Transition")
          .transition(SweepRightTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addSlideF { implicit ctx =>
        import ctx._
        for {
          slide <- StepByStepSlide.make[IO](
            Vector(
              "The library has support for slides that gradually relevel it's content",

              "Step 1: Start with the smallest possible text slide.",
              "Step 1: Start with the smallest possible text slide.\nStep 2: Add titles, alignment, transitions, and overlays when they help the audience.",
              "Step 1: Start with the smallest possible text slide.\nStep 2: Add titles, alignment, transitions, and overlays when they help the audience.\nStep 3: Reach for custom slides when you want different behavior, like step-by-step reveal."
            )
          )
        } yield {
          (builder: SlideBuilder[IO, SlideBuilder.WithoutSlide]) =>
            builder
              .addSlide(slide)
              .title("Step By Step")
              .transition(MorphTransition())
        }
      }
      .addTextSlide { _ =>
        _.content(
          """

          |The next slide is different from the text and step-by-step slides.

            |It is a custom slide with its own state, its own input handling,
            |and its own rendering behavior.

            |In this demo:
            |- use w, a, s, and d to move
            |- collect the ? characters
            |- avoid running into yourself
            |
            |If you want this concept in isolation, run InteractiveSlideExample.""".stripMargin
        ).title("Introducing Interactive Slides")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addSlideF { implicit ctx =>
        import ctx._
        for {
          animator <- Animator.make[IO]()
        } yield { (builder: SlideBuilder[IO, SlideBuilder.WithoutSlide]) =>
          val slide = ExampleInteractiveSlide.make[IO](animator)
          builder
            .addSlide(slide)
            .title("Interactive")
        }
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          Bye() +
            """

              |Where to go next:
              |
              |Try the smaller examples when you want one concept at a time,
              |and come back to AdvancedExample when you want the full overview.
              |""".stripMargin
        ).title("Bye")
          .transition(FallingCharactersTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .run()
  }

}

