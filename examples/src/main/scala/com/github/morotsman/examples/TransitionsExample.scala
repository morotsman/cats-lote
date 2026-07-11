package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.SessionBuilder
import com.github.morotsman.lote.interpreter.transition.{FallingCharactersTransition, MorphTransition, ReplaceTransition}

object TransitionsExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """Transitions are attached to the current slide with `.transition(...)`.
            |
            |When you leave the slide, that transition decides how the next slide appears.
            |
            |Leave this slide to see a replace transition.""".stripMargin
        ).title("How Transitions Work")
          .transition(ReplaceTransition('.'))
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """`ReplaceTransition` is a simple option for sharp visual changes.
            |
            |It is easy to read and works well when you want the audience
            |to notice that the presentation is moving to a new idea.""".stripMargin
        ).title("Replace Transition")
          .transition(MorphTransition())
      }
      .addTextSlide { implicit ctx =>
        import ctx._
        _.content(
          """`MorphTransition` is smoother.
            |
            |It works especially well for text-heavy decks,
            |because the change feels more continuous.""".stripMargin
        ).title("Morph Transition")
          .transition(FallingCharactersTransition())
      }
      .addTextSlide { _ =>
        _.content(
          """`FallingCharactersTransition` is more playful.
            |
            |It is a good reminder that transitions can shape the tone of the deck,
            |not just move you from slide to slide.
            |
            |Next, try CustomTransitionExample to build one of your own.""".stripMargin
        ).title("Falling Characters")
      }
      .run()
}
