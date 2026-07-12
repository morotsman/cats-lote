package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.builders.SessionBuilder

object TransitionsExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide {
        _.content(
          """Transitions are attached to the current slide with builder helpers like `.replaceTransition(...)`.
            |
            |When you leave the slide, that transition decides how the next slide appears,
            |because simply replacing one block of text with another would apparently be too emotionally direct.
            |
            |Leave this slide to see a replace transition.""".stripMargin
        ).title("How Transitions Work")
          .replaceTransition('.')
      }
      .addTextSlide {
        _.content(
          """`ReplaceTransition` is a simple option for sharp visual changes.
            |
            |It is easy to read and works well when you want the audience
            |to notice that the presentation is moving to a new idea instead of quietly drifting there.""".stripMargin
        ).title("Replace Transition")
          .morphTransition()
      }
      .addTextSlide {
        _.content(
          """`MorphTransition` is smoother.
            |
            |It works especially well for text-heavy decks,
            |because the change feels more continuous and slightly less like being hit with a new paragraph.""".stripMargin
        ).title("Morph Transition")
          .fallingCharactersTransition()
      }
      .addTextSlide {
        _.content(
          """`FallingCharactersTransition` is more playful.
            |
            |It is a good reminder that transitions can shape the tone of the deck,
            |not just move you from slide to slide while pretending to be invisible.
            |
            |Next, try CustomTransitionExample to build one of your own, because a stock animation is only so much personality.""".stripMargin
        ).title("Falling Characters")
      }
      .run()
}
