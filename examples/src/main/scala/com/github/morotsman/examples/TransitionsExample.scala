package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.api.builders.SessionBuilder

object TransitionsExample extends IOApp.Simple {

  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
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
            """`ReplaceTransition` is the direct option.
            |
            |It swaps characters with a sharp visual cut,
            |which works well when you want the audience to notice the slide changed
            |instead of quietly drifting into the next one like nothing happened.""".stripMargin
          ).title("Replace Transition")
            .morphTransition()
        }
        .addTextSlide {
          _.content(
            """`MorphTransition` is smoother.
            |
            |It works especially well for text-heavy decks
            |where the change should feel continuous
            |and slightly less like being hit in the face with a new paragraph.""".stripMargin
          ).title("Morph Transition")
            .fallingCharactersTransition()
        }
        .addTextSlide {
          _.content(
            """`FallingCharactersTransition` is more playful.
            |
            |It's a good reminder that transitions shape the tone of your deck,
            |not just ferry people between slides while pretending to be invisible.
            |
            |Next, try CustomTransitionExample to build your own,
            |because stock animations can only carry so much personality.""".stripMargin
          ).title("Falling Characters")
        }
        .run()
    }
}
