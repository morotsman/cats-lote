package com.github.morotsman.examples

import cats.effect.Temporal
import com.github.morotsman.examples.slides.{Agenda, Animator, Bye, ExampleInteractiveSlide, Start}
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.interpreter.transition.{FallingCharactersTransition, MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, Presentation, VerticalAlignment}
import cats.implicits._

object ExamplePresentation {

  private val instruction1 =
    """
      |Supports different alignments
      |""".stripMargin

  def make[F[_]: Temporal: NConsole](): F[Presentation[F]] = {
    def createPresentation(): F[Presentation[F]] = {
      for {
        animator <- Animator.make[F]()
        interactiveSlide <- ExampleInteractiveSlide.make[F](animator)
        presentation = PresentationBuilder[F]()
          .addTextSlide {
            _.content(Start())
              .transition(out = ReplaceTransition(' '))
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content(Agenda())
              .transition(out = FallingCharactersTransition(1.4, 1.3))
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Left))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Right))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Left))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content(instruction1)
              .transition(out = MorphTransition())
              .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
          }
          .addSlide { builder =>
            builder.addSlide(interactiveSlide)
          }
          .addTextSlide {
            _.content(Bye())
              .transition(out = FallingCharactersTransition())
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          }
          .addTextSlide {
            _.content("")
              .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          }
          .build()
      } yield presentation

    }
    createPresentation()
  }


}
