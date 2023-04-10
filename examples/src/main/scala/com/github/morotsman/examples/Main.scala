package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.{Agenda, Animator, Bye, ExampleInteractiveSlide, Start}
import com.github.morotsman.lote.algebra.Slide
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter2
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInstances.IONConsole
import com.github.morotsman.lote.interpreter.transition.{FallingCharactersTransition, MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, Presentation, VerticalAlignment}


object Session1 extends IOApp.Simple {

  private val instruction1 =
    """
      |Supports different alignments
      |""".stripMargin

  override def run(): IO[Unit] = {
    def createPresentation(interactiveSlide: Slide[IO]): Presentation[IO] =
      PresentationBuilder[IO]()
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


    for {
      console <- NConsole.make[IO]()
      animator <- Animator.make[IO]()
      interactiveSlide <- ExampleInteractiveSlide.make[IO](animator)
      executor <- PresentationExecutorInterpreter2.make[IO](console, createPresentation(interactiveSlide))
      _ <- executor.start()
    } yield ()
  }

}