package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.slides.{Agenda, Animator, Bye, ExampleInteractiveSlide, Start}
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInstances.IONConsole
import com.github.morotsman.lote.interpreter.transition.{FallingCharactersTransition, MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, Presentation, VerticalAlignment}

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  private val instruction1 =
    """
      |Supports different alignments
      |""".stripMargin

  override def run(): IO[Unit] = {
    def createPresentation(console: NConsole[IO], interactiveSlide: Slide[IO]): Presentation[IO] =
      PresentationBuilder[IO](console)
        .addTextSlide {
          _.content(Start())
            .transition(out = ReplaceTransition(console, ' '))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(Agenda())
            .transition(out = FallingCharactersTransition(console, 1.4, 1.3))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Right))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(instruction1)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
        }
        .addSlide { builder =>
          builder.addSlide(interactiveSlide)
        }
        .addTextSlide {
          _.content(Bye())
            .transition(out = FallingCharactersTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content("")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .build()


    for {
      // TODO middleware is not working since I'm not injecting the console in the slides etc :-(
      middleware <- Middleware.make[IO]()
      timer <- Timer.make[IO](30.minutes)
      _ <- middleware.addOverlays(List(
        timer
      ))
      animator <- Animator.make[IO](middleware)
      interactiveSlide <- ExampleInteractiveSlide.make[IO](middleware, animator)
      executor <- PresentationExecutorInterpreter.make[IO](middleware, createPresentation(middleware, interactiveSlide))
      _ <- executor.start()
    } yield ()
  }

}
