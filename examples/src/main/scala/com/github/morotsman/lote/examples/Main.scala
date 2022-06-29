package com.github.morotsman.lote.examples

import cats.effect._
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.examples.slides._
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, MiddlewareState, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.transition.{MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object Main extends IOApp.Simple {

  private val moving =
    """
      |this is the first row
      |second row
      |and this is finally the third row
      |""".stripMargin

  override def run(): IO[Unit] = {
    for {
      middleware <- Ref[IO].of(MiddlewareState[IO](List.empty)).map(s => Middleware.make[IO](s))
      console <- NConsole.make[IO](middleware)
      presentation <- PresentationBuilder[IO](console, middleware)
        .addTextSlide {
          _.content(Start())
            .transition(out = ReplaceTransition(console, ' '))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(Agenda())
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(DistributedSystem())
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Right))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Center))
        }
        .addTextSlide {
          _.content(moving)
            .transition(out = MorphTransition(console))
            .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
        }
        .addExitSlide(Bye(console))
        //.addOverlay(Timer.make[IO]())
        .build()
      executor <- PresentationExecutorInterpreter.make[IO](console, presentation)
      _ <- executor.start()
    } yield ()
  }

}
