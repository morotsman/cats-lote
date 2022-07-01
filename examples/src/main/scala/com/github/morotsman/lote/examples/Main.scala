package com.github.morotsman.lote.examples

import cats.effect._
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.examples.slides._
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.transition.{MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

import scala.concurrent.duration.DurationInt

object Main extends IOApp.Simple {

  private val moving =
    """
      |this is the first row
      |second row
      |and this is finally the third row
      |""".stripMargin

  override def run(): IO[Unit] = {
    val presentation = PresentationBuilder[IO]()
      .addTextSlide {
        _.content(Start())
          .transition(out = ReplaceTransition(' '))
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(Agenda())
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(DistributedSystem())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(moving)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
      }
      .addExitSlide(Bye[IO]())
      .build()

    for {
      console <- NConsole.make[IO]()
      middleware <- Middleware.make[IO](console)
      timer <- Timer.make[IO](30.minutes, console)
      _ <- middleware.addOverlays(List(
        timer
      ))
      executor <- PresentationExecutorInterpreter.make[IO](middleware, presentation)
      _ <- executor.start()
    } yield ()
  }

}
