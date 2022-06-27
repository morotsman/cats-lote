package com.github.morotsman.lote.examples

import cats.effect._
import com.github.morotsman.lote.PresentationBuilder
import com.github.morotsman.lote.examples.slides._
import com.github.morotsman.lote.interpreter.NConsoleInstances.IONConsole
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.transition.{MorphTransition, ReplaceTransition}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = {
    val presentation = PresentationBuilder[IO]()
      .addTextSlide {
        _.content(Start()).transition(right = ReplaceTransition(' '))
      }
      .addTextSlide {
        _.content(Agenda()).transition(right = MorphTransition())
      }
      .addTextSlide {
        _.content(DistributedSystem())
      }
      .addExitSlide(Bye())
      .build()

    for {
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- executor.start()
    } yield ()
  }

}
