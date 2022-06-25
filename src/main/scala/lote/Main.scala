package com.github.morotsman.lote

import cats.effect._
import com.github.morotsman.lote.slides.{Agenda, DistributedSystem, Start}
import com.github.morotsman.lote.interpreter.NConsoleInstances.IONConsole
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.transition.{MorphTransition, ReplaceTransition}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = {
    val presentation = PresentationBuilder[IO]()
      .addSlide(Start())
      .addTransitions(right = ReplaceTransition(' '))
      .addSlide(Agenda())
      .addTransitions(right = MorphTransition())
      .addSlide(DistributedSystem())
      .build()

    for {
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- executor.start()
    } yield ()
  }

}
