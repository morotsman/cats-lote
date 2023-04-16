package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.model.Presentation

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {

    def createPresentation(implicit console: NConsole[IO]): IO[Presentation[IO]] =
      ExamplePresentation.make[IO]()

    implicit val console: NConsole[IO] = NConsole.make[IO]()

    for {
      middleware <- Middleware.make[IO]()
      timer <- Timer.make[IO](30.minutes)
      _ <- middleware.addOverlays(List(timer))
      presentation <- createPresentation(middleware)
      executor <- PresentationExecutorInterpreter.make(presentation)
      _ <- executor.start()
    } yield ()
  }

}
