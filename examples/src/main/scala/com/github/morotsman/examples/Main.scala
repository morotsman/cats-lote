package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  private val instruction1 =
    """
      |Supports different alignments
      |""".stripMargin

  override def run(): IO[Unit] = {
    implicit val console = NConsole.make[IO]()

    for {
      middleware <- Middleware.make[IO]()
      timer <- Timer.make[IO](30.minutes)
      _ <- middleware.addOverlays(List(timer))
      presentation <- ExamplePresentation.make[IO]()
      executor <- PresentationExecutorInterpreter.make(middleware, presentation)
      _ <- executor.start()
    } yield ()
  }

}
