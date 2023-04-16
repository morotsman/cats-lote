package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.{Middleware, NConsole}
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {

    def createMiddleware(): IO[NConsole[IO]] = {
      implicit val console: NConsole[IO] = NConsole.make[IO]()
      for {
        middleware <- Middleware.make[IO]()
        timer <- Timer.make[IO](30.minutes)
        _ <- middleware.addOverlays(List(timer))
      } yield middleware
    }

    def startPresentation(implicit console: NConsole[IO]): IO[Unit] =
      for {
        presentation <- ExamplePresentation.make[IO]()
        executor <- PresentationExecutorInterpreter.make(presentation)
        _ <- executor.start()
      } yield ()



    for {
      middleware <- createMiddleware()
      _ <- startPresentation(middleware)
    } yield ()
  }

}
