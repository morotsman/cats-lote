package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInterpreter
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Idle, IdleConfig, Middleware, ProgressBar, Timer}

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {

    def createMiddleware(): IO[(NConsole[IO], ProgressBar[IO], Idle[IO])] = {
      implicit val console: NConsole[IO] = NConsoleInterpreter.make[IO]()
      for {
        middleware <- Middleware.make[IO]()
        timer <- Timer.make[IO](30.minutes)
        progressBar <- ProgressBar.make[IO](14)
        idle <- Idle.make[IO](IdleConfig(idleTimeout = 5.seconds))
        _ <- middleware.addOverlays(List(progressBar, timer, idle))
      } yield (middleware, progressBar, idle)
    }

    def startPresentation(progressBar: ProgressBar[IO], idle: Idle[IO])(implicit console: NConsole[IO]): IO[Unit] =
      for {
        presentation <- ExamplePresentation.make[IO]()
        executor <- PresentationExecutorInterpreter.make(presentation, { index =>
          progressBar.setCurrentSlide(index) *> idle.notifyActivity()
        })
        _ <- executor.start()
      } yield ()



    for {
      result <- createMiddleware()
      (middleware, progressBar, idle) = result
      _ <- startPresentation(progressBar, idle)(middleware)
    } yield ()
  }

}
