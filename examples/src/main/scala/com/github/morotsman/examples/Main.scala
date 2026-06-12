package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.{NConsole, Ticker}
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInterpreter
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Idle, IdleConfig, Middleware, ProgressBar, Timer}

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {

    def createMiddleware(): IO[(NConsole[IO], Ticker[IO], ProgressBar[IO], Idle[IO])] = {
      implicit val console: NConsole[IO] = NConsoleInterpreter.make[IO]()
      for {
        ticker <- TickerInterpreter.make[IO]()
        middleware <- {
          implicit val t: Ticker[IO] = ticker
          Middleware.make[IO]()
        }
        timer <- Timer.make[IO](30.minutes)
        progressBar <- ProgressBar.make[IO](14)
        idle <- Idle.make[IO](IdleConfig(idleTimeout = 5.seconds))
        _ <- middleware.addOverlays(List(progressBar, timer, idle))
      } yield (middleware, ticker, progressBar, idle)
    }

    def startPresentation(progressBar: ProgressBar[IO], idle: Idle[IO])(implicit console: NConsole[IO], ticker: Ticker[IO]): IO[Unit] =
      for {
        presentation <- ExamplePresentation.make[IO]()
        executor <- PresentationExecutorInterpreter.make(presentation, { index =>
          progressBar.setCurrentSlide(index) *> idle.notifyActivity()
        })
        _ <- executor.start()
      } yield ()



    for {
      result <- createMiddleware()
      (middleware, ticker, progressBar, idle) = result
      _ <- startPresentation(progressBar, idle)(middleware, ticker)
    } yield ()
  }

}
