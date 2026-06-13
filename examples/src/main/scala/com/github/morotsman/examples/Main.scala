package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.{IdleDetector, NConsole, Ticker}
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInterpreter
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Idle, Middleware, ProgressBar, Timer}
import com.github.morotsman.lote.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter}

import scala.concurrent.duration.DurationInt


object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {

    def createMiddleware(console: NConsole[IO]): IO[(NConsole[IO], Ticker[IO], ProgressBar[IO], IdleDetector[IO])] = {
      for {
        ticker <- TickerInterpreter.make[IO]()
        idleDetector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 5.seconds))
        middleware <- Middleware.make[IO](console, ticker, idleDetector)
        timer <- Timer.make[IO](30.minutes)
        progressBar <- ProgressBar.make[IO](14)
        idle <- Idle.make[IO](idleDetector)
        _ <- middleware.addOverlays(List(progressBar, timer, idle))
      } yield (middleware, ticker, progressBar, idleDetector)
    }

    def startPresentation(progressBar: ProgressBar[IO], idleDetector: IdleDetector[IO])(implicit console: NConsole[IO], ticker: Ticker[IO]): IO[Unit] =
      for {
        presentation <- ExamplePresentation.make[IO]()
        executor <- PresentationExecutorInterpreter.make(presentation, { index =>
          progressBar.setCurrentSlide(index) *> idleDetector.notifyActivity()
        })
        _ <- executor.start()
      } yield ()

    NConsoleInterpreter.resource[IO]().use { console =>
      for {
        result <- createMiddleware(console)
        (middleware, ticker, progressBar, idleDetector) = result
        _ <- startPresentation(progressBar, idleDetector)(middleware, ticker)
      } yield ()
    }
  }

}
