package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.algebra.{NConsole, Ticker}
import com.github.morotsman.lote.interpreter.nconsole.{IdleAwareNConsole, NConsoleInterpreter}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Idle, Middleware, NavigationSubscriber, ProgressBar, QuickNavigation, Timer}
import com.github.morotsman.lote.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter}

import scala.concurrent.duration.DurationInt

object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {
    NConsoleInterpreter.resource[IO]().use { rawConsole =>
      for {
        ticker <- TickerInterpreter.make[IO]()
        idleDetector <- IdleDetectorInterpreter.make[IO](
          IdleDetectorConfig(idleTimeout = 2.minutes)
        )
        console = IdleAwareNConsole.wrap[IO](rawConsole, idleDetector)

        // add middleware
        timer <- Timer.make[IO](30.minutes)
        progressBar <- ProgressBar.make[IO](14)
        quickNavigation <- QuickNavigation.make[IO]()
        idle <- Idle.make[IO](idleDetector)
        consoleWithMiddleware <- Middleware.make[IO](console, ticker)
        _ <- consoleWithMiddleware.addOverlays(
          List(progressBar, timer, quickNavigation, idle)
        )

        // start presentation
        implicit0(c: NConsole[IO]) = consoleWithMiddleware
        implicit0(t: Ticker[IO]) = ticker
        presentation <- ExamplePresentation.make[IO]()
        _ <- quickNavigation.setTiles(presentation.titles)
        executor <- PresentationExecutorInterpreter.make(
          presentation,
          { index =>
            progressBar.setCurrentSlide(index) *> idleDetector.notifyActivity()
          }
        )
        _ <- quickNavigation.subscribe(NavigationSubscriber(id = 1L, callback = index => executor.setSlide(index)))
        _ <- executor.start()
      } yield ()
    }
  }

}
