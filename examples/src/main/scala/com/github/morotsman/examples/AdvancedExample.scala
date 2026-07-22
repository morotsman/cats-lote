package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.TerminalPlatform

/** The full-featured advanced presentation running in a JVM terminal.
  *
  * Uses `SharedAdvancedPresentation` for all slides. 3D positioning calls (`.at()`, `.rotatedBy()`) are gracefully
  * ignored by the terminal renderer, so the same presentation works in both environments.
  */
object AdvancedExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      SharedAdvancedPresentation
        .build[IO]()
        .run()
    }
  }
}
