package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.api.builders.SessionBuilder
import org.scalajs.dom

object BrowserPresentationExample extends IOApp.Simple {

  val run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.xtermTerminal[IO](container).use { implicit terminal =>
      SessionBuilder[IO]()
        .addTextSlide(
          _.title("Browser Test")
            .content(
              "It works! cats-lote running in a browser via xterm.js.\n\nUse arrow keys to navigate between slides."
            )
        )
        .addTextSlide(
          _.title("Same API")
            .content(
              "This uses the exact same presentation API as the terminal version.\n\nAll slides, transitions, and overlays work identically."
            )
        )
        .addTextSlide(
          _.title("Thank You!")
            .content("Press 'q' to quit.")
        )
        .run()
    }
  }
}
