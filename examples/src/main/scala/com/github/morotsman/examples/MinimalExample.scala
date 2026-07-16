package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.api.builders.SessionBuilder

object MinimalExample extends IOApp.Simple {

  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      SessionBuilder[IO]()
        .addTextSlide {
          _.content("Hello, terminal. Please try to contain your excitement.")
        }
        .addTextSlide {
          _.content("Goodbye. That was the full dramatic arc.")
        }
        .run()
    }
}
