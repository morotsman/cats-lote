package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.SessionBuilder

object MinimalExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide { _ =>
        _.content("Hello, Terminal!")
      }
      .addTextSlide { _ =>
        _.content("Goodbye!")
      }
      .run()
}



