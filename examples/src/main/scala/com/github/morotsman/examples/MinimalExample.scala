package com.github.morotsman.examples

import cats.effect.IO
import com.github.morotsman.lote.api.LoteApp
import com.github.morotsman.lote.api.builders.SessionBuilder

object MinimalExample extends LoteApp {

  def presentation = SessionBuilder[IO]()
    .addTextSlide {
      _.content("Hello, terminal. Please try to contain your excitement.")
    }
    .addTextSlide {
      _.content("Goodbye. That was the full dramatic arc.")
    }
}
