package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.landscape.Landscape3DSlide
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
import org.scalajs.dom

/** The same full-featured presentation as AdvancedExample, rendered via Three.js / WebGL.
  *
  * Uses `SharedAdvancedPresentation` for all common slides and injects the 3D landscape slide which requires a browser
  * environment. A `RafTickerInterpreter` is used instead of the default sleep-based ticker so that all rendering is
  * driven by `requestAnimationFrame` (~60 fps, vsync-aligned).
  */
object AdvancedWebGLExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
      SharedAdvancedPresentation
        .build[IO](landscape3DSlide = Some(Landscape3DSlide.contextual[IO]()))
        .withCustomTicker(RafTickerInterpreter.make[IO]) // TODO maybe it should be picked automatically?
        .run()
    }
  }
}
