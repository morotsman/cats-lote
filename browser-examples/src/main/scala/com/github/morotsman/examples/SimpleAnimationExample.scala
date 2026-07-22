package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
import org.scalajs.dom

/** A lightweight browser example that runs only the simple animation demos.
  *
  * Uses `SharedSimpleExamplesPresentation` — just 9 slides covering the core
  * animation concepts (counter, glide, scroll, wipe transition). No 3D
  * positioning, no heavy textures, instant startup.
  *
  * To run this example, change `Compile / mainClass` in build.sbt to:
  * {{{
  * Compile / mainClass := Some("com.github.morotsman.examples.SimpleAnimationExample")
  * }}}
  *
  * Then:
  * {{{
  * sbt browserExamples/fastLinkJS
  * cd browser-examples
  * python3 -m http.server 8080
  * }}}
  *
  * Open http://127.0.0.1:8080/simple.html
  */
object SimpleAnimationExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
      SharedSimpleExamplesPresentation
        .build[IO]()
        .withCustomTicker(RafTickerInterpreter.make[IO])
        .run()
    }
  }
}

