package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.LoteApp

/** A lightweight browser example that runs only the simple animation demos.
  *
  * Uses `SharedSimpleExamplesPresentation` — just 9 slides covering the core animation concepts (counter, glide,
  * scroll, wipe transition). No 3D positioning, no heavy textures, instant startup.
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
object SimpleAnimationExample extends LoteApp {

  def presentation = SharedSimpleExamplesPresentation.build[IO]()
}
