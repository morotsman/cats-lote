package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.LoteApp

/** Terminal version of the SimpleAnimationExample.
  *
  * Runs the same `SharedSimpleExamplesPresentation` (counter, glide, scroll, wipe transition) but renders to a JLine
  * terminal instead of a WebGL canvas.
  *
  * To run:
  * {{{
  * sbt "examples/runMain com.github.morotsman.examples.SimpleAnimationExample"
  * }}}
  */
object SimpleAnimationExample extends LoteApp {

  def presentation = SharedSimpleExamplesPresentation.build[IO]()
}
