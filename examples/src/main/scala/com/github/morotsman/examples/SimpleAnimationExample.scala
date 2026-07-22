package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.TerminalPlatform

/** Terminal version of the SimpleAnimationExample.
  *
  * Runs the same `SharedSimpleExamplesPresentation` (counter, glide, scroll,
  * wipe transition) but renders to a JLine terminal instead of a WebGL canvas.
  *
  * To run:
  * {{{
  * sbt "examples/runMain com.github.morotsman.examples.SimpleAnimationExample"
  * }}}
  */
object SimpleAnimationExample extends IOApp.Simple {

  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      SharedSimpleExamplesPresentation
        .build[IO]()
        .run()
    }
}

