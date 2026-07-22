package com.github.morotsman.lote.api

import cats.effect.Sync
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import com.github.morotsman.lote.internal.interpreter.nconsole.JLineTerminal

/** JVM-only entry point providing a JLine-backed terminal as a `Resource`.
  *
  * Use this in JVM applications to obtain a `Terminal[F]` for `SessionBuilder.run()`:
  *
  * {{{
  * TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
  *   SessionBuilder[IO]()
  *     .addTextSlide(_.content("Hello"))
  *     .run()
  * }
  * }}}
  */
object TerminalPlatform {

  /** Creates a JLine-backed `Terminal[F]` wrapped in a `Resource` for safe lifecycle management. */
  def jlineTerminal[F[_]: Sync](): cats.effect.Resource[F, TerminalAlgebra[F]] =
    JLineTerminal.resource[F]()
}
