package com.github.morotsman.lote.api

import cats.effect.{Async, Resource}
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import com.github.morotsman.lote.internal.interpreter.nconsole.XtermTerminal
import org.scalajs.dom

/** JS-only entry point providing an xterm.js-backed terminal as a `Resource`.
  *
  * Use this in Scala.js browser applications to obtain a `Terminal[F]` for `SessionBuilder.run()`:
  *
  * {{{
  * val container = dom.document.getElementById("terminal").asInstanceOf[dom.HTMLElement]
  * TerminalPlatform.xtermTerminal[IO](container).use { implicit terminal =>
  *   SessionBuilder[IO]()
  *     .addTextSlide(_.content("Hello from the browser!"))
  *     .run()
  * }
  * }}}
  */
object TerminalPlatform {

  /** Creates an xterm.js-backed `Terminal[F]` wrapped in a `Resource` for safe lifecycle management.
    *
    * @param container
    *   the DOM element where the terminal will be rendered
    */
  def xtermTerminal[F[_]: Async](container: dom.HTMLElement): Resource[F, TerminalAlgebra[F]] =
    XtermTerminal.resource[F](container)
}

