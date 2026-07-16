package com.github.morotsman.lote.api

import cats.effect.{Async, Resource}
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import com.github.morotsman.lote.internal.interpreter.nconsole.{XtermTerminal, ThreeJsTerminal}
import org.scalajs.dom

/** JS-only entry point providing browser-backed terminals as `Resource`s.
  *
  * Two backends are available:
  *
  *  - '''xterm.js''' — lightweight terminal emulator that natively interprets ANSI sequences.
  *  - '''Three.js / WebGL''' — hardware-accelerated renderer that draws styled text onto a
  *    WebGL canvas via Three.js, enabling 3D effects and post-processing.
  *
  * {{{
  * val container = dom.document.getElementById("terminal").asInstanceOf[dom.HTMLElement]
  *
  * // xterm.js backend
  * TerminalPlatform.xtermTerminal[IO](container).use { implicit terminal => ... }
  *
  * // Three.js / WebGL backend
  * TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal => ... }
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

  /** Creates a Three.js / WebGL-backed `Terminal[F]` wrapped in a `Resource`.
    *
    * Requires Three.js to be available as `window.THREE` (e.g. loaded via CDN).
    * The terminal is rendered as a textured plane in a WebGL scene, enabling
    * hardware-accelerated rendering and potential 3D visual effects.
    *
    * @param container
    *   the DOM element where the WebGL canvas will be appended
    */
  def threeJsTerminal[F[_]: Async](container: dom.HTMLElement): Resource[F, TerminalAlgebra[F]] =
    ThreeJsTerminal.resource[F](container)
}

