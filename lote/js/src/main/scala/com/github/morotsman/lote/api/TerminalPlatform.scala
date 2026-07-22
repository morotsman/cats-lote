package com.github.morotsman.lote.api

import cats.effect.{Async, Resource}
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import com.github.morotsman.lote.internal.interpreter.nconsole.ThreeJsTerminal
import org.scalajs.dom

/** JS-only entry point providing a browser-backed terminal as a `Resource`.
  *
  * Uses '''Three.js / WebGL''' — a hardware-accelerated renderer that draws styled text onto a WebGL canvas via
  * Three.js, enabling 3D spatial navigation and post-processing effects.
  *
  * {{{
  * val container = dom.document.getElementById("terminal").asInstanceOf[dom.HTMLElement]
  * TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal => ... }
  * }}}
  */
object TerminalPlatform {

  /** Creates a Three.js / WebGL-backed `Terminal[F]` wrapped in a `Resource`.
    *
    * Requires Three.js to be available as `window.THREE` (e.g. loaded via CDN). The terminal is rendered as textured
    * planes in a WebGL scene, enabling hardware-accelerated rendering with 3D spatial navigation.
    *
    * @param container
    *   the DOM element where the WebGL canvas will be appended
    * @param config
    *   optional [[WebGLConfig]] to tune clipping planes, field of view, background colour, and other renderer settings.
    *   Uses sensible defaults when omitted.
    */
  def threeJsTerminal[F[_]: Async](
      container: dom.HTMLElement,
      config: WebGLConfig = WebGLConfig()
  ): Resource[F, TerminalAlgebra[F]] =
    ThreeJsTerminal.resource[F](container, config)
}
