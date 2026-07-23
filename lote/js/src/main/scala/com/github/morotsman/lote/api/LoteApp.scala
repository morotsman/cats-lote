package com.github.morotsman.lote.api

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.builders.SessionBuilder
import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
import org.scalajs.dom

/** A convenience entry point that eliminates IOApp / terminal-resource boilerplate.
  *
  * Instead of wiring `IOApp.Simple`, `TerminalPlatform`, and `Resource.use` manually, extend `LoteApp` and implement
  * `presentation`:
  *
  * {{{
  * object MyPresentation extends LoteApp {
  *   def presentation = SessionBuilder[IO]()
  *     .addTextSlide(_.content("Hello!"))
  *     .addTextSlide(_.content("Goodbye!"))
  * }
  * }}}
  *
  * On JS/browser, this automatically creates a Three.js / WebGL terminal targeting the DOM element with id `"terminal"`
  * and uses `requestAnimationFrame`-based rendering. Override `containerId` or `webGLConfig` to customise the terminal
  * setup.
  *
  * For full control over terminal configuration, use `IOApp.Simple` with `TerminalPlatform` directly.
  */
trait LoteApp extends IOApp.Simple {

  /** Builds the presentation session. Called once when the application starts.
    *
    * The returned `SessionBuilder` will be executed with a Three.js terminal and RAF-based ticker — no need to call
    * `.run()` yourself.
    */
  def presentation: SessionBuilder[IO]

  /** The id of the DOM element where the WebGL canvas will be rendered. Defaults to `"terminal"`. */
  def containerId: String = "terminal"

  /** Optional [[WebGLConfig]] to tune the WebGL renderer. Uses sensible defaults. */
  def webGLConfig: WebGLConfig = WebGLConfig()

  override final def run: IO[Unit] = {
    val container = dom.document
      .getElementById(containerId)
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container, webGLConfig).use { implicit terminal =>
      presentation
        .withCustomTicker(RafTickerInterpreter.make[IO])
        .run()
    }
  }
}
