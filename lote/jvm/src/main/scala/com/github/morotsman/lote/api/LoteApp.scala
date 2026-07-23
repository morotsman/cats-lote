package com.github.morotsman.lote.api

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.builders.SessionBuilder

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
  * On JVM, this automatically creates a JLine terminal and runs the presentation. For full control over terminal
  * configuration, use `IOApp.Simple` with `TerminalPlatform` directly.
  */
trait LoteApp extends IOApp.Simple {

  /** Builds the presentation session. Called once when the application starts.
    *
    * The returned `SessionBuilder` will be executed with a JLine terminal — no need to call `.run()` yourself.
    */
  def presentation: SessionBuilder[IO]

  override final def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      presentation.run()
    }
}
