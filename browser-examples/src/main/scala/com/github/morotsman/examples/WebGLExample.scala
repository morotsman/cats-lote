package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Milestone, TerminalPlatform, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder
import org.scalajs.dom

import scala.concurrent.duration.DurationInt

/** Browser example using the Three.js / WebGL terminal backend.
  *
  * Same presentation API as the xterm.js example — only the terminal backend differs. Three.js must be loaded via CDN
  * in the companion HTML page.
  */
object WebGLExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
      SessionBuilder[IO]()
        .withTimer(30.minutes)
        .withProgressBar(
          List(
            Milestone("Start", 0),
            Milestone("WebGL", 1),
            Milestone("Same API", 2),
            Milestone("Bye", 3)
          )
        )
        .withQuickNavigation()
        .withFrameRate(60)
        .withAnimationFrameRate(25)
        .addTextSlide {
          _.content(
            """
              |_________     _________________________         .____    ______________________________
              |\_   ___ \   /  _  \__    ___/   _____/         |    |   \_____  \__    ___/\_   _____/
              |/    \  \/  /  /_\  \|    |  \_____  \   ______ |    |    /   |   \|    |    |    __)_
              |\     \____/    |    \    |  /        \ /_____/ |    |___/    |    \    |    |        \
              | \______  /\____|__  /____| /_______  /         |_______ \_______  /____|   /_______  /
              |        \/         \/               \/                  \/       \/                 \/
              |
              |                          ╔═══════════════════════════╗
              |                          ║   WebGL / Three.js Mode   ║
              |                          ╚═══════════════════════════╝
              |""".stripMargin
          ).title("Start")
        }
        .addTextSlide {
          _.content(
            """
              |This presentation is rendered using Three.js / WebGL.
              |
              |Under the hood:
              |- Terminal content is drawn to an offscreen Canvas2D
              |- ANSI escape sequences are parsed for colors and styles
              |- The canvas is uploaded as a texture to a WebGL plane
              |- Three.js renders the scene with hardware acceleration
              |
              |This opens the door to:
              |- 3D camera transitions between slides
              |- Post-processing effects (bloom, glitch, CRT)
              |- Custom shader-based visual effects
              |
              |All without changing the presentation API.""".stripMargin
          ).title("WebGL Backend")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(
            """
              |The Terminal[F] trait is backend-agnostic:
              |
              |  Terminal[F[_]]
              |  ├── JLineTerminal     — JVM (terminal emulator)
              |  ├── XtermTerminal     — Browser (xterm.js)
              |  └── ThreeJsTerminal   — Browser (WebGL)
              |
              |Same 5 methods: read, write, size, flush, close.
              |Same presentation logic. Different rendering.
              |
              |Swap one line to change backend:
              |  TerminalPlatform.xtermTerminal[IO](el)
              |  TerminalPlatform.threeJsTerminal[IO](el)""".stripMargin
          ).title("Same API")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
            .morphTransition()
        }
        .addTextSlide {
          _.content(
            """
              |
              |
              |        Thanks for checking out the WebGL backend!
              |
              |        The same slides, overlays, transitions, and input handling
              |        work identically across all three backends.
              |
              |""".stripMargin
          ).title("Bye")
            .fallingCharactersTransition()
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
        }
        .run()
    }
  }
}
