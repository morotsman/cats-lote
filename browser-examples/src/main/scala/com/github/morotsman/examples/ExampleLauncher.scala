package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.landscape.Landscape3DSlide
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
import org.scalajs.dom

/** Unified launcher for all browser examples.
  *
  * Reads `window.location.hash` to determine which example to run:
  *   - `#simple` → SimpleAnimationExample
  *   - `#webgl` → AdvancedWebGLExample
  *   - `#spatial` → SpatialLayoutExample
  *
  * If no hash (or unknown hash) is present, renders an interactive menu to pick an example. Clicking a menu item sets
  * the hash and reloads.
  *
  * This is the default main class — no need to edit build.sbt to switch examples.
  */
object ExampleLauncher extends IOApp.Simple {

  private def hash: String =
    dom.window.location.hash.stripPrefix("#").trim.toLowerCase

  override def run: IO[Unit] = {
    val h = hash
    if (h.isEmpty || !Set("simple", "webgl", "spatial").contains(h)) {
      IO(showMenu())
    } else {
      val container = dom.document
        .getElementById("terminal")
        .asInstanceOf[dom.HTMLElement]

      TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
        val session = h match {
          case "simple" => SharedSimpleExamplesPresentation.build[IO]()
          case "webgl" =>
            SharedAdvancedPresentation.build[IO](landscape3DSlide = Some(Landscape3DSlide.contextual[IO]()))
          case "spatial" => SpatialLayoutExample.slides[IO]()
        }
        session
          .withCustomTicker(RafTickerInterpreter.make[IO])
          .run()
      }
    }
  }

  private def showMenu(): Unit = {
    val container = dom.document.getElementById("terminal")
    if (container != null) {
      container.innerHTML = menuHtml
      // Attach click handlers
      dom.document.querySelectorAll("[data-example]").foreach { node =>
        val el = node.asInstanceOf[dom.HTMLElement]
        el.addEventListener(
          "click",
          (_: dom.Event) => {
            val example = el.getAttribute("data-example")
            dom.window.location.hash = example
            dom.window.location.reload()
          }
        )
      }
    }
  }

  private val menuHtml: String =
    """
      |<div style="
      |  display: flex;
      |  flex-direction: column;
      |  align-items: center;
      |  justify-content: center;
      |  height: 100vh;
      |  background: #1a1a2e;
      |  font-family: 'Courier New', monospace;
      |  color: #e0e0e0;
      |">
      |  <h1 style="color: #7fdbca; margin-bottom: 8px; font-size: 2rem;">cats-lote examples</h1>
      |  <p style="color: #888; margin-bottom: 40px; font-size: 0.9rem;">Click an example to launch it</p>
      |  <div style="display: flex; flex-direction: column; gap: 16px; width: 320px;">
      |    <button data-example="simple" style="
      |      padding: 16px 24px;
      |      font-size: 1.1rem;
      |      font-family: inherit;
      |      background: #16213e;
      |      color: #7fdbca;
      |      border: 1px solid #7fdbca;
      |      border-radius: 8px;
      |      cursor: pointer;
      |      transition: background 0.2s;
      |    ">Simple Animations</button>
      |    <button data-example="webgl" style="
      |      padding: 16px 24px;
      |      font-size: 1.1rem;
      |      font-family: inherit;
      |      background: #16213e;
      |      color: #7fdbca;
      |      border: 1px solid #7fdbca;
      |      border-radius: 8px;
      |      cursor: pointer;
      |      transition: background 0.2s;
      |    ">Advanced WebGL</button>
      |    <button data-example="spatial" style="
      |      padding: 16px 24px;
      |      font-size: 1.1rem;
      |      font-family: inherit;
      |      background: #16213e;
      |      color: #7fdbca;
      |      border: 1px solid #7fdbca;
      |      border-radius: 8px;
      |      cursor: pointer;
      |      transition: background 0.2s;
      |    ">Spatial Layouts</button>
      |  </div>
      |  <p style="color: #555; margin-top: 40px; font-size: 0.8rem;">
      |    Tip: bookmark with hash (e.g. <code>#webgl</code>) to skip this menu
      |  </p>
      |</div>
    """.stripMargin
}
