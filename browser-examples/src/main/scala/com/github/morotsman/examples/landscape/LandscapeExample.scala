package com.github.morotsman.examples.landscape

import cats.effect._
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, TerminalPlatform, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder
import org.scalajs.dom

/** Standalone browser example: an interactive landscape rendered via WebGL.
  *
  * Use `a` / `d` to scroll the camera left/right, `s` to stop. Figures roam the countryside on their own.
  *
  * Run with: sbt "browserExamples/fastLinkJS" then open landscape.html
  */
object LandscapeExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
      SessionBuilder[IO]()
        .withFrameRate(30)
        .withAnimationFrameRate(15)
        .addTextSlide {
          _.content(
            """
              |
              |   ╔═══════════════════════════════════════╗
              |   ║       Welcome to the Landscape        ║
              |   ║                                       ║
              |   ║   Press → to enter the scene          ║
              |   ║                                       ║
              |   ║   Controls:                           ║
              |   ║     A — scroll left                   ║
              |   ║     D — scroll right                  ║
              |   ║     S — stop scrolling                ║
              |   ║                                       ║
              |   ║   Figures roam on their own.          ║
              |   ║   Press → / ← to navigate slides.    ║
              |   ╚═══════════════════════════════════════╝
              |""".stripMargin
          ).title("Welcome")
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
            .flipTransition()
        }
        .addSlideF {
          _.addSlideF(LandscapeSlide.contextual[IO]())
            .map(_.title("Landscape (ASCII)"))
        }
        .addTextSlide {
          _.content(
            """
              |
              |   ╔═══════════════════════════════════════╗
              |   ║       Entering the 3D Realm           ║
              |   ║                                       ║
              |   ║   Same landscape idea — but now       ║
              |   ║   rendered as actual 3D geometry      ║
              |   ║   with a fairy-tale appearance.       ║
              |   ║                                       ║
              |   ║   Controls:                           ║
              |   ║     A / D — rotate camera             ║
              |   ║     W / S — zoom in / out             ║
              |   ║     Q — reset to auto-rotate          ║
              |   ║                                       ║
              |   ║   Press → to enter the 3D scene.      ║
              |   ╚═══════════════════════════════════════╝
              |""".stripMargin
          ).title("3D Intro")
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
            .flipTransition()
        }
        .addSlideF {
          _.addSlideF(Landscape3DSlide.contextual[IO]())
            .map(_.title("Landscape (3D)"))
        }
        .addTextSlide {
          _.content(
            """
              |
              |   Thanks for exploring the landscape!
              |
              |   The ASCII version was rendered through the
              |   cats-lote Terminal[F] abstraction.
              |
              |   The 3D version used Three.js directly to render
              |   rolling hills, castles, trees, and roaming figures
              |   as actual geometry with fairy-tale lighting.
              |
              |""".stripMargin
          ).title("Fin")
            .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
            .smokeTransition()
        }
        .run()
    }
  }
}
