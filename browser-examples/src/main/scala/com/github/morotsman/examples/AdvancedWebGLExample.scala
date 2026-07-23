package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.landscape.Landscape3DSlide
import com.github.morotsman.lote.api.LoteApp

/** The same full-featured presentation as AdvancedExample, rendered via Three.js / WebGL.
  *
  * Uses `SharedAdvancedPresentation` for all common slides and injects the 3D landscape slide which requires a browser
  * environment. The `LoteApp` base class automatically configures a `requestAnimationFrame`-based ticker (~60 fps,
  * vsync-aligned).
  */
object AdvancedWebGLExample extends LoteApp {

  def presentation =
    SharedAdvancedPresentation
      .build[IO](landscape3DSlide = Some(Landscape3DSlide.contextual[IO]()))

}
