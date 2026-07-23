package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.LoteApp

/** The full-featured advanced presentation running in a JVM terminal.
  *
  * Uses `SharedAdvancedPresentation` for all slides. 3D positioning calls (`.at()`, `.rotatedBy()`) are gracefully
  * ignored by the terminal renderer, so the same presentation works in both environments.
  */
object AdvancedExample extends LoteApp {

  def presentation = SharedAdvancedPresentation.build[IO]()
}
