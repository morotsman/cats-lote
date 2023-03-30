package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.Screen

trait Overlay[F[_]] {
  def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]
}
