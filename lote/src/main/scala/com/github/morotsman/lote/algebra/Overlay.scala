package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.Context

trait Overlay[F[_]] {
  def applyOverlay(context: Context, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]
}
