package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.model.{Screen, ScreenAdjusted}

trait Overlay[F[_]] {
  def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]
}
