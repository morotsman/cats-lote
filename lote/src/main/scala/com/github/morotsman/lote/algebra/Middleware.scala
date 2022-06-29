package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

trait Middleware[F[_]] {
  def addOverlays(overlay: List[Overlay[F]]): F[Unit]

  def applyMiddleware(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]
}


