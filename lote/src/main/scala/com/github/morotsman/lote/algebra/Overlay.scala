package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

trait Overlay[F[_]] {
  def applyOverlay(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]
}

object Overlay {
  def make[F[_]](): Overlay[F] = new Overlay[F] {
    override def applyOverlay(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = ???
  }
}
