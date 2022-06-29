package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

object Timer {
  def make[F[_]: Monad](): Overlay[F] = new Overlay[F] {
    override def applyOverlay(screenAdjusted: NConsole.ScreenAdjusted): F[NConsole.ScreenAdjusted] = {
      Monad[F].pure(ScreenAdjusted("hepp"))
    }
  }

}
