package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.Context

object Timer {
  def make[F[_]: Monad](): Overlay[F] = (context: Context, screenAdjusted: NConsole.ScreenAdjusted) => {
    Monad[F].pure(ScreenAdjusted("hepp"))
  }

}
