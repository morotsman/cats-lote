package com.github.morotsman.lote.internal.algebra

import com.github.morotsman.lote.api.spi.{NConsole, Overlay}

private[lote] trait Middleware[F[_]] extends NConsole[F] {
  def addOverlays(overlay: List[Overlay[F]]): F[Unit]
}

