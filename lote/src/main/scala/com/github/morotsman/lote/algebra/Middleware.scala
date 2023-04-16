package com.github.morotsman.lote.algebra

trait Middleware[F[_]] extends NConsole[F] {
  def addOverlays(overlay: List[Overlay[F]]): F[Unit]
}


