package com.github.morotsman.lote.internal.algebra

private[lote] trait PresentationExecutor[F[_]] {
  def start(): F[Unit]
  def setSlide(index: Int): F[Unit]
}
