package com.github.morotsman
package lote.algebra

trait PresentationExecutor[F[_]] {
  def start(): F[Unit]
  def setSlide(index: Int): F[Unit]
}
