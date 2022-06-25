package com.github.morotsman
package lote.algebra

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): F[Unit]
}
