package com.github.morotsman
package lote.algebra

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit]
}
