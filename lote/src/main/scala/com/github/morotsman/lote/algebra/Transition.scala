package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.model.UserInput

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): F[Unit]
  def userInput(input: UserInput): F[Unit]
}
