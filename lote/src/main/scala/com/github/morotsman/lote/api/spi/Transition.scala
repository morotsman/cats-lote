package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.UserInput

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): F[Unit]
  def userInput(input: UserInput): F[Unit]
}

