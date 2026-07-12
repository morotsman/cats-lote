package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.{ScreenAdjusted, UserInput}

trait Slide[F[_]] {
  def content: F[ScreenAdjusted]

  def startShow: F[Unit]

  def stopShow: F[Unit]

  def userInput(input: UserInput): F[Unit]
}

