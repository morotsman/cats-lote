package com.github.morotsman.lote.api.spi

import cats.Applicative
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted, UserInput}

trait Overlay[F[_]] {
  def applyOverlay(
      context: Screen,
      screenAdjusted: ScreenAdjusted,
      originalContent: ScreenAdjusted
  ): F[ScreenAdjusted]

  def onUserInput(userInput: UserInput)(implicit F: Applicative[F]): F[Unit] = {
    val _ = userInput
    F.unit
  }
}
