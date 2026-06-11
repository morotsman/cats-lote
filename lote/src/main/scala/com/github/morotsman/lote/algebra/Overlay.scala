package com.github.morotsman.lote.algebra

import com.github.morotsman.lote.model.{Screen, ScreenAdjusted, UserInput}

trait Overlay[F[_]] {
  def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted]

  def onKeyPress(input: UserInput): F[Unit]

  def onContentChange(content: String): F[Unit]
}
