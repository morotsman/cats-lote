package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.model.{ScreenAdjusted, UserInput}

trait Slide[F[_]] {
  def content: F[ScreenAdjusted]

  def startShow: F[Unit]

  def stopShow: F[Unit]

  def userInput(input: UserInput): F[Unit]
}
