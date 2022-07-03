package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import lote.model.UserInput

trait Slide[F[_]] {
  def content: NConsole[F] => F[ScreenAdjusted]

  def startShow: NConsole[F] => F[Unit]

  def stopShow: F[Unit]

  def userInput(input: UserInput): F[Unit]
}
