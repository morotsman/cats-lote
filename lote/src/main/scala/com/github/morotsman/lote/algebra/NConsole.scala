package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import lote.model.{Alignment, Screen, UserInput}


trait NConsole[F[_]] {
  def read(timeoutInMillis: Long): F[UserInput]

  def read(): F[UserInput]

  def readInterruptible(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[ScreenAdjusted]

  def writeString(s: ScreenAdjusted): F[Unit]

  def clear(): F[Unit]

  def context: F[Screen]
}
