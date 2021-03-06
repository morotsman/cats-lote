package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import lote.model.{Alignment, Context, UserInput}


trait NConsole[F[_]] {
  def read(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[ScreenAdjusted]

  def writeString(s: ScreenAdjusted): F[Unit]

  def clear(): F[Unit]

  def context: F[Context]
}
