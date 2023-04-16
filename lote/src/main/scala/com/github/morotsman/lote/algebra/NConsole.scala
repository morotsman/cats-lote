package com.github.morotsman
package lote.algebra

import com.github.morotsman.lote.model.{Alignment, Screen, ScreenAdjusted, UserInput}


trait NConsole[F[_]] {
  def read(timeoutInMillis: Long): F[UserInput]

  def read(): F[UserInput]

  def readInterruptible(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[ScreenAdjusted]

  def writeString(s: ScreenAdjusted): F[Unit]

  def clear(): F[Unit]

  def context: F[Screen]
}

object NConsole {
  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] = instance
}
