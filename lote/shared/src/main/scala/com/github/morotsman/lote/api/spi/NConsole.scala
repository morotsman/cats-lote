package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.{Alignment, Screen, ScreenAdjusted, UserInput}

import scala.annotation.implicitNotFound

@implicitNotFound("No implicit NConsole[${F}] found. An NConsole instance is provided by SlideContext inside slide/transition/overlay builders, or by SlideTestHarness in tests.")
trait NConsole[F[_]] {
  def read(timeoutInMillis: Long): F[UserInput]

  def read(): F[UserInput]

  def readInterruptible(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[ScreenAdjusted]

  def writeString(s: ScreenAdjusted): F[Unit]

  def clear(): F[Unit]

  def close(): F[Unit]

  def context: F[Screen]
}

object NConsole {
  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] =
    instance
}
