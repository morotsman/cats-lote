package com.github.morotsman
package lote.algebra

import lote.model.UserInput

trait NConsole[F[_]] {
  def read(): F[UserInput]

  def centerAlignText(s: String): F[String]

  def writeStringCenterAligned(s: String): F[Unit]

  def writeString(s: String): F[Unit]

  def clear(): F[Unit]
}
