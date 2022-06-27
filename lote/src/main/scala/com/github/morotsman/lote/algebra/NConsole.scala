package com.github.morotsman
package lote.algebra

import lote.model.{Alignment, UserInput}


trait NConsole[F[_]] {
  def read(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[String]

  def writeString(s: String, alignment: Alignment): F[Unit]

  def writeString(s: String): F[Unit]

  def clear(): F[Unit]
}
