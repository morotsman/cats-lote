package com.github.morotsman
package lote.algebra

import lote.model.UserInput

trait Slide[F[_]] {
  def content: F[String]

  def startShow(): F[Unit]

  def stopShow(): F[Unit]

  def userInput(input: UserInput): F[Unit]
}
