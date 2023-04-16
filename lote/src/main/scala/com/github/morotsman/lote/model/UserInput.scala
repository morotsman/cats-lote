package com.github.morotsman
package lote.model

import com.github.morotsman.lote.model.SpecialKey.SpecialKey

sealed trait UserInput

final case class Key(k: SpecialKey) extends UserInput

final case class Character(c: Char) extends UserInput


object SpecialKey extends Enumeration {
  type SpecialKey = Value
  val Up, Down, Left, Right, Esc, Unknown, Timeout = Value
}
