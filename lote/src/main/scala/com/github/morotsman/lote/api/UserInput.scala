package com.github.morotsman.lote.api

import com.github.morotsman.lote.api.SpecialKey.SpecialKey

sealed trait UserInput

final case class Key(k: SpecialKey) extends UserInput

final case class Character(c: Char) extends UserInput

final case class MouseClick(x: Int, y: Int) extends UserInput

final case class MouseMove(x: Int, y: Int) extends UserInput

object SpecialKey extends Enumeration {
  type SpecialKey = Value
  val Enter, Space, Up, Down, Left, Right, Esc, Unknown, Timeout = Value
}
