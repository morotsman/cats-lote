package com.github.morotsman.lote.api

import com.github.morotsman.lote.api.HorizontalAlignment.HorizontalAlignment
import com.github.morotsman.lote.api.VerticalAlignment.VerticalAlignment

object VerticalAlignment extends Enumeration {
  type VerticalAlignment = Value
  val Up, Center, Down = Value
}

object HorizontalAlignment extends Enumeration {
  type HorizontalAlignment = Value
  val Left, Center, Right = Value
}

case class Alignment(
    verticalAlignment: VerticalAlignment,
    horizontalAlignment: HorizontalAlignment
)
