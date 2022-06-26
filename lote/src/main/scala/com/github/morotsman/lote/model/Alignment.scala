package com.github.morotsman.lote.model

import com.github.morotsman.lote.model.HorizontalAlignment.HorizontalAlignment
import com.github.morotsman.lote.model.VerticalAlignment.VerticalAlignment

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
