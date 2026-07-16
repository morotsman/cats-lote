package com.github.morotsman.lote.internal.tools

case class XY(x: Int, y: Int)

object ScreenHelper {

  def indexToXY(screenWidth: Int, index: Int): XY =
    XY(index / screenWidth, index % screenWidth)

  def xyToIndex(screenWidth: Int, xy: XY): Int =
    xy.x * screenWidth + xy.y

}
