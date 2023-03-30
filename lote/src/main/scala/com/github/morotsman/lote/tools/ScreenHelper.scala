package com.github.morotsman.lote.tools

case class XY(row: Int, column: Int)

object ScreenHelper {

  def indexToXY(screenWidth: Int, index: Int): XY =
    XY(index/screenWidth, index % screenWidth)

  def xyToIndex(screenWidth: Int, xy: XY): Int =
    xy.row * screenWidth + xy.column

}
