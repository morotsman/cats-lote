package com.github.morotsman.lote.interpreter.nconsole

import com.github.morotsman.lote.model.HorizontalAlignment.HorizontalAlignment
import com.github.morotsman.lote.model.VerticalAlignment.VerticalAlignment
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object Aligner {

  final val SPACE = " "

  def alignText(s: String, alignment: Alignment, width: Int, height: Int): String = {
    val alignedHorizontal = alignTextHorizontal(s, alignment.horizontalAlignment, width)
    alignVertical(alignedHorizontal, alignment.verticalAlignment, width, height)
  }

  def alignTextHorizontal(s: String, horizontalAlignment: HorizontalAlignment, width: Int): String = {
    val splitByNewLine = s.split("\n")
    (horizontalAlignment match {
      case HorizontalAlignment.Left =>
        splitByNewLine.map { line =>
          val padding = SPACE * (width - line.length)
          line + padding
        }
      case HorizontalAlignment.Center =>
        val minLeftPad = splitByNewLine.map(line => (width - line.length) / 2).min
        val leftPadding = SPACE * minLeftPad
        splitByNewLine.map { line =>
          val rightPadding = SPACE * (width - minLeftPad - line.length)
          leftPadding + line + rightPadding
        }
      case HorizontalAlignment.Right =>
        splitByNewLine.map { line =>
          val padding = SPACE * (width - line.length)
          padding + line
        }
    }).mkString("\n")
  }

  def alignVertical(s: String, verticalAlignment: VerticalAlignment, width: Int, height: Int): String = {
    val splitByNewLine = s.split("\n")
    val emptyRow = SPACE * width
    val rowsToAdd = height - splitByNewLine.size - 1
    val pad = Array.fill(rowsToAdd)(emptyRow)
    val rows = verticalAlignment match {
      case VerticalAlignment.Up =>
        splitByNewLine ++ pad
      case VerticalAlignment.Center =>
        val (leftPad, rightPad) = pad.splitAt(rowsToAdd / 2)
        leftPad ++ splitByNewLine ++ rightPad
      case VerticalAlignment.Down =>
        pad ++ splitByNewLine
    }
    rows.mkString("\n")
  }
}
