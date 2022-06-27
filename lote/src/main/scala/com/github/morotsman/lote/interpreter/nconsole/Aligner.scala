package com.github.morotsman.lote.interpreter.nconsole

import com.github.morotsman.lote.model.HorizontalAlignment.HorizontalAlignment
import com.github.morotsman.lote.model.VerticalAlignment.VerticalAlignment
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object Aligner {

  final val SPACE = " "

  def alignText(s: String, alignment: Alignment, width: Int, height: Int): String = {
    val alignedHorizontal = alignTextHorizontal(s, alignment.horizontalAlignment, width, height)
    alignVertical(alignedHorizontal, alignment.verticalAlignment, width, height)
  }

  def alignTextHorizontal(s: String, horizontalAlignment: HorizontalAlignment, width: Int, height: Int): String = {
    val splitByNewLine = s.split("\n")
    (horizontalAlignment match {
      case HorizontalAlignment.Left =>
        splitByNewLine.map { line =>
          line + Array.fill(width - line.length)(SPACE).mkString("")
        }
      case HorizontalAlignment.Center =>
        val padFactor = splitByNewLine.map(line => (width - line.length) / 2).min
        val padding = Array.fill(padFactor)(SPACE).mkString("")
        splitByNewLine.map { line =>
          padding + line + Array.fill(width - padding.length - line.length)(SPACE).mkString("")
        }
      case HorizontalAlignment.Right =>
        splitByNewLine.map { line =>
          Array.fill(width - line.length)(SPACE).mkString("") + line
        }
    }).mkString("\n")
  }

  def alignVertical(s: String, verticalAlignment: VerticalAlignment, width: Int, height: Int): String = {
    val splitByNewLine = s.split("\n")
    val emptyRow = Array.fill(width)(SPACE).mkString("")
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
