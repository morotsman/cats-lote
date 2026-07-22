package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom.CanvasRenderingContext2D

/** Renders ANSI-styled terminal lines onto a Canvas2D context.
  *
  * Parses ANSI SGR sequences via `AnsiParser` and draws each character cell with the correct foreground/background
  * color, bold, italic, and underline styles.
  *
  * ==Performance note==
  * `renderLine` is on the hot path — it is called for every dirty row on every `write`. The font string
  * (`s"${style}${weight}${cellHeight - 4}px $fontFamily"`) is rebuilt per character cell because bold/italic may change
  * mid-line. For slides with uniform styling, most cells produce the same string; caching the last font string and
  * skipping `canvas.font =` when unchanged would reduce Canvas 2-D state changes, but in practice the dirty-check in
  * `ThreeJsTerminal.write` already limits how often this method is called.
  */
private[nconsole] object WebGLCanvasRenderer {

  /** Render a single line of ANSI-styled text onto the canvas at the given row.
    *
    * @param canvas
    *   the 2D rendering context of the offscreen canvas
    * @param line
    *   a single line potentially containing ANSI SGR escape sequences
    * @param row
    *   the row index (0-based)
    * @param cols
    *   total number of columns (used to clear remaining cells)
    * @param cellWidth
    *   pixel width of one character cell
    * @param cellHeight
    *   pixel height of one character cell
    * @param fontFamily
    *   CSS font-family string for text rendering
    */
  def renderLine(
      canvas: CanvasRenderingContext2D,
      line: String,
      row: Int,
      cols: Int,
      cellWidth: Int,
      cellHeight: Int,
      fontFamily: String,
      transparentBg: Boolean = false
  ): Unit = {
    val y = row * cellHeight
    val styledChars = AnsiParser.parseLine(line)

    // Clear the row
    if (transparentBg) {
      canvas.clearRect(0, y, cols * cellWidth, cellHeight)
    } else {
      canvas.fillStyle = "#000000"
      canvas.fillRect(0, y, cols * cellWidth, cellHeight)
    }

    styledChars.zipWithIndex.foreach { case (sc, col) =>
      val x = col * cellWidth

      // Background
      val bgCss = AnsiParser.AnsiColor.toCss(sc.bg, isForeground = false)
      if (bgCss != "#000000") {
        canvas.fillStyle = bgCss
        canvas.fillRect(x, y, cellWidth, cellHeight)
      }

      // Character
      if (sc.char != ' ') {
        val fgCss = AnsiParser.AnsiColor.toCss(sc.fg, isForeground = true)
        canvas.fillStyle = fgCss

        val weight = if (sc.bold) "bold " else ""
        val style = if (sc.italic) "italic " else ""
        canvas.font = s"${style}${weight}${cellHeight - 4}px $fontFamily"

        canvas.textBaseline = "top"
        canvas.fillText(sc.char.toString, x, y + 2)

        // Underline
        if (sc.underline) {
          canvas.strokeStyle = fgCss
          canvas.lineWidth = 1
          canvas.beginPath()
          canvas.moveTo(x, y + cellHeight - 2)
          canvas.lineTo(x + cellWidth, y + cellHeight - 2)
          canvas.stroke()
        }
      }
    }

    // Clear remaining cells beyond content
    if (styledChars.length < cols) {
      if (transparentBg) {
        canvas.clearRect(styledChars.length * cellWidth, y, (cols - styledChars.length) * cellWidth, cellHeight)
      } else {
        canvas.fillStyle = "#000000"
        canvas.fillRect(styledChars.length * cellWidth, y, (cols - styledChars.length) * cellWidth, cellHeight)
      }
    }
  }
}
