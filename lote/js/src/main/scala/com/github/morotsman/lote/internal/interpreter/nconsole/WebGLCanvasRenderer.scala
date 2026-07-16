package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom.CanvasRenderingContext2D

/** Renders ANSI-styled terminal lines onto a Canvas2D context.
  *
  * Parses ANSI SGR sequences via `AnsiParser` and draws each character cell with the
  * correct foreground/background color, bold, italic, and underline styles.
  */
private[nconsole] object WebGLCanvasRenderer {

  /** Render a single line of ANSI-styled text onto the canvas at the given row.
    *
    * @param ctx       the 2D rendering context of the offscreen canvas
    * @param line      a single line potentially containing ANSI SGR escape sequences
    * @param row       the row index (0-based)
    * @param cols      total number of columns (used to clear remaining cells)
    * @param cellWidth  pixel width of one character cell
    * @param cellHeight pixel height of one character cell
    * @param fontFamily CSS font-family string for text rendering
    */
  def renderLine(
      ctx: CanvasRenderingContext2D,
      line: String,
      row: Int,
      cols: Int,
      cellWidth: Int,
      cellHeight: Int,
      fontFamily: String
  ): Unit = {
    val y = row * cellHeight
    val styledChars = AnsiParser.parseLine(line)

    // Clear the row
    ctx.fillStyle = "#000000"
    ctx.fillRect(0, y, cols * cellWidth, cellHeight)

    styledChars.zipWithIndex.foreach { case (sc, col) =>
      val x = col * cellWidth

      // Background
      val bgCss = AnsiParser.AnsiColor.toCss(sc.bg, isForeground = false)
      if (bgCss != "#000000") {
        ctx.fillStyle = bgCss
        ctx.fillRect(x, y, cellWidth, cellHeight)
      }

      // Character
      if (sc.char != ' ') {
        val fgCss = AnsiParser.AnsiColor.toCss(sc.fg, isForeground = true)
        ctx.fillStyle = fgCss

        val weight = if (sc.bold) "bold " else ""
        val style  = if (sc.italic) "italic " else ""
        ctx.font = s"${style}${weight}${cellHeight - 4}px $fontFamily"

        ctx.textBaseline = "top"
        ctx.fillText(sc.char.toString, x, y + 2)

        // Underline
        if (sc.underline) {
          ctx.strokeStyle = fgCss
          ctx.lineWidth = 1
          ctx.beginPath()
          ctx.moveTo(x, y + cellHeight - 2)
          ctx.lineTo(x + cellWidth, y + cellHeight - 2)
          ctx.stroke()
        }
      }
    }

    // Clear remaining cells beyond content
    if (styledChars.length < cols) {
      ctx.fillStyle = "#000000"
      ctx.fillRect(styledChars.length * cellWidth, y, (cols - styledChars.length) * cellWidth, cellHeight)
    }
  }
}

