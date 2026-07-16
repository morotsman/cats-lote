package com.github.morotsman.lote.internal.interpreter.nconsole

/** Parses a string containing ANSI SGR escape sequences into a sequence of styled characters.
  *
  * Used by rendering backends (Canvas2D, Three.js) that do not natively interpret ANSI sequences,
  * unlike xterm.js which handles them automatically.
  */
private[nconsole] object AnsiParser {

  /** A single character with its associated style. */
  case class StyledChar(
      char: Char,
      fg: AnsiColor,
      bg: AnsiColor,
      bold: Boolean,
      italic: Boolean,
      underline: Boolean
  )

  /** Represents an ANSI color. */
  sealed trait AnsiColor
  object AnsiColor {
    case object Default                extends AnsiColor
    case class Basic(code: Int)        extends AnsiColor // 0-7 standard, 8-15 bright
    case class Color256(index: Int)    extends AnsiColor
    case class Rgb(r: Int, g: Int, b: Int) extends AnsiColor

    /** Convert an AnsiColor to a CSS color string. */
    def toCss(color: AnsiColor, isForeground: Boolean): String = color match {
      case Default =>
        if (isForeground) "#c0c0c0" else "#000000"
      case Basic(code) =>
        basicColorToCss(code)
      case Color256(index) =>
        color256ToCss(index)
      case Rgb(r, g, b) =>
        f"#$r%02x$g%02x$b%02x"
    }

    private val basicColors: Array[String] = Array(
      "#000000", // 0 black
      "#aa0000", // 1 red
      "#00aa00", // 2 green
      "#aa5500", // 3 yellow
      "#0000aa", // 4 blue
      "#aa00aa", // 5 magenta
      "#00aaaa", // 6 cyan
      "#aaaaaa", // 7 white
      "#555555", // 8 bright black
      "#ff5555", // 9 bright red
      "#55ff55", // 10 bright green
      "#ffff55", // 11 bright yellow
      "#5555ff", // 12 bright blue
      "#ff55ff", // 13 bright magenta
      "#55ffff", // 14 bright cyan
      "#ffffff"  // 15 bright white
    )

    private def basicColorToCss(code: Int): String =
      if (code >= 0 && code < basicColors.length) basicColors(code)
      else "#c0c0c0"

    private def color256ToCss(index: Int): String =
      if (index < 16) basicColorToCss(index)
      else if (index < 232) {
        val i = index - 16
        val r = (i / 36) * 51
        val g = ((i / 6) % 6) * 51
        val b = (i % 6) * 51
        f"#$r%02x$g%02x$b%02x"
      } else {
        val gray = 8 + (index - 232) * 10
        f"#$gray%02x$gray%02x$gray%02x"
      }
  }

  private case class SgrState(
      fg: AnsiColor = AnsiColor.Default,
      bg: AnsiColor = AnsiColor.Default,
      bold: Boolean = false,
      italic: Boolean = false,
      underline: Boolean = false
  )

  /** Parse a single line (no newlines) containing ANSI SGR sequences into styled characters. */
  def parseLine(line: String): Vector[StyledChar] = {
    val result = Vector.newBuilder[StyledChar]
    var state = SgrState()
    var i = 0

    while (i < line.length) {
      if (line.charAt(i) == '\u001b' && i + 1 < line.length && line.charAt(i + 1) == '[') {
        // Find the end of the CSI sequence
        val start = i + 2
        var end = start
        while (end < line.length && line.charAt(end) != 'm' &&
               line.charAt(end) != 'H' && line.charAt(end) != 'J' &&
               line.charAt(end) != 'K' && line.charAt(end) != 'A' &&
               line.charAt(end) != 'B' && line.charAt(end) != 'C' &&
               line.charAt(end) != 'D' && line.charAt(end) != 'h' &&
               line.charAt(end) != 'l' && line.charAt(end) != 'M' &&
               line.charAt(end) != 'n' && line.charAt(end) != 's' &&
               line.charAt(end) != 'u') {
          end += 1
        }
        if (end < line.length) {
          val command = line.charAt(end)
          if (command == 'm') {
            // SGR sequence — parse and update state
            val params = line.substring(start, end)
            state = applySgr(state, params)
          }
          // Skip all other CSI sequences (cursor positioning, erase, etc.)
          i = end + 1
        } else {
          // Malformed sequence, emit the escape char
          result += StyledChar('\u001b', state.fg, state.bg, state.bold, state.italic, state.underline)
          i += 1
        }
      } else {
        result += StyledChar(line.charAt(i), state.fg, state.bg, state.bold, state.italic, state.underline)
        i += 1
      }
    }
    result.result()
  }

  private def applySgr(state: SgrState, params: String): SgrState = {
    if (params.isEmpty) return state.copy(fg = AnsiColor.Default, bg = AnsiColor.Default, bold = false, italic = false, underline = false)

    val codes = params.split(';').flatMap { s =>
      try Some(s.toInt) catch { case _: NumberFormatException => None }
    }
    var s = state
    var idx = 0

    while (idx < codes.length) {
      val code = codes(idx)
      code match {
        case 0 => s = SgrState() // reset
        case 1 => s = s.copy(bold = true)
        case 3 => s = s.copy(italic = true)
        case 4 => s = s.copy(underline = true)
        case 22 => s = s.copy(bold = false)
        case 23 => s = s.copy(italic = false)
        case 24 => s = s.copy(underline = false)
        case c if c >= 30 && c <= 37 => s = s.copy(fg = AnsiColor.Basic(c - 30))
        case 38 =>
          // Extended foreground
          if (idx + 1 < codes.length && codes(idx + 1) == 5 && idx + 2 < codes.length) {
            s = s.copy(fg = AnsiColor.Color256(codes(idx + 2)))
            idx += 2
          } else if (idx + 1 < codes.length && codes(idx + 1) == 2 && idx + 4 < codes.length) {
            s = s.copy(fg = AnsiColor.Rgb(codes(idx + 2), codes(idx + 3), codes(idx + 4)))
            idx += 4
          }
        case 39 => s = s.copy(fg = AnsiColor.Default)
        case c if c >= 40 && c <= 47 => s = s.copy(bg = AnsiColor.Basic(c - 40))
        case 48 =>
          // Extended background
          if (idx + 1 < codes.length && codes(idx + 1) == 5 && idx + 2 < codes.length) {
            s = s.copy(bg = AnsiColor.Color256(codes(idx + 2)))
            idx += 2
          } else if (idx + 1 < codes.length && codes(idx + 1) == 2 && idx + 4 < codes.length) {
            s = s.copy(bg = AnsiColor.Rgb(codes(idx + 2), codes(idx + 3), codes(idx + 4)))
            idx += 4
          }
        case 49 => s = s.copy(bg = AnsiColor.Default)
        case c if c >= 90 && c <= 97  => s = s.copy(fg = AnsiColor.Basic(c - 90 + 8))
        case c if c >= 100 && c <= 107 => s = s.copy(bg = AnsiColor.Basic(c - 100 + 8))
        case _ => // ignore unknown
      }
      idx += 1
    }
    s
  }
}

