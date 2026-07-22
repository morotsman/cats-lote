package com.github.morotsman.lote.internal.interpreter.nconsole

/** Parses a string containing ANSI SGR (Select Graphic Rendition) escape sequences into a sequence of [[StyledChar]]
  * values.
  *
  * Used by rendering backends (Canvas2D, Three.js) that do not natively interpret ANSI sequences, unlike xterm.js which
  * handles them automatically.
  *
  * ==Supported ANSI SGR codes==
  *   - '''0''': Reset all attributes
  *   - '''1 / 22''': Bold on / off
  *   - '''3 / 23''': Italic on / off
  *   - '''4 / 24''': Underline on / off
  *   - '''30–37''': Standard foreground colors (black, red, green, yellow, blue, magenta, cyan, white)
  *   - '''38;5;n''': 256-color foreground
  *   - '''38;2;r;g;b''': 24-bit RGB foreground
  *   - '''39''': Default foreground
  *   - '''40–47''': Standard background colors
  *   - '''48;5;n''': 256-color background
  *   - '''48;2;r;g;b''': 24-bit RGB background
  *   - '''49''': Default background
  *   - '''90–97''': Bright foreground colors
  *   - '''100–107''': Bright background colors
  *
  * Non-SGR CSI sequences (cursor movement, erase, etc.) are silently skipped.
  *
  * @see
  *   [[https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters ANSI SGR specification]]
  */
private[nconsole] object AnsiParser {

  /** A single character paired with its computed visual style.
    *
    * @param char
    *   the literal character
    * @param fg
    *   foreground color (the color of the character glyph itself) derived from ANSI SGR state
    * @param bg
    *   background color (the color of the cell behind the character) derived from ANSI SGR state
    * @param bold
    *   whether bold (SGR 1) is active
    * @param italic
    *   whether italic (SGR 3) is active
    * @param underline
    *   whether underline (SGR 4) is active
    */
  case class StyledChar(
      char: Char,
      fg: AnsiColor,
      bg: AnsiColor,
      bold: Boolean,
      italic: Boolean,
      underline: Boolean
  )

  /** Represents an ANSI color value.
    *
    * Four variants cover the full range of ANSI color specifications:
    *   - [[AnsiColor.Default]] – terminal default color
    *   - [[AnsiColor.Basic]] – one of the 16 standard / bright colors (codes 0–15)
    *   - [[AnsiColor.Color256]] – an index into the 256-color palette
    *   - [[AnsiColor.Rgb]] – a 24-bit true-color value
    */
  sealed trait AnsiColor
  object AnsiColor {

    /** The terminal's default foreground or background color.
      *
      * When resolved via [[toCss]], foreground defaults to light gray (`#c0c0c0`, the text drawn on screen) and
      * background defaults to black (`#000000`, the cell area behind each character).
      */
    case object Default extends AnsiColor

    /** A standard or bright ANSI color.
      *
      * Codes 0–7 correspond to the standard colors (black, red, green, yellow, blue, magenta, cyan, white). Codes 8–15
      * are the bright/high-intensity variants of the same colors.
      *
      * @param code
      *   color index in the range 0–15
      */
    case class Basic(code: Int) extends AnsiColor

    /** A color from the 256-color palette.
      *
      * Indices 0–15 map to the basic colors, 16–231 form a 6×6×6 color cube, and 232–255 are a grayscale ramp.
      *
      * @param index
      *   palette index in the range 0–255
      */
    case class Color256(index: Int) extends AnsiColor

    /** A 24-bit true-color value.
      *
      * @param r
      *   red component (0–255)
      * @param g
      *   green component (0–255)
      * @param b
      *   blue component (0–255)
      */
    case class Rgb(r: Int, g: Int, b: Int) extends AnsiColor

    /** Converts an [[AnsiColor]] to a CSS hex color string (e.g. `"#ff5555"`).
      *
      * @param color
      *   the ANSI color to convert
      * @param isForeground
      *   `true` when converting a foreground color (the text glyph), `false` for a background color (the cell behind
      *   the text); this only affects the [[Default]] case, which resolves to light gray (`#c0c0c0`) for foreground and
      *   black (`#000000`) for background
      * @return
      *   a CSS-compatible hex color string
      */
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

    /** Lookup table mapping basic color codes (0–15) to CSS hex strings.
      *
      * Indices 0–7 are standard colors; 8–15 are bright variants.
      */
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
      "#ffffff" // 15 bright white
    )

    /** Converts a basic color code (0–15) to a CSS hex string.
      *
      * @param code
      *   the basic color code
      * @return
      *   the corresponding CSS hex color, or `"#c0c0c0"` if out of range
      */
    private def basicColorToCss(code: Int): String =
      if (code >= 0 && code < basicColors.length) basicColors(code)
      else "#c0c0c0"

    /** Converts a 256-color palette index to a CSS hex string.
      *
      * The 256-color palette is structured as:
      *   - '''0–15''': basic colors (delegated to [[basicColorToCss]])
      *   - '''16–231''': 6×6×6 color cube where each axis value is mapped to one of {0, 51, 102, 153, 204, 255}
      *   - '''232–255''': 24-step grayscale ramp from dark (#080808) to light (#e8e8e8)
      *
      * @param index
      *   the palette index (0–255)
      * @return
      *   the corresponding CSS hex color string
      */
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

  /** Internal mutable state tracking the current SGR attributes while parsing.
    *
    * @param fg
    *   current foreground color (text glyph color)
    * @param bg
    *   current background color (cell fill color)
    * @param bold
    *   whether bold is active
    * @param italic
    *   whether italic is active
    * @param underline
    *   whether underline is active
    */
  private case class SgrState(
      fg: AnsiColor = AnsiColor.Default,
      bg: AnsiColor = AnsiColor.Default,
      bold: Boolean = false,
      italic: Boolean = false,
      underline: Boolean = false
  )

  /** Parses a single line of text that may contain ANSI CSI escape sequences, extracting styled characters.
    *
    * The parser walks the string character-by-character. When it encounters an ESC (`\u001b`) followed by `[`, it reads
    * the full CSI sequence. Only SGR sequences (terminated by `m`) update the current style; all other CSI commands are
    * silently discarded. Printable characters are emitted as [[StyledChar]] values carrying the current style state.
    *
    * '''Implementation note:''' This method deliberately uses mutable `var`s and `while` loops instead of idiomatic
    * functional constructs (`foldLeft`, recursive methods, iterators) for performance. It is called once per line of
    * terminal output on every render frame, so avoiding closure allocations, iterator overhead, and intermediate
    * collections has a measurable impact— especially on the Scala.js target where the JIT is less aggressive.
    *
    * '''Note:''' The input should not contain newline characters; use one call per line.
    *
    * @param line
    *   a single line of text, potentially containing ANSI escape sequences
    * @return
    *   an ordered vector of [[StyledChar]] representing the visible characters with their associated styles
    */
  def parseLine(line: String): Vector[StyledChar] = {
    val result = Vector.newBuilder[StyledChar]
    var state = SgrState()
    var i = 0

    while (i < line.length) {
      if (line.charAt(i) == '\u001b' && i + 1 < line.length && line.charAt(i + 1) == '[') {
        // Find the end of the CSI sequence
        val start = i + 2
        var end = start
        while (
          end < line.length && line.charAt(end) != 'm' &&
          line.charAt(end) != 'H' && line.charAt(end) != 'J' &&
          line.charAt(end) != 'K' && line.charAt(end) != 'A' &&
          line.charAt(end) != 'B' && line.charAt(end) != 'C' &&
          line.charAt(end) != 'D' && line.charAt(end) != 'h' &&
          line.charAt(end) != 'l' && line.charAt(end) != 'M' &&
          line.charAt(end) != 'n' && line.charAt(end) != 's' &&
          line.charAt(end) != 'u'
        ) {
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

  /** Applies an SGR parameter string to the current state, returning the updated state.
    *
    * The parameter string is the content between `ESC[` and `m` in an SGR sequence (e.g. `"1;38;5;196"` from
    * `ESC[1;38;5;196m`). Multiple codes separated by `;` are processed left-to-right.
    *
    * An empty parameter string is treated as SGR 0 (full reset), which matches the behaviour specified by ECMA-48.
    *
    * '''Implementation note:''' Uses a mutable index and `while` loop rather than a fold or recursive descent because
    * extended-color codes (38/48) consume variable numbers of subsequent parameters, requiring manual index advancement
    * that is awkward to express functionally without sacrificing clarity or speed.
    *
    * @param state
    *   the current SGR attribute state
    * @param params
    *   the semicolon-separated SGR code string (may be empty)
    * @return
    *   the updated state after applying all codes
    */
  private def applySgr(state: SgrState, params: String): SgrState = {
    if (params.isEmpty)
      return state.copy(fg = AnsiColor.Default, bg = AnsiColor.Default, bold = false, italic = false, underline = false)

    val codes = params.split(';').flatMap { s =>
      try Some(s.toInt)
      catch { case _: NumberFormatException => None }
    }
    var s = state
    var idx = 0

    while (idx < codes.length) {
      val code = codes(idx)
      code match {
        case 0                       => s = SgrState() // reset
        case 1                       => s = s.copy(bold = true)
        case 3                       => s = s.copy(italic = true)
        case 4                       => s = s.copy(underline = true)
        case 22                      => s = s.copy(bold = false)
        case 23                      => s = s.copy(italic = false)
        case 24                      => s = s.copy(underline = false)
        case c if c >= 30 && c <= 37 => s = s.copy(fg = AnsiColor.Basic(c - 30))
        case 38                      =>
          // Extended foreground
          if (idx + 1 < codes.length && codes(idx + 1) == 5 && idx + 2 < codes.length) {
            s = s.copy(fg = AnsiColor.Color256(codes(idx + 2)))
            idx += 2
          } else if (idx + 1 < codes.length && codes(idx + 1) == 2 && idx + 4 < codes.length) {
            s = s.copy(fg = AnsiColor.Rgb(codes(idx + 2), codes(idx + 3), codes(idx + 4)))
            idx += 4
          }
        case 39                      => s = s.copy(fg = AnsiColor.Default)
        case c if c >= 40 && c <= 47 => s = s.copy(bg = AnsiColor.Basic(c - 40))
        case 48                      =>
          // Extended background
          if (idx + 1 < codes.length && codes(idx + 1) == 5 && idx + 2 < codes.length) {
            s = s.copy(bg = AnsiColor.Color256(codes(idx + 2)))
            idx += 2
          } else if (idx + 1 < codes.length && codes(idx + 1) == 2 && idx + 4 < codes.length) {
            s = s.copy(bg = AnsiColor.Rgb(codes(idx + 2), codes(idx + 3), codes(idx + 4)))
            idx += 4
          }
        case 49                        => s = s.copy(bg = AnsiColor.Default)
        case c if c >= 90 && c <= 97   => s = s.copy(fg = AnsiColor.Basic(c - 90 + 8))
        case c if c >= 100 && c <= 107 => s = s.copy(bg = AnsiColor.Basic(c - 100 + 8))
        case _                         => // ignore unknown
      }
      idx += 1
    }
    s
  }
}
