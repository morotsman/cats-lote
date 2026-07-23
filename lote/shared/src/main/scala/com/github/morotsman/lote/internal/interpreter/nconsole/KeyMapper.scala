package com.github.morotsman.lote.internal.interpreter.nconsole

/** Pure key-name-to-character-code mapping extracted from [[WebGLInputHandler]] for testability.
  *
  * Converts a DOM key name (e.g. `"ArrowUp"`, `"Enter"`) and modifier state into the same escape sequences used by
  * JLine and xterm.js. This allows the full keyboard mapping table to be tested without a browser environment.
  */
private[lote] object KeyMapper {

  /** Convert a key name and ctrl-key state to character codes matching JLine/xterm.js encoding.
    *
    * @param key
    *   the `KeyboardEvent.key` value (e.g. `"ArrowUp"`, `"a"`, `"Enter"`)
    * @param ctrlKey
    *   whether the Ctrl modifier was held
    * @return
    *   the sequence of character codes to enqueue, or `Seq.empty` if the key is not mapped
    */
  def keyNameToChars(key: String, ctrlKey: Boolean): Seq[Int] = key match {
    case "ArrowUp"    => Seq(27, 91, 65)
    case "ArrowDown"  => Seq(27, 91, 66)
    case "ArrowRight" => Seq(27, 91, 67)
    case "ArrowLeft"  => Seq(27, 91, 68)
    case "Enter"      => Seq(13)
    case "Escape"     => Seq(27)
    case "Backspace"  => Seq(127)
    case "Tab"        => Seq(9)
    case "Delete"     => Seq(27, 91, 51, 126)
    case "Home"       => Seq(27, 91, 72)
    case "End"        => Seq(27, 91, 70)
    case "PageUp"     => Seq(27, 91, 53, 126)
    case "PageDown"   => Seq(27, 91, 54, 126)
    case k if k.length == 1 =>
      if (ctrlKey) {
        val code = k.toUpperCase.charAt(0) - 64
        if (code > 0 && code < 32) Seq(code) else Seq.empty
      } else {
        Seq(k.charAt(0).toInt)
      }
    case _ => Seq.empty
  }
}
