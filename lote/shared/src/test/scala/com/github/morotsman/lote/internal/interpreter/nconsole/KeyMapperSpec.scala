package com.github.morotsman.lote.internal.interpreter.nconsole

import munit.FunSuite

class KeyMapperSpec extends FunSuite {

  // ── Arrow keys ──────────────────────────────────────────────────────────

  test("ArrowUp produces ESC [ A") {
    assertEquals(KeyMapper.keyNameToChars("ArrowUp", ctrlKey = false), Seq(27, 91, 65))
  }

  test("ArrowDown produces ESC [ B") {
    assertEquals(KeyMapper.keyNameToChars("ArrowDown", ctrlKey = false), Seq(27, 91, 66))
  }

  test("ArrowRight produces ESC [ C") {
    assertEquals(KeyMapper.keyNameToChars("ArrowRight", ctrlKey = false), Seq(27, 91, 67))
  }

  test("ArrowLeft produces ESC [ D") {
    assertEquals(KeyMapper.keyNameToChars("ArrowLeft", ctrlKey = false), Seq(27, 91, 68))
  }

  // ── Special keys ────────────────────────────────────────────────────────

  test("Enter produces CR (13)") {
    assertEquals(KeyMapper.keyNameToChars("Enter", ctrlKey = false), Seq(13))
  }

  test("Escape produces ESC (27)") {
    assertEquals(KeyMapper.keyNameToChars("Escape", ctrlKey = false), Seq(27))
  }

  test("Backspace produces DEL (127)") {
    assertEquals(KeyMapper.keyNameToChars("Backspace", ctrlKey = false), Seq(127))
  }

  test("Tab produces HT (9)") {
    assertEquals(KeyMapper.keyNameToChars("Tab", ctrlKey = false), Seq(9))
  }

  test("Delete produces ESC [ 3 ~") {
    assertEquals(KeyMapper.keyNameToChars("Delete", ctrlKey = false), Seq(27, 91, 51, 126))
  }

  test("Home produces ESC [ H") {
    assertEquals(KeyMapper.keyNameToChars("Home", ctrlKey = false), Seq(27, 91, 72))
  }

  test("End produces ESC [ F") {
    assertEquals(KeyMapper.keyNameToChars("End", ctrlKey = false), Seq(27, 91, 70))
  }

  test("PageUp produces ESC [ 5 ~") {
    assertEquals(KeyMapper.keyNameToChars("PageUp", ctrlKey = false), Seq(27, 91, 53, 126))
  }

  test("PageDown produces ESC [ 6 ~") {
    assertEquals(KeyMapper.keyNameToChars("PageDown", ctrlKey = false), Seq(27, 91, 54, 126))
  }

  // ── Regular characters ──────────────────────────────────────────────────

  test("single letter 'a' produces its char code") {
    assertEquals(KeyMapper.keyNameToChars("a", ctrlKey = false), Seq('a'.toInt))
  }

  test("single letter 'Z' produces its char code") {
    assertEquals(KeyMapper.keyNameToChars("Z", ctrlKey = false), Seq('Z'.toInt))
  }

  test("digit '5' produces its char code") {
    assertEquals(KeyMapper.keyNameToChars("5", ctrlKey = false), Seq('5'.toInt))
  }

  test("space produces its char code") {
    assertEquals(KeyMapper.keyNameToChars(" ", ctrlKey = false), Seq(' '.toInt))
  }

  // ── Ctrl combos ─────────────────────────────────────────────────────────

  test("Ctrl+C produces code 3") {
    assertEquals(KeyMapper.keyNameToChars("c", ctrlKey = true), Seq(3))
  }

  test("Ctrl+A produces code 1") {
    assertEquals(KeyMapper.keyNameToChars("a", ctrlKey = true), Seq(1))
  }

  test("Ctrl+Z produces code 26") {
    assertEquals(KeyMapper.keyNameToChars("z", ctrlKey = true), Seq(26))
  }

  test("Ctrl+D produces code 4") {
    assertEquals(KeyMapper.keyNameToChars("d", ctrlKey = true), Seq(4))
  }

  test("Ctrl with uppercase letter also works") {
    assertEquals(KeyMapper.keyNameToChars("C", ctrlKey = true), Seq(3))
  }

  // ── Edge cases ──────────────────────────────────────────────────────────

  test("unknown multi-char key returns empty") {
    assertEquals(KeyMapper.keyNameToChars("F1", ctrlKey = false), Seq.empty)
  }

  test("unknown key 'Shift' returns empty") {
    assertEquals(KeyMapper.keyNameToChars("Shift", ctrlKey = false), Seq.empty)
  }

  test("Ctrl with non-alpha key returns empty") {
    // '@' - 64 = 0, which is not in range (0, 32), so empty
    assertEquals(KeyMapper.keyNameToChars("@", ctrlKey = true), Seq.empty)
  }

  test("arrow keys ignore ctrlKey flag") {
    assertEquals(KeyMapper.keyNameToChars("ArrowUp", ctrlKey = true), Seq(27, 91, 65))
  }
}
