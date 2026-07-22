package com.github.morotsman.lote.internal.interpreter.nconsole

import munit.FunSuite

class AnsiParserSpec extends FunSuite {

  import AnsiParser._
  import AnsiParser.AnsiColor._

  test("plain text produces styled chars with default colors") {
    val result = parseLine("hello")
    assertEquals(result.length, 5)
    assertEquals(result.map(_.char).mkString, "hello")
    assert(result.forall(_.fg == Default))
    assert(result.forall(_.bg == Default))
    assert(result.forall(!_.bold))
  }

  test("SGR reset (ESC[0m) restores defaults") {
    val result = parseLine("\u001b[1mA\u001b[0mB")
    assertEquals(result.length, 2)
    assertEquals(result(0).bold, true)
    assertEquals(result(0).char, 'A')
    assertEquals(result(1).bold, false)
    assertEquals(result(1).char, 'B')
  }

  test("basic foreground color (30-37)") {
    val result = parseLine("\u001b[31mR\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).fg, Basic(1)) // red
    assertEquals(result(0).char, 'R')
  }

  test("basic background color (40-47)") {
    val result = parseLine("\u001b[42mG\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).bg, Basic(2)) // green
  }

  test("bright foreground color (90-97)") {
    val result = parseLine("\u001b[90mX\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).fg, Basic(8)) // bright black
  }

  test("bright background color (100-107)") {
    val result = parseLine("\u001b[103mY\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).bg, Basic(11)) // bright yellow
  }

  test("256-color foreground (38;5;n)") {
    val result = parseLine("\u001b[38;5;196mZ\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).fg, Color256(196))
  }

  test("256-color background (48;5;n)") {
    val result = parseLine("\u001b[48;5;21mW\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).bg, Color256(21))
  }

  test("RGB foreground (38;2;r;g;b)") {
    val result = parseLine("\u001b[38;2;255;128;0mO\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).fg, Rgb(255, 128, 0))
  }

  test("RGB background (48;2;r;g;b)") {
    val result = parseLine("\u001b[48;2;0;64;128mP\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).bg, Rgb(0, 64, 128))
  }

  test("bold, italic, underline") {
    val result = parseLine("\u001b[1;3;4mQ\u001b[0m")
    assertEquals(result.length, 1)
    assertEquals(result(0).bold, true)
    assertEquals(result(0).italic, true)
    assertEquals(result(0).underline, true)
  }

  test("non-SGR CSI sequences are skipped") {
    // cursor positioning ESC[2;1H followed by text
    val result = parseLine("\u001b[2;1Hhello")
    assertEquals(result.map(_.char).mkString, "hello")
  }

  test("combined foreground and background") {
    val result = parseLine("\u001b[31;42mAB\u001b[0m")
    assertEquals(result.length, 2)
    assertEquals(result(0).fg, Basic(1))
    assertEquals(result(0).bg, Basic(2))
    assertEquals(result(1).fg, Basic(1))
    assertEquals(result(1).bg, Basic(2))
  }

  test("empty string produces empty vector") {
    assertEquals(parseLine("").length, 0)
  }

  test("color CSS conversion - basic") {
    assertEquals(AnsiColor.toCss(Basic(1), isForeground = true), "#aa0000")
    assertEquals(AnsiColor.toCss(Default, isForeground = true), "#c0c0c0")
    assertEquals(AnsiColor.toCss(Default, isForeground = false), "#000000")
  }

  test("color CSS conversion - 256 color") {
    // Index 16 = rgb(0,0,0)
    assertEquals(AnsiColor.toCss(Color256(16), isForeground = true), "#000000")
    // Index 232 = gray 8
    assertEquals(AnsiColor.toCss(Color256(232), isForeground = true), "#080808")
  }

  test("color CSS conversion - RGB") {
    assertEquals(AnsiColor.toCss(Rgb(255, 0, 128), isForeground = true), "#ff0080")
  }
}
