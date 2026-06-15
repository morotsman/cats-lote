package com.github.morotsman.lote.interpreter.nconsole

import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}
import munit.FunSuite

class AlignerSpec extends FunSuite {

  // --- alignTextHorizontal ---

  test("alignTextHorizontal Left pads right side with spaces") {
    val result = Aligner.alignTextHorizontal("Hi", HorizontalAlignment.Left, 10)
    assertEquals(result, "Hi        ")
    assertEquals(result.length, 10)
  }

  test("alignTextHorizontal Left with multiline pads each line") {
    val result =
      Aligner.alignTextHorizontal("AB\nCDE", HorizontalAlignment.Left, 10)
    val lines = result.split("\n")
    assertEquals(lines(0), "AB        ")
    assertEquals(lines(1), "CDE       ")
    lines.foreach(l => assertEquals(l.length, 10))
  }

  test("alignTextHorizontal Center places text in the middle") {
    val result =
      Aligner.alignTextHorizontal("Hi", HorizontalAlignment.Center, 10)
    val lines = result.split("\n")
    // (10 - 2) / 2 = 4 spaces left padding
    assert(lines(0).startsWith("    Hi"))
    assertEquals(lines(0).length, 10)
  }

  test("alignTextHorizontal Center with multiline uses minimum left padding") {
    val result =
      Aligner.alignTextHorizontal("AB\nCDEFG", HorizontalAlignment.Center, 10)
    val lines = result.split("\n")
    // min padding: min((10-2)/2, (10-5)/2) = min(4, 2) = 2
    assert(lines(0).startsWith("  AB"))
    assert(lines(1).startsWith("  CDEFG"))
    lines.foreach(l => assertEquals(l.length, 10))
  }

  test("alignTextHorizontal Right places text towards the right") {
    val result =
      Aligner.alignTextHorizontal("Hi", HorizontalAlignment.Right, 10)
    val lines = result.split("\n")
    // padding = 10 - 2 - 1 = 7
    assert(lines(0).contains("Hi"))
    assertEquals(lines(0).length, 10)
    // Text should be near the right edge
    assert(lines(0).indexOf("Hi") >= 5)
  }

  test("alignTextHorizontal Right with multiline uses minimum padding") {
    val result =
      Aligner.alignTextHorizontal("AB\nCDEFG", HorizontalAlignment.Right, 10)
    val lines = result.split("\n")
    // min padding: min(10-2-1, 10-5-1) = min(7, 4) = 4
    assert(lines(0).startsWith("    AB"))
    assert(lines(1).startsWith("    CDEFG"))
    lines.foreach(l => assertEquals(l.length, 10))
  }

  // --- alignVertical ---

  test("alignVertical Up places content at the top") {
    val result = Aligner.alignVertical("Hello", VerticalAlignment.Up, 10, 5)
    val lines = result.split("\n")
    assertEquals(lines(0), "Hello")
    // Remaining lines should be padding
    for (i <- 1 until lines.length) {
      assertEquals(lines(i).trim, "")
    }
  }

  test("alignVertical Center places content in the middle") {
    val result = Aligner.alignVertical("X", VerticalAlignment.Center, 5, 7)
    val lines = result.split("\n")
    // height=7, content=1 line, rowsToAdd=7-1-1=5, topPad=5/2=2
    assertEquals(lines(2), "X")
  }

  test("alignVertical Down places content at the bottom") {
    val result = Aligner.alignVertical("End", VerticalAlignment.Down, 10, 5)
    val lines = result.split("\n")
    // rowsToAdd=5-1-1=3, all padding before content
    assertEquals(lines.last, "End")
    for (i <- 0 until 3) {
      assertEquals(lines(i).trim, "")
    }
  }

  test("alignVertical produces correct total number of lines") {
    val result = Aligner.alignVertical("A\nB", VerticalAlignment.Center, 10, 10)
    val lines = result.split("\n")
    // height - 1 = 9 total lines (height includes the -1 in implementation)
    assertEquals(lines.length, 9)
  }

  // --- alignText (full) ---

  test("alignText Center/Center places single word in center of screen") {
    val result = Aligner.alignText(
      "Hi",
      Alignment(VerticalAlignment.Center, HorizontalAlignment.Center),
      20,
      7
    )
    val lines = result.split("\n")
    // Vertical: rowsToAdd=7-1-1=5, topPad=5/2=2, so content at index 2
    assert(lines(2).contains("Hi"))
    // Horizontal: (20-2)/2=9
    assertEquals(lines(2).indexOf("Hi"), 9)
  }

  test("alignText Up/Left places text at top-left corner") {
    val result = Aligner.alignText(
      "AB",
      Alignment(VerticalAlignment.Up, HorizontalAlignment.Left),
      20,
      5
    )
    val lines = result.split("\n")
    assert(lines(0).startsWith("AB"))
    assertEquals(lines(0).length, 20)
  }

  test("alignText Down/Right places text at bottom-right area") {
    val result = Aligner.alignText(
      "XY",
      Alignment(VerticalAlignment.Down, HorizontalAlignment.Right),
      20,
      5
    )
    val lines = result.split("\n")
    val lastLine = lines.last
    assert(lastLine.contains("XY"))
    // Should be near the right side
    assert(lastLine.indexOf("XY") >= 10)
  }

  test("alignText handles multiline content with Center/Center") {
    val result = Aligner.alignText(
      "Line1\nLine2",
      Alignment(VerticalAlignment.Center, HorizontalAlignment.Center),
      20,
      9
    )
    val lines = result.split("\n")
    // rowsToAdd=9-2-1=6, topPad=6/2=3
    assert(lines(3).contains("Line1"))
    assert(lines(4).contains("Line2"))
  }

  test("alignText all lines have consistent width") {
    val result = Aligner.alignText(
      "short\nlonger text",
      Alignment(VerticalAlignment.Up, HorizontalAlignment.Left),
      30,
      6
    )
    val lines = result.split("\n")
    lines.foreach(l => assertEquals(l.length, 30))
  }

  test("alignText with exact width content has no extra padding horizontally") {
    val content = "X" * 10
    val result =
      Aligner.alignTextHorizontal(content, HorizontalAlignment.Left, 10)
    assertEquals(result, content)
    assertEquals(result.length, 10)
  }
}
