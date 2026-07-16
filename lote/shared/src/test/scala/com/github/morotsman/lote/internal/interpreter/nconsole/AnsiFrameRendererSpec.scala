package com.github.morotsman.lote.internal.interpreter.nconsole

import munit.FunSuite

class AnsiFrameRendererSpec extends FunSuite {

  test("first frame renders each visible line") {
    val (frame, command) = AnsiFrameRenderer.render(
      previousFrame = Vector.empty,
      content = "hello\nworld",
      width = 20,
      height = 5
    )

    assertEquals(frame, Vector("hello", "world"))
    assertEquals(command, "\u001b[1;1Hhello\u001b[K\u001b[2;1Hworld\u001b[K")
  }

  test("unchanged frame produces no terminal commands") {
    val previous = Vector("same", "frame")

    val (frame, command) = AnsiFrameRenderer.render(
      previousFrame = previous,
      content = "same\nframe",
      width = 20,
      height = 5
    )

    assertEquals(frame, previous)
    assertEquals(command, "")
  }

  test("only changed rows are updated") {
    val (frame, command) = AnsiFrameRenderer.render(
      previousFrame = Vector("title", "before", "footer"),
      content = "title\nafter\nfooter",
      width = 20,
      height = 5
    )

    assertEquals(frame, Vector("title", "after", "footer"))
    assertEquals(command, "\u001b[2;1Hafter\u001b[K")
  }

  test("shrinking content clears trailing characters and removed rows") {
    val (frame, command) = AnsiFrameRenderer.render(
      previousFrame = Vector("longer line", "second row"),
      content = "short",
      width = 20,
      height = 5
    )

    assertEquals(frame, Vector("short"))
    assertEquals(command, "\u001b[1;1Hshort\u001b[K\u001b[2;1H\u001b[K")
  }

  test("normalize truncates to visible width and height") {
    val frame = AnsiFrameRenderer.normalize(
      content = "abcdef\n123456\nxyz",
      width = 4,
      height = 2
    )

    assertEquals(frame, Vector("abcd", "1234"))
  }

  test("normalize preserves ANSI reset when truncating colored text") {
    val frame = AnsiFrameRenderer.normalize(
      content = "\u001b[90mOverlays\u001b[0mInteractive",
      width = 8,
      height = 1
    )

    assertEquals(frame, Vector("\u001b[90mOverlays\u001b[0m"))
  }
}
