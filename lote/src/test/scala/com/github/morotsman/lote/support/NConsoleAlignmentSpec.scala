package com.github.morotsman.lote.support

import cats.effect.IO
import com.github.morotsman.lote.model._
import munit.CatsEffectSuite

class NConsoleAlignmentSpec extends CatsEffectSuite {

  test("alignText with Center/Center places text in the middle") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      result <- console.alignText("Hi", Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines.length, 5)
      val middleLine = lines(2)
      assert(middleLine.contains("Hi"))
      assertEquals(middleLine.indexOf("Hi"), 9)
    }
  }

  test("alignText with Up/Left places text at top-left") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      result <- console.alignText("Hi", Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines.length, 5)
      assert(lines(0).startsWith("Hi"))
    }
  }

  test("alignText with Down/Right places text at bottom-right") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      result <- console.alignText("Hi", Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines.length, 5)
      val lastLine = lines(4)
      assert(lastLine.endsWith("Hi"))
      assertEquals(lastLine.indexOf("Hi"), 18)
    }
  }

  test("alignText handles multiline content") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 6))
      result <- console.alignText("Line1\nLine2", Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines.length, 6)
      assert(lines(0).startsWith("Line1"))
      assert(lines(1).startsWith("Line2"))
    }
  }
}

