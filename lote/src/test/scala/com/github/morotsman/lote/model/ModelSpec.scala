package com.github.morotsman.lote.model

import munit.FunSuite

class ModelSpec extends FunSuite {

  test("Screen stores width and height") {
    val screen = Screen(80, 24)
    assertEquals(screen.screenWidth, 80)
    assertEquals(screen.screenHeight, 24)
  }

  test("Alignment stores vertical and horizontal alignment") {
    val alignment =
      Alignment(VerticalAlignment.Center, HorizontalAlignment.Left)
    assertEquals(alignment.verticalAlignment, VerticalAlignment.Center)
    assertEquals(alignment.horizontalAlignment, HorizontalAlignment.Left)
  }

  test("ScreenAdjusted stores content string") {
    val sa = ScreenAdjusted("hello world")
    assertEquals(sa.content, "hello world")
  }

  test("UserInput Character holds a char") {
    val input: UserInput = Character('a')
    input match {
      case Character(c) => assertEquals(c, 'a')
      case _            => fail("Expected Character")
    }
  }

  test("UserInput Key holds a SpecialKey") {
    val input: UserInput = Key(SpecialKey.Right)
    input match {
      case Key(k) => assertEquals(k, SpecialKey.Right)
      case _      => fail("Expected Key")
    }
  }

  test("SpecialKey has all expected values") {
    val allKeys = SpecialKey.values
    assert(allKeys.contains(SpecialKey.Enter))
    assert(allKeys.contains(SpecialKey.Up))
    assert(allKeys.contains(SpecialKey.Down))
    assert(allKeys.contains(SpecialKey.Left))
    assert(allKeys.contains(SpecialKey.Right))
    assert(allKeys.contains(SpecialKey.Esc))
    assert(allKeys.contains(SpecialKey.Unknown))
    assert(allKeys.contains(SpecialKey.Timeout))
  }

  test("VerticalAlignment has Up, Center, Down") {
    val all = VerticalAlignment.values
    assert(all.contains(VerticalAlignment.Up))
    assert(all.contains(VerticalAlignment.Center))
    assert(all.contains(VerticalAlignment.Down))
  }

  test("HorizontalAlignment has Left, Center, Right") {
    val all = HorizontalAlignment.values
    assert(all.contains(HorizontalAlignment.Left))
    assert(all.contains(HorizontalAlignment.Center))
    assert(all.contains(HorizontalAlignment.Right))
  }
}
