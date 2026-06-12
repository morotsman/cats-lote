package com.github.morotsman.lote.tools

import munit.FunSuite

class ScreenHelperSpec extends FunSuite {

  test("indexToXY converts linear index to (row, col)") {
    val screenWidth = 10
    assertEquals(ScreenHelper.indexToXY(screenWidth, 0), XY(0, 0))
    assertEquals(ScreenHelper.indexToXY(screenWidth, 5), XY(0, 5))
    assertEquals(ScreenHelper.indexToXY(screenWidth, 10), XY(1, 0))
    assertEquals(ScreenHelper.indexToXY(screenWidth, 15), XY(1, 5))
    assertEquals(ScreenHelper.indexToXY(screenWidth, 23), XY(2, 3))
  }

  test("xyToIndex converts (row, col) to linear index") {
    val screenWidth = 10
    assertEquals(ScreenHelper.xyToIndex(screenWidth, XY(0, 0)), 0)
    assertEquals(ScreenHelper.xyToIndex(screenWidth, XY(0, 5)), 5)
    assertEquals(ScreenHelper.xyToIndex(screenWidth, XY(1, 0)), 10)
    assertEquals(ScreenHelper.xyToIndex(screenWidth, XY(2, 3)), 23)
  }

  test("indexToXY and xyToIndex are inverses") {
    val screenWidth = 80
    for (i <- 0 until 240) {
      val xy = ScreenHelper.indexToXY(screenWidth, i)
      val backToIndex = ScreenHelper.xyToIndex(screenWidth, xy)
      assertEquals(backToIndex, i)
    }
  }
}

