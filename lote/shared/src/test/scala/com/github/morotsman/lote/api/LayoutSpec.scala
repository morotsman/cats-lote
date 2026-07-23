package com.github.morotsman.lote.api

import munit.FunSuite

class LayoutSpec extends FunSuite {

  private val epsilon = 1e-9

  // ── Grid layout ──

  test("grid — produces correct number of positions") {
    val layout = Layout.grid(cols = 3)
    List(0, 1, 3, 5, 6, 9).foreach { count =>
      assertEquals(layout.positions(count).size, count)
    }
  }

  test("grid — first position is at origin") {
    val positions = Layout.grid(cols = 2).positions(4)
    assertEqualsDouble(positions.head.x, 0.0, epsilon)
    assertEqualsDouble(positions.head.y, 0.0, epsilon)
  }

  test("grid — arranges in row-major order with default spacing") {
    val positions = Layout.grid(cols = 3).positions(6)
    // Row 0: (0,0), (1800,0), (3600,0)
    // Row 1: (0,1200), (1800,1200), (3600,1200)
    assertEqualsDouble(positions(0).x, 0.0, epsilon)
    assertEqualsDouble(positions(0).y, 0.0, epsilon)
    assertEqualsDouble(positions(1).x, 1800.0, epsilon)
    assertEqualsDouble(positions(1).y, 0.0, epsilon)
    assertEqualsDouble(positions(2).x, 3600.0, epsilon)
    assertEqualsDouble(positions(2).y, 0.0, epsilon)
    assertEqualsDouble(positions(3).x, 0.0, epsilon)
    assertEqualsDouble(positions(3).y, 1200.0, epsilon)
    assertEqualsDouble(positions(4).x, 1800.0, epsilon)
    assertEqualsDouble(positions(4).y, 1200.0, epsilon)
    assertEqualsDouble(positions(5).x, 3600.0, epsilon)
    assertEqualsDouble(positions(5).y, 1200.0, epsilon)
  }

  test("grid — custom spacing is applied") {
    val positions = Layout.grid(cols = 2, spacingX = 500.0, spacingY = 300.0).positions(4)
    assertEqualsDouble(positions(1).x, 500.0, epsilon)
    assertEqualsDouble(positions(2).y, 300.0, epsilon)
    assertEqualsDouble(positions(3).x, 500.0, epsilon)
    assertEqualsDouble(positions(3).y, 300.0, epsilon)
  }

  test("grid — custom origin offsets all positions") {
    val origin = SlidePosition(x = 100.0, y = 200.0, z = 50.0)
    val positions = Layout.grid(cols = 2, origin = origin).positions(3)
    assertEqualsDouble(positions(0).x, 100.0, epsilon)
    assertEqualsDouble(positions(0).y, 200.0, epsilon)
    assertEqualsDouble(positions(0).z, 50.0, epsilon)
    assertEqualsDouble(positions(1).x, 1900.0, epsilon)
    assertEqualsDouble(positions(1).y, 200.0, epsilon)
  }

  test("grid — single column produces vertical stack") {
    val positions = Layout.grid(cols = 1).positions(3)
    positions.foreach { p =>
      assertEqualsDouble(p.x, 0.0, epsilon)
    }
    assertEqualsDouble(positions(0).y, 0.0, epsilon)
    assertEqualsDouble(positions(1).y, 1200.0, epsilon)
    assertEqualsDouble(positions(2).y, 2400.0, epsilon)
  }

  test("grid — cols == count produces single row") {
    val positions = Layout.grid(cols = 4).positions(4)
    positions.foreach { p =>
      assertEqualsDouble(p.y, 0.0, epsilon)
    }
    assertEqualsDouble(positions(0).x, 0.0, epsilon)
    assertEqualsDouble(positions(3).x, 5400.0, epsilon)
  }

  test("grid — cols must be > 0") {
    intercept[IllegalArgumentException] {
      Layout.grid(cols = 0)
    }
    intercept[IllegalArgumentException] {
      Layout.grid(cols = -1)
    }
  }

  test("grid — zero count produces empty list") {
    assertEquals(Layout.grid(cols = 3).positions(0), List.empty)
  }

  // ── Circle layout ──

  test("circle — produces correct number of positions") {
    val layout = Layout.circle(radius = 500.0)
    List(1, 2, 4, 8).foreach { count =>
      assertEquals(layout.positions(count).size, count)
    }
  }

  test("circle — single slide is placed at angle 0 (right of center)") {
    val positions = Layout.circle(radius = 100.0, centerX = 0.0, centerY = 0.0).positions(1)
    assertEqualsDouble(positions.head.x, 100.0, epsilon)
    assertEqualsDouble(positions.head.y, 0.0, epsilon)
  }

  test("circle — four slides are placed at cardinal points") {
    val r = 200.0
    val positions = Layout.circle(radius = r).positions(4)
    // 0°=right, 90°=down, 180°=left, 270°=up
    assertEqualsDouble(positions(0).x, r, epsilon)
    assertEqualsDouble(positions(0).y, 0.0, epsilon)
    assertEqualsDouble(positions(1).x, 0.0, epsilon)
    assertEqualsDouble(positions(1).y, r, epsilon)
    assertEqualsDouble(positions(2).x, -r, epsilon)
    assertEqualsDouble(positions(2).y, 0.0, epsilon)
    assertEqualsDouble(positions(3).x, 0.0, epsilon)
    assertEqualsDouble(positions(3).y, -r, epsilon)
  }

  test("circle — all positions are at the specified radius from center") {
    val r = 300.0
    val cx = 50.0
    val cy = -25.0
    val positions = Layout.circle(radius = r, centerX = cx, centerY = cy).positions(7)
    positions.foreach { p =>
      val dist = math.sqrt(math.pow(p.x - cx, 2) + math.pow(p.y - cy, 2))
      assertEqualsDouble(dist, r, epsilon)
    }
  }

  test("circle — center offset is applied") {
    val positions = Layout.circle(radius = 100.0, centerX = 500.0, centerY = 300.0).positions(1)
    assertEqualsDouble(positions.head.x, 600.0, epsilon)
    assertEqualsDouble(positions.head.y, 300.0, epsilon)
  }

  test("circle — centerZ is propagated to all positions") {
    val positions = Layout.circle(radius = 100.0, centerZ = 42.0).positions(3)
    positions.foreach { p =>
      assertEqualsDouble(p.z, 42.0, epsilon)
    }
  }

  test("circle — startAngle rotates all positions") {
    // startAngle = 90 means first slide is at bottom (sin=1, cos=0)
    val r = 100.0
    val positions = Layout.circle(radius = r, startAngle = 90.0).positions(1)
    assertEqualsDouble(positions.head.x, 0.0, epsilon)
    assertEqualsDouble(positions.head.y, r, epsilon)
  }

  test("circle — radius must be > 0") {
    intercept[IllegalArgumentException] {
      Layout.circle(radius = 0.0)
    }
    intercept[IllegalArgumentException] {
      Layout.circle(radius = -5.0)
    }
  }

  test("circle — count must be > 0") {
    intercept[IllegalArgumentException] {
      Layout.circle(radius = 100.0).positions(0)
    }
  }

  // ── Spiral layout ──

  test("spiral — produces correct number of positions") {
    val layout = Layout.spiral()
    List(0, 1, 5, 10).foreach { count =>
      assertEquals(layout.positions(count).size, count)
    }
  }

  test("spiral — first position is at startRadius on x-axis") {
    val positions = Layout.spiral(startRadius = 100.0, growth = 50.0).positions(1)
    assertEqualsDouble(positions.head.x, 100.0, epsilon)
    assertEqualsDouble(positions.head.y, 0.0, epsilon)
  }

  test("spiral — radius grows with each slide") {
    val startR = 100.0
    val growth = 50.0
    val positions = Layout.spiral(startRadius = startR, growth = growth, centerX = 0.0, centerY = 0.0).positions(5)
    positions.zipWithIndex.foreach { case (p, i) =>
      val expectedR = startR + i * growth
      val actualR = math.sqrt(p.x * p.x + p.y * p.y)
      assertEqualsDouble(actualR, expectedR, epsilon)
    }
  }

  test("spiral — angle step controls rotation between slides") {
    // 90-degree steps: right, up, left, down pattern
    val positions = Layout.spiral(startRadius = 100.0, growth = 0.0, angleStep = 90.0).positions(4)
    val r = 100.0
    assertEqualsDouble(positions(0).x, r, epsilon)
    assertEqualsDouble(positions(0).y, 0.0, epsilon)
    assertEqualsDouble(positions(1).x, 0.0, epsilon)
    assertEqualsDouble(positions(1).y, r, epsilon)
    assertEqualsDouble(positions(2).x, -r, epsilon)
    assertEqualsDouble(positions(2).y, 0.0, epsilon)
    assertEqualsDouble(positions(3).x, 0.0, epsilon)
    assertEqualsDouble(positions(3).y, -r, epsilon)
  }

  test("spiral — zStep increments z for each slide") {
    val positions = Layout.spiral(zStep = -100.0).positions(4)
    positions.zipWithIndex.foreach { case (p, i) =>
      assertEqualsDouble(p.z, i * -100.0, epsilon)
    }
  }

  test("spiral — center offset is applied") {
    val cx = 200.0
    val cy = 300.0
    val cz = 50.0
    val positions =
      Layout.spiral(startRadius = 100.0, growth = 0.0, centerX = cx, centerY = cy, centerZ = cz).positions(1)
    assertEqualsDouble(positions.head.x, cx + 100.0, epsilon)
    assertEqualsDouble(positions.head.y, cy, epsilon)
    assertEqualsDouble(positions.head.z, cz, epsilon)
  }

  test("spiral — zero count produces empty list") {
    assertEquals(Layout.spiral().positions(0), List.empty)
  }

  // ── Line layout ──

  test("line — produces correct number of positions") {
    val layout = Layout.line()
    List(0, 1, 3, 7).foreach { count =>
      assertEquals(layout.positions(count).size, count)
    }
  }

  test("line — default is horizontal with 1800 spacing") {
    val positions = Layout.line().positions(4)
    positions.zipWithIndex.foreach { case (p, i) =>
      assertEqualsDouble(p.x, i * 1800.0, epsilon)
      assertEqualsDouble(p.y, 0.0, epsilon)
      assertEqualsDouble(p.z, 0.0, epsilon)
    }
  }

  test("line — vertical direction") {
    val positions = Layout.line(spacing = 500.0, dirX = 0.0, dirY = 1.0).positions(3)
    positions.zipWithIndex.foreach { case (p, i) =>
      assertEqualsDouble(p.x, 0.0, epsilon)
      assertEqualsDouble(p.y, i * 500.0, epsilon)
    }
  }

  test("line — diagonal direction is normalized") {
    // direction (1,1,0) should be normalized to (1/√2, 1/√2, 0)
    val spacing = 100.0
    val positions = Layout.line(spacing = spacing, dirX = 1.0, dirY = 1.0).positions(2)
    val n = 1.0 / math.sqrt(2.0)
    assertEqualsDouble(positions(1).x, spacing * n, epsilon)
    assertEqualsDouble(positions(1).y, spacing * n, epsilon)
  }

  test("line — 3D direction works") {
    val positions = Layout.line(spacing = 300.0, dirX = 0.0, dirY = 0.0, dirZ = 1.0).positions(3)
    positions.zipWithIndex.foreach { case (p, i) =>
      assertEqualsDouble(p.x, 0.0, epsilon)
      assertEqualsDouble(p.y, 0.0, epsilon)
      assertEqualsDouble(p.z, i * 300.0, epsilon)
    }
  }

  test("line — custom origin offsets all positions") {
    val origin = SlidePosition(x = 10.0, y = 20.0, z = 30.0)
    val positions = Layout.line(spacing = 100.0, origin = origin).positions(3)
    positions.zipWithIndex.foreach { case (p, i) =>
      assertEqualsDouble(p.x, 10.0 + i * 100.0, epsilon)
      assertEqualsDouble(p.y, 20.0, epsilon)
      assertEqualsDouble(p.z, 30.0, epsilon)
    }
  }

  test("line — unnormalized direction vector has same result as normalized") {
    val pos1 = Layout.line(spacing = 100.0, dirX = 3.0, dirY = 4.0).positions(3)
    val pos2 = Layout.line(spacing = 100.0, dirX = 0.6, dirY = 0.8).positions(3)
    pos1.zip(pos2).foreach { case (a, b) =>
      assertEqualsDouble(a.x, b.x, epsilon)
      assertEqualsDouble(a.y, b.y, epsilon)
    }
  }

  test("line — direction must be non-zero") {
    intercept[IllegalArgumentException] {
      Layout.line(dirX = 0.0, dirY = 0.0, dirZ = 0.0)
    }
  }

  test("line — zero count produces empty list") {
    assertEquals(Layout.line().positions(0), List.empty)
  }

  test("line — spacing between consecutive slides is constant") {
    val spacing = 250.0
    val positions = Layout.line(spacing = spacing, dirX = 3.0, dirY = 4.0, dirZ = 5.0).positions(5)
    positions.sliding(2).foreach {
      case List(a, b) =>
        val dist = math.sqrt(math.pow(b.x - a.x, 2) + math.pow(b.y - a.y, 2) + math.pow(b.z - a.z, 2))
        assertEqualsDouble(dist, spacing, epsilon)
      case _ =>
    }
  }
}
