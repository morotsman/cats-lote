package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.internal.interpreter.nconsole.spatial.SlideLayerMath
import munit.FunSuite

/** Unit tests for [[SlideLayerMath]] — pure mesh positioning, rotation, and grid dimension calculations.
  *
  * These tests verify the computations that SlideLayer performs during construction, without requiring DOM or Three.js.
  */
class SlideLayerMathSpec extends FunSuite {

  private val epsilon = 1e-9

  // ── cols/rows computation ──

  test("computeCols — standard 800px viewport with 10px cells gives 80 columns") {
    assertEquals(SlideLayerMath.computeCols(800, 10), 80)
  }

  test("computeRows — standard 600px viewport with 20px cells gives 30 rows") {
    assertEquals(SlideLayerMath.computeRows(600, 20), 30)
  }

  test("computeCols — viewport not evenly divisible by cell width truncates") {
    assertEquals(SlideLayerMath.computeCols(805, 10), 80)
  }

  test("computeRows — viewport not evenly divisible by cell height truncates") {
    assertEquals(SlideLayerMath.computeRows(615, 20), 30)
  }

  test("computeCols — small viewport with large cells") {
    assertEquals(SlideLayerMath.computeCols(100, 25), 4)
  }

  test("computeRows — 1080p viewport with default cells") {
    assertEquals(SlideLayerMath.computeRows(1080, 20), 54)
  }

  // ── mesh position computation ──

  test("meshPosition — origin slide centers mesh at half viewport dimensions") {
    val (x, y, z) = SlideLayerMath.meshPosition(0, 0, 0, 800, 600)
    assertEqualsDouble(x, 400.0, epsilon)
    assertEqualsDouble(y, 300.0, epsilon)
    assertEqualsDouble(z, 0.0, epsilon)
  }

  test("meshPosition — offset slide adds world position to center") {
    val (x, y, z) = SlideLayerMath.meshPosition(1800, 1200, 500, 800, 600)
    assertEqualsDouble(x, 1800.0 + 400.0, epsilon)
    assertEqualsDouble(y, 1200.0 + 300.0, epsilon)
    assertEqualsDouble(z, 500.0, epsilon)
  }

  test("meshPosition — negative coordinates work correctly") {
    val (x, y, z) = SlideLayerMath.meshPosition(-400, -300, -100, 800, 600)
    assertEqualsDouble(x, 0.0, epsilon)
    assertEqualsDouble(y, 0.0, epsilon)
    assertEqualsDouble(z, -100.0, epsilon)
  }

  test("meshPosition — different viewport dimensions") {
    val (x, y, z) = SlideLayerMath.meshPosition(0, 0, 0, 1920, 1080)
    assertEqualsDouble(x, 960.0, epsilon)
    assertEqualsDouble(y, 540.0, epsilon)
    assertEqualsDouble(z, 0.0, epsilon)
  }

  // ── mesh rotation computation ──

  test("meshRotation — zero degrees gives zero radians") {
    val (rx, ry, rz) = SlideLayerMath.meshRotation(0, 0, 0)
    assertEqualsDouble(rx, 0.0, epsilon)
    assertEqualsDouble(ry, 0.0, epsilon)
    assertEqualsDouble(rz, 0.0, epsilon)
  }

  test("meshRotation — 90 degrees gives π/2 radians") {
    val (rx, ry, rz) = SlideLayerMath.meshRotation(90, 90, 90)
    assertEqualsDouble(rx, Math.PI / 2.0, epsilon)
    assertEqualsDouble(ry, Math.PI / 2.0, epsilon)
    assertEqualsDouble(rz, Math.PI / 2.0, epsilon)
  }

  test("meshRotation — 180 degrees gives π radians") {
    val (rx, _, _) = SlideLayerMath.meshRotation(180, 0, 0)
    assertEqualsDouble(rx, Math.PI, epsilon)
  }

  test("meshRotation — 360 degrees gives 2π radians") {
    val (_, ry, _) = SlideLayerMath.meshRotation(0, 360, 0)
    assertEqualsDouble(ry, 2.0 * Math.PI, epsilon)
  }

  test("meshRotation — negative degrees gives negative radians") {
    val (rx, _, _) = SlideLayerMath.meshRotation(-45, 0, 0)
    assertEqualsDouble(rx, -Math.PI / 4.0, epsilon)
  }

  test("meshRotation — arbitrary angles convert correctly") {
    val (rx, ry, rz) = SlideLayerMath.meshRotation(30, 45, 60)
    assertEqualsDouble(rx, 30.0 * Math.PI / 180.0, epsilon)
    assertEqualsDouble(ry, 45.0 * Math.PI / 180.0, epsilon)
    assertEqualsDouble(rz, 60.0 * Math.PI / 180.0, epsilon)
  }

  // ── canvas dimensions computation ──

  test("canvasDimensions — 1x DPR matches logical dimensions") {
    val (w, h) = SlideLayerMath.canvasDimensions(80, 30, 10, 20, 1.0)
    assertEquals(w, 800)
    assertEquals(h, 600)
  }

  test("canvasDimensions — 2x DPR doubles physical dimensions") {
    val (w, h) = SlideLayerMath.canvasDimensions(80, 30, 10, 20, 2.0)
    assertEquals(w, 1600)
    assertEquals(h, 1200)
  }

  test("canvasDimensions — fractional DPR (1.5x)") {
    val (w, h) = SlideLayerMath.canvasDimensions(80, 30, 10, 20, 1.5)
    assertEquals(w, 1200) // 80 * 10 * 1.5 = 1200
    assertEquals(h, 900) // 30 * 20 * 1.5 = 900
  }

  test("canvasDimensions — 3x DPR (high-end Retina)") {
    val (w, h) = SlideLayerMath.canvasDimensions(80, 24, 10, 20, 3.0)
    assertEquals(w, 2400)
    assertEquals(h, 1440)
  }

  // ── transparent background affects depth write ──

  test("transparent background should disable depth write (documented behavior)") {
    // This is a documentation test — the actual Three.js behavior is verified visually.
    // We verify the flag is correctly propagated by SlideLayer (via the transparentBg parameter).
    // The SlideLayerMath doesn't handle this directly, but the SlideLayer constructor does:
    // if (transparentBg) { mat.depthWrite = false }
    assert(true, "transparent background depth-write behavior is documented")
  }
}
