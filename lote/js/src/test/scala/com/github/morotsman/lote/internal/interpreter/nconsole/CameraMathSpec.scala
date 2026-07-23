package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.SlidePosition
import munit.FunSuite

/** Unit tests for the pure camera math utilities extracted from CameraAnimator.
  *
  * These tests verify the rotation matrix computations (computeNormal, computeUp), distance calculations,
  * interpolation, easing, and transition duration logic without requiring a Three.js scene or DOM.
  */
class CameraMathSpec extends FunSuite {

  private val epsilon = 1e-9

  // ── computeNormal ──

  test("computeNormal — identity rotation points along +Z") {
    val (nx, ny, nz) = CameraMath.computeNormal(SlidePosition(0, 0, 0))
    assertEqualsDouble(nx, 0.0, epsilon)
    assertEqualsDouble(ny, 0.0, epsilon)
    assertEqualsDouble(nz, 1.0, epsilon)
  }

  test("computeNormal — 90° rotation around Y points along +X") {
    val (nx, ny, nz) = CameraMath.computeNormal(SlidePosition(0, 0, 0, rotY = 90.0))
    assertEqualsDouble(nx, 1.0, epsilon)
    assertEqualsDouble(ny, 0.0, epsilon)
    assertEqualsDouble(nz, 0.0, 1e-6)
  }

  test("computeNormal — 90° rotation around X points along -Y") {
    // rotX = 90° → normal = (0, -sin(90°)*cos(0°), cos(90°)*cos(0°)) = (0, -1, 0)
    val (nx, ny, nz) = CameraMath.computeNormal(SlidePosition(0, 0, 0, rotX = 90.0))
    assertEqualsDouble(nx, 0.0, epsilon)
    assertEqualsDouble(ny, -1.0, epsilon)
    assertEqualsDouble(nz, 0.0, 1e-6)
  }

  test("computeNormal — 180° rotation around Y points along -Z") {
    val (nx, ny, nz) = CameraMath.computeNormal(SlidePosition(0, 0, 0, rotY = 180.0))
    assertEqualsDouble(nx, 0.0, 1e-6)
    assertEqualsDouble(ny, 0.0, epsilon)
    assertEqualsDouble(nz, -1.0, epsilon)
  }

  test("computeNormal — result is a unit vector") {
    // Test with arbitrary rotation
    val (nx, ny, nz) = CameraMath.computeNormal(SlidePosition(0, 0, 0, rotX = 30.0, rotY = 45.0))
    val length = Math.sqrt(nx * nx + ny * ny + nz * nz)
    assertEqualsDouble(length, 1.0, 1e-6)
  }

  // ── computeUp ──

  test("computeUp — identity rotation points along +Y") {
    val (ux, uy, uz) = CameraMath.computeUp(SlidePosition(0, 0, 0))
    assertEqualsDouble(ux, 0.0, epsilon)
    assertEqualsDouble(uy, 1.0, epsilon)
    assertEqualsDouble(uz, 0.0, epsilon)
  }

  test("computeUp — 90° rotZ tilts up vector into -X") {
    // rotZ = 90° → up = (-cos(0)*sin(90), cos(0)*cos(90)-0, sin(0)*cos(90)+0) = (-1, 0, 0)
    val (ux, uy, uz) = CameraMath.computeUp(SlidePosition(0, 0, 0, rotZ = 90.0))
    assertEqualsDouble(ux, -1.0, 1e-6)
    assertEqualsDouble(uy, 0.0, 1e-6)
    assertEqualsDouble(uz, 0.0, 1e-6)
  }

  test("computeUp — result is a unit vector for arbitrary rotations") {
    val (ux, uy, uz) = CameraMath.computeUp(SlidePosition(0, 0, 0, rotX = 25.0, rotY = 60.0, rotZ = 15.0))
    val length = Math.sqrt(ux * ux + uy * uy + uz * uz)
    assertEqualsDouble(length, 1.0, 1e-6)
  }

  test("computeUp and computeNormal — are orthogonal") {
    val pos = SlidePosition(0, 0, 0, rotX = 30.0, rotY = 45.0, rotZ = 10.0)
    val (nx, ny, nz) = CameraMath.computeNormal(pos)
    val (ux, uy, uz) = CameraMath.computeUp(pos)
    val dot = nx * ux + ny * uy + nz * uz
    assertEqualsDouble(dot, 0.0, 1e-6, "Normal and up vectors should be orthogonal")
  }

  // ── euclidean ──

  test("euclidean — same position yields zero") {
    val pos = SlidePosition(5, 10, 15)
    assertEqualsDouble(CameraMath.euclidean(pos, pos), 0.0, epsilon)
  }

  test("euclidean — axis-aligned distance") {
    val a = SlidePosition(0, 0, 0)
    val b = SlidePosition(3, 4, 0)
    assertEqualsDouble(CameraMath.euclidean(a, b), 5.0, epsilon)
  }

  test("euclidean — 3D distance") {
    val a = SlidePosition(1, 2, 3)
    val b = SlidePosition(4, 6, 3)
    // sqrt(9 + 16 + 0) = 5
    assertEqualsDouble(CameraMath.euclidean(a, b), 5.0, epsilon)
  }

  test("euclidean — symmetric") {
    val a = SlidePosition(1, 2, 3)
    val b = SlidePosition(10, 20, 30)
    assertEqualsDouble(CameraMath.euclidean(a, b), CameraMath.euclidean(b, a), epsilon)
  }

  test("euclidean — ignores rotation") {
    val a = SlidePosition(0, 0, 0, rotX = 45)
    val b = SlidePosition(0, 0, 0, rotX = 90, rotY = 180)
    assertEqualsDouble(CameraMath.euclidean(a, b), 0.0, epsilon)
  }

  // ── slideCenter ──

  test("slideCenter — origin slide") {
    val (cx, cy, cz) = CameraMath.slideCenter(SlidePosition(0, 0, 0), 800, 480)
    assertEqualsDouble(cx, 400.0, epsilon)
    assertEqualsDouble(cy, 240.0, epsilon)
    assertEqualsDouble(cz, 0.0, epsilon)
  }

  test("slideCenter — offset slide") {
    val (cx, cy, cz) = CameraMath.slideCenter(SlidePosition(100, 200, 50), 800, 480)
    assertEqualsDouble(cx, 500.0, epsilon)
    assertEqualsDouble(cy, 440.0, epsilon)
    assertEqualsDouble(cz, 50.0, epsilon)
  }

  // ── cameraPositionFor ──

  test("cameraPositionFor — identity rotation places camera at center + Z offset") {
    val pos = SlidePosition(0, 0, 0)
    val (camX, camY, camZ) = CameraMath.cameraPositionFor(pos, 800, 480, 2000)
    assertEqualsDouble(camX, 400.0, epsilon) // center X
    assertEqualsDouble(camY, 240.0, epsilon) // center Y
    assertEqualsDouble(camZ, 2000.0, epsilon) // center Z + distance along normal
  }

  test("cameraPositionFor — rotated slide places camera along rotated normal") {
    val pos = SlidePosition(0, 0, 0, rotY = 90.0)
    val (camX, camY, camZ) = CameraMath.cameraPositionFor(pos, 800, 480, 2000)
    // Normal points along +X, so camera should be at center + (2000, 0, 0)
    assertEqualsDouble(camX, 400.0 + 2000.0, 1e-6)
    assertEqualsDouble(camY, 240.0, epsilon)
    assertEqualsDouble(camZ, 0.0, 1e-6)
  }

  // ── transitionDuration ──

  test("transitionDuration — zero distance yields base duration") {
    val result = CameraMath.transitionDuration(0.0, 800.0, 1.8)
    assertEqualsDouble(result, 800.0, epsilon)
  }

  test("transitionDuration — scales with square root of distance") {
    val d1 = CameraMath.transitionDuration(100.0, 800.0, 1.8)
    val d2 = CameraMath.transitionDuration(400.0, 800.0, 1.8)
    // sqrt(400) = 2 * sqrt(100), so extra time should double
    val extra1 = d1 - 800.0
    val extra2 = d2 - 800.0
    assertEqualsDouble(extra2 / extra1, 2.0, 1e-6)
  }

  test("transitionDuration — always >= baseMs") {
    val distances = List(0.0, 0.001, 1.0, 100.0, 10000.0)
    distances.foreach { d =>
      val result = CameraMath.transitionDuration(d, 800.0, 1.8)
      assert(result >= 800.0, s"Duration $result < base 800.0 for distance $d")
    }
  }

  // ── easeInOut ──

  test("easeInOut — boundary values") {
    assertEqualsDouble(CameraMath.easeInOut(0.0), 0.0, epsilon)
    assertEqualsDouble(CameraMath.easeInOut(1.0), 1.0, epsilon)
  }

  test("easeInOut — midpoint is 0.5 (symmetric)") {
    assertEqualsDouble(CameraMath.easeInOut(0.5), 0.5, epsilon)
  }

  test("easeInOut — clamps below 0") {
    assertEqualsDouble(CameraMath.easeInOut(-0.5), 0.0, epsilon)
    assertEqualsDouble(CameraMath.easeInOut(-100.0), 0.0, epsilon)
  }

  test("easeInOut — clamps above 1") {
    assertEqualsDouble(CameraMath.easeInOut(1.5), 1.0, epsilon)
    assertEqualsDouble(CameraMath.easeInOut(100.0), 1.0, epsilon)
  }

  test("easeInOut — monotonically non-decreasing in [0, 1]") {
    val steps = 1000
    val values = (0 to steps).map(i => CameraMath.easeInOut(i.toDouble / steps))
    values.sliding(2).foreach {
      case Seq(a, b) => assert(b >= a - epsilon, s"Not monotonic: $a > $b")
      case _         =>
    }
  }

  test("easeInOut — output bounded in [0, 1]") {
    val steps = 1000
    (0 to steps).foreach { i =>
      val t = i.toDouble / steps
      val result = CameraMath.easeInOut(t)
      assert(result >= -epsilon, s"easeInOut($t) = $result < 0")
      assert(result <= 1.0 + epsilon, s"easeInOut($t) = $result > 1")
    }
  }

  test("easeInOut — slower start and end than linear (ease curve shape)") {
    // At t=0.25, easeInOut should be below linear
    val q1 = CameraMath.easeInOut(0.25)
    assert(q1 < 0.25, s"easeInOut(0.25) = $q1 should be < 0.25")
    // At t=0.75, easeInOut should be above linear
    val q3 = CameraMath.easeInOut(0.75)
    assert(q3 > 0.75, s"easeInOut(0.75) = $q3 should be > 0.75")
  }

  // ── lerp ──

  test("lerp — t=0 returns a") {
    assertEqualsDouble(CameraMath.lerp(10.0, 20.0, 0.0), 10.0, epsilon)
  }

  test("lerp — t=1 returns b") {
    assertEqualsDouble(CameraMath.lerp(10.0, 20.0, 1.0), 20.0, epsilon)
  }

  test("lerp — t=0.5 returns midpoint") {
    assertEqualsDouble(CameraMath.lerp(10.0, 20.0, 0.5), 15.0, epsilon)
  }

  test("lerp — works with negative values") {
    assertEqualsDouble(CameraMath.lerp(-10.0, 10.0, 0.5), 0.0, epsilon)
  }

  test("lerp — extrapolates beyond [0, 1]") {
    assertEqualsDouble(CameraMath.lerp(0.0, 10.0, 2.0), 20.0, epsilon)
    assertEqualsDouble(CameraMath.lerp(0.0, 10.0, -1.0), -10.0, epsilon)
  }
}
