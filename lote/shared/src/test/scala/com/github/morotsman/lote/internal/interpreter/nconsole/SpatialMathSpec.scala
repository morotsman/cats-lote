package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.SlidePosition
import munit.FunSuite

class SpatialMathSpec extends FunSuite {

  // ── resolvePositions ──────────────────────────────────────────────────

  test("resolvePositions: empty input returns empty output") {
    assertEquals(
      SpatialMath.resolvePositions(Vector.empty),
      Vector.empty
    )
  }

  test("resolvePositions: single explicit position is preserved") {
    val pos = SlidePosition(10, 20, 30)
    assertEquals(
      SpatialMath.resolvePositions(Vector(Some(pos))),
      Vector(pos)
    )
  }

  test("resolvePositions: single None defaults to origin") {
    assertEquals(
      SpatialMath.resolvePositions(Vector(None)),
      Vector(SlidePosition(0, 0, 0))
    )
  }

  test("resolvePositions: None inherits the preceding explicit position") {
    val pos = SlidePosition(100, 200, 300)
    val result = SpatialMath.resolvePositions(Vector(Some(pos), None, None))
    assertEquals(result, Vector(pos, pos, pos))
  }

  test("resolvePositions: multiple Nones before first explicit default to origin") {
    val pos = SlidePosition(5, 5, 5)
    val result = SpatialMath.resolvePositions(Vector(None, None, Some(pos), None))
    val origin = SlidePosition(0, 0, 0)
    assertEquals(result, Vector(origin, origin, pos, pos))
  }

  test("resolvePositions: alternating explicit and None positions") {
    val a = SlidePosition(1, 0, 0)
    val b = SlidePosition(2, 0, 0)
    val result = SpatialMath.resolvePositions(Vector(Some(a), None, Some(b), None))
    assertEquals(result, Vector(a, a, b, b))
  }

  test("resolvePositions: all explicit positions are preserved") {
    val a = SlidePosition(1, 2, 3)
    val b = SlidePosition(4, 5, 6)
    val c = SlidePosition(7, 8, 9)
    val result = SpatialMath.resolvePositions(Vector(Some(a), Some(b), Some(c)))
    assertEquals(result, Vector(a, b, c))
  }

  test("resolvePositions: rotation and transparentBackground are preserved") {
    val pos = SlidePosition(0, 0, 0, rotX = 45, rotY = 90, rotZ = 180, transparentBackground = true)
    val result = SpatialMath.resolvePositions(Vector(Some(pos), None))
    assertEquals(result(0), pos)
    assertEquals(result(1), pos)
  }

  // ── deduplicatePositions ──────────────────────────────────────────────

  test("deduplicatePositions: empty input returns empty results") {
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector.empty)
    assertEquals(unique, Vector.empty)
    assertEquals(mapping, Vector.empty)
  }

  test("deduplicatePositions: all distinct positions produce one layer each") {
    val a = SlidePosition(0, 0, 0)
    val b = SlidePosition(100, 0, 0)
    val c = SlidePosition(200, 0, 0)
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(a, b, c))
    assertEquals(unique, Vector(a, b, c))
    assertEquals(mapping, Vector(0, 1, 2))
  }

  test("deduplicatePositions: duplicate positions share one layer") {
    val pos = SlidePosition(50, 50, 50)
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(pos, pos, pos))
    assertEquals(unique, Vector(pos))
    assertEquals(mapping, Vector(0, 0, 0))
  }

  test("deduplicatePositions: mixed duplicates and unique positions") {
    val a = SlidePosition(0, 0, 0)
    val b = SlidePosition(100, 0, 0)
    // slide 0 -> a, slide 1 -> b, slide 2 -> a again, slide 3 -> b again
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(a, b, a, b))
    assertEquals(unique, Vector(a, b))
    assertEquals(mapping, Vector(0, 1, 0, 1))
  }

  test("deduplicatePositions: insertion order of unique positions is preserved") {
    val a = SlidePosition(300, 0, 0)
    val b = SlidePosition(100, 0, 0)
    val c = SlidePosition(200, 0, 0)
    val (unique, _) = SpatialMath.deduplicatePositions(Vector(a, b, c, a))
    assertEquals(unique, Vector(a, b, c))
  }

  test("deduplicatePositions: deduplication keys on 6-DOF, not transparentBackground") {
    val a = SlidePosition(1, 2, 3, 4, 5, 6, transparentBackground = false)
    val b = SlidePosition(1, 2, 3, 4, 5, 6, transparentBackground = true)
    // Same 6-DOF but different transparentBackground — should share a layer
    // (the first occurrence wins)
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(a, b))
    assertEquals(unique.length, 1)
    assertEquals(unique.head, a) // first occurrence kept
    assertEquals(mapping, Vector(0, 0))
  }

  test("deduplicatePositions: positions differing only in rotation are distinct") {
    val a = SlidePosition(0, 0, 0, rotX = 0)
    val b = SlidePosition(0, 0, 0, rotX = 45)
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(a, b))
    assertEquals(unique, Vector(a, b))
    assertEquals(mapping, Vector(0, 1))
  }

  test("deduplicatePositions: single position returns single layer") {
    val pos = SlidePosition(10, 20, 30, rotX = 1, rotY = 2, rotZ = 3)
    val (unique, mapping) = SpatialMath.deduplicatePositions(Vector(pos))
    assertEquals(unique, Vector(pos))
    assertEquals(mapping, Vector(0))
  }

  // ── Integration: resolvePositions + deduplicatePositions ──────────────

  test("resolve then deduplicate: Nones sharing a position are deduplicated") {
    val pos = SlidePosition(100, 200, 0)
    val positions: Vector[Option[SlidePosition]] = Vector(Some(pos), None, None)
    val resolved = SpatialMath.resolvePositions(positions)
    val (unique, mapping) = SpatialMath.deduplicatePositions(resolved)
    assertEquals(unique, Vector(pos))
    assertEquals(mapping, Vector(0, 0, 0))
  }

  test("resolve then deduplicate: slides at different positions get distinct layers") {
    val a = SlidePosition(0, 0, 0)
    val b = SlidePosition(1800, 0, 0)
    val positions: Vector[Option[SlidePosition]] = Vector(Some(a), Some(b), None, Some(a))
    val resolved = SpatialMath.resolvePositions(positions)
    val (unique, mapping) = SpatialMath.deduplicatePositions(resolved)
    assertEquals(unique, Vector(a, b))
    assertEquals(mapping, Vector(0, 1, 1, 0))
  }
}
