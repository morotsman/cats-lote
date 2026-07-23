package com.github.morotsman.lote.api.support

import munit.FunSuite

class EasingSpec extends FunSuite {

  // All easing functions to test
  private val allEasings: List[(String, Double => Double)] = List(
    "linear" -> Easing.linear,
    "easeInQuad" -> Easing.easeInQuad,
    "easeOutQuad" -> Easing.easeOutQuad,
    "easeInOutQuad" -> Easing.easeInOutQuad,
    "easeInCubic" -> Easing.easeInCubic,
    "easeOutCubic" -> Easing.easeOutCubic,
    "easeInOutCubic" -> Easing.easeInOutCubic,
    "easeInSine" -> Easing.easeInSine,
    "easeOutSine" -> Easing.easeOutSine,
    "easeInOutSine" -> Easing.easeInOutSine,
    "easeInExpo" -> Easing.easeInExpo,
    "easeOutExpo" -> Easing.easeOutExpo,
    "easeOutBounce" -> Easing.easeOutBounce,
    "easeInBounce" -> Easing.easeInBounce,
    "easeOutElastic" -> Easing.easeOutElastic
  )

  // Easing functions that are guaranteed monotonically non-decreasing in [0, 1]
  private val monotonicEasings: List[(String, Double => Double)] = List(
    "linear" -> Easing.linear,
    "easeInQuad" -> Easing.easeInQuad,
    "easeOutQuad" -> Easing.easeOutQuad,
    "easeInOutQuad" -> Easing.easeInOutQuad,
    "easeInCubic" -> Easing.easeInCubic,
    "easeOutCubic" -> Easing.easeOutCubic,
    "easeInOutCubic" -> Easing.easeInOutCubic,
    "easeInSine" -> Easing.easeInSine,
    "easeOutSine" -> Easing.easeOutSine,
    "easeInOutSine" -> Easing.easeInOutSine,
    "easeInExpo" -> Easing.easeInExpo,
    "easeOutExpo" -> Easing.easeOutExpo
  )

  private val epsilon = 1e-9

  // ── Boundary conditions: f(0) = 0 and f(1) = 1 ──

  allEasings.foreach { case (name, f) =>
    test(s"$name — f(0) == 0") {
      assertEqualsDouble(f(0.0), 0.0, epsilon)
    }

    test(s"$name — f(1) == 1") {
      assertEqualsDouble(f(1.0), 1.0, epsilon)
    }
  }

  // ── Symmetry: ease-in-out functions should satisfy f(0.5) ≈ 0.5 ──

  private val symmetricEasings: List[(String, Double => Double)] = List(
    "easeInOutQuad" -> Easing.easeInOutQuad,
    "easeInOutCubic" -> Easing.easeInOutCubic,
    "easeInOutSine" -> Easing.easeInOutSine
  )

  symmetricEasings.foreach { case (name, f) =>
    test(s"$name — f(0.5) == 0.5 (symmetric midpoint)") {
      assertEqualsDouble(f(0.5), 0.5, epsilon)
    }
  }

  // ── Monotonicity: non-decreasing for standard easings ──

  monotonicEasings.foreach { case (name, f) =>
    test(s"$name — monotonically non-decreasing in [0, 1]") {
      val steps = 1000
      val values = (0 to steps).map(i => f(i.toDouble / steps))
      values.sliding(2).foreach {
        case Seq(a, b) =>
          assert(
            b >= a - epsilon,
            s"$name is not monotonic: f(${values.indexOf(a)}/$steps) = $a > f(${values.indexOf(b)}/$steps) = $b"
          )
        case _ =>
      }
    }
  }

  // ── Output bounded in [0, 1] for monotonic easings ──

  monotonicEasings.foreach { case (name, f) =>
    test(s"$name — output bounded in [0, 1]") {
      val steps = 1000
      (0 to steps).foreach { i =>
        val t = i.toDouble / steps
        val result = f(t)
        assert(result >= -epsilon, s"$name($t) = $result < 0")
        assert(result <= 1.0 + epsilon, s"$name($t) = $result > 1")
      }
    }
  }

  // ── Linear is identity ──

  test("linear — is the identity function") {
    val steps = 100
    (0 to steps).foreach { i =>
      val t = i.toDouble / steps
      assertEqualsDouble(Easing.linear(t), t, epsilon)
    }
  }

  // ── Specific known values ──

  test("easeInQuad — f(0.5) == 0.25") {
    assertEqualsDouble(Easing.easeInQuad(0.5), 0.25, epsilon)
  }

  test("easeOutQuad — f(0.5) == 0.75") {
    assertEqualsDouble(Easing.easeOutQuad(0.5), 0.75, epsilon)
  }

  test("easeInCubic — f(0.5) == 0.125") {
    assertEqualsDouble(Easing.easeInCubic(0.5), 0.125, epsilon)
  }

  test("easeOutCubic — f(0.5) == 0.875") {
    assertEqualsDouble(Easing.easeOutCubic(0.5), 0.875, epsilon)
  }

  // ── Bounce: ease-in is complement of ease-out ──

  test("easeInBounce + easeOutBounce — complement relationship") {
    val steps = 100
    (0 to steps).foreach { i =>
      val t = i.toDouble / steps
      assertEqualsDouble(
        Easing.easeInBounce(t) + Easing.easeOutBounce(1 - t),
        1.0,
        epsilon,
        s"Complement failed at t=$t"
      )
    }
  }

  // ── Elastic: overshoots but settles at endpoints ──

  test("easeOutElastic — overshoots then settles (contains values > 1.0)") {
    val steps = 1000
    val values = (1 to steps - 1).map(i => Easing.easeOutElastic(i.toDouble / steps))
    assert(values.exists(_ > 1.0), "easeOutElastic should overshoot past 1.0")
  }

  // ── Ease-in vs ease-out: ease-in should be below linear, ease-out above ──

  test("easeInQuad — below linear for t in (0, 1)") {
    val steps = 100
    (1 until steps).foreach { i =>
      val t = i.toDouble / steps
      assert(Easing.easeInQuad(t) <= t, s"easeInQuad($t) should be <= $t")
    }
  }

  test("easeOutQuad — above linear for t in (0, 1)") {
    val steps = 100
    (1 until steps).foreach { i =>
      val t = i.toDouble / steps
      assert(Easing.easeOutQuad(t) >= t, s"easeOutQuad($t) should be >= $t")
    }
  }
}
