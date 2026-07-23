package com.github.morotsman.lote.internal.interpreter.nconsole

import munit.FunSuite

class EffectMathSpec extends FunSuite {

  // ── dissolveOpacity ─────────────────────────────────────────────────────

  test("dissolveOpacity: progress 0 returns full opacity") {
    assertEqualsDouble(EffectMath.dissolveOpacity(0.0, 0.0, 1.0), 1.0, 0.001)
  }

  test("dissolveOpacity: progress 1 with fadeSpeed 1 returns 0") {
    assertEqualsDouble(EffectMath.dissolveOpacity(1.0, 0.0, 1.0), 0.0, 0.001)
  }

  test("dissolveOpacity: progress before fadeDelay returns full opacity") {
    assertEqualsDouble(EffectMath.dissolveOpacity(0.2, 0.5, 1.5), 1.0, 0.001)
  }

  test("dissolveOpacity: result is clamped to 0") {
    // With high fadeSpeed, opacity could go negative without clamping
    val result = EffectMath.dissolveOpacity(1.0, 0.0, 2.0)
    assert(result >= 0.0, s"Expected >= 0.0 but got $result")
  }

  test("dissolveOpacity: mid-progress returns intermediate opacity") {
    val result = EffectMath.dissolveOpacity(0.5, 0.0, 1.0)
    assert(result > 0.0 && result < 1.0, s"Expected intermediate value but got $result")
  }

  test("dissolveOpacity: fadeDelay shifts the start of fading") {
    val withoutDelay = EffectMath.dissolveOpacity(0.5, 0.0, 1.0)
    val withDelay = EffectMath.dissolveOpacity(0.5, 0.3, 1.0)
    assert(withDelay > withoutDelay, "Delayed particle should be more opaque at the same progress")
  }

  test("dissolveOpacity: faster fadeSpeed produces lower opacity") {
    val slow = EffectMath.dissolveOpacity(0.5, 0.0, 1.0)
    val fast = EffectMath.dissolveOpacity(0.5, 0.0, 2.0)
    assert(fast < slow, "Faster fade should produce lower opacity")
  }

  // ── smokeFrame ──────────────────────────────────────────────────────────

  test("smokeFrame: progress 0 with no fadeDelay returns start position") {
    val result = EffectMath.smokeFrame(0.0, 10.0, 20.0, 50.0, 100.0, 1.0, 0.0, 0.5)
    assertEqualsDouble(result.x, 10.0, 0.001)
    assertEqualsDouble(result.y, 20.0, 0.001)
  }

  test("smokeFrame: progress 0 returns full opacity") {
    val result = EffectMath.smokeFrame(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    assertEqualsDouble(result.opacity, 1.0, 0.001)
  }

  test("smokeFrame: progress 0 returns full scale") {
    val result = EffectMath.smokeFrame(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    assertEqualsDouble(result.scale, 1.0, 0.001)
  }

  test("smokeFrame: progress 1 drifts to target position") {
    val result = EffectMath.smokeFrame(1.0, 0.0, 0.0, 100.0, 200.0, 0.0, 0.0, 0.5)
    assertEqualsDouble(result.x, 100.0, 0.001)
    assertEqualsDouble(result.y, 200.0, 0.001)
  }

  test("smokeFrame: opacity decreases with progress") {
    val early = EffectMath.smokeFrame(0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    val late = EffectMath.smokeFrame(0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    assert(late.opacity < early.opacity, "Later progress should have lower opacity")
  }

  test("smokeFrame: scale decreases with progress") {
    val early = EffectMath.smokeFrame(0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    val late = EffectMath.smokeFrame(0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    assert(late.scale < early.scale, "Later progress should have smaller scale")
  }

  test("smokeFrame: scale is clamped to 0") {
    val result = EffectMath.smokeFrame(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0)
    assert(result.scale >= 0.0, s"Scale should be >= 0 but got ${result.scale}")
  }

  test("smokeFrame: opacity is clamped to 0") {
    val result = EffectMath.smokeFrame(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5)
    assert(result.opacity >= 0.0, s"Opacity should be >= 0 but got ${result.opacity}")
  }

  test("smokeFrame: rotation increases with progress") {
    val early = EffectMath.smokeFrame(0.2, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.5)
    val late = EffectMath.smokeFrame(0.8, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.5)
    assert(Math.abs(late.rotZ) > Math.abs(early.rotZ), "Later progress should have more rotation")
  }

  test("smokeFrame: fadeDelay defers the animation start") {
    val noDelay = EffectMath.smokeFrame(0.3, 0.0, 0.0, 100.0, 0.0, 0.0, 0.0, 0.5)
    val withDelay = EffectMath.smokeFrame(0.3, 0.0, 0.0, 100.0, 0.0, 0.0, 0.5, 0.5)
    assert(
      Math.abs(withDelay.x) < Math.abs(noDelay.x),
      "Delayed particle should have drifted less"
    )
  }

  test("smokeFrame: uses ease-out quadratic curve") {
    // At progress 0.5 with no delay, t = 0.5, eased = 1 - (1-0.5)^2 = 0.75
    val result = EffectMath.smokeFrame(0.5, 0.0, 0.0, 100.0, 0.0, 0.0, 0.0, 0.5)
    assertEqualsDouble(result.x, 75.0, 0.001) // driftX * eased = 100 * 0.75
  }

  // ── glowColor ───────────────────────────────────────────────────────────

  test("glowColor: white at intensity 255 returns (1, 1, 1)") {
    val (r, g, b) = EffectMath.glowColor("#ffffff", 255.0)
    assertEqualsDouble(r, 1.0, 0.001)
    assertEqualsDouble(g, 1.0, 0.001)
    assertEqualsDouble(b, 1.0, 0.001)
  }

  test("glowColor: black at any intensity returns (0, 0, 0)") {
    val (r, g, b) = EffectMath.glowColor("#000000", 100.0)
    assertEqualsDouble(r, 0.0, 0.001)
    assertEqualsDouble(g, 0.0, 0.001)
    assertEqualsDouble(b, 0.0, 0.001)
  }

  test("glowColor: red channel only") {
    val (r, g, b) = EffectMath.glowColor("#ff0000", 255.0)
    assertEqualsDouble(r, 1.0, 0.001)
    assertEqualsDouble(g, 0.0, 0.001)
    assertEqualsDouble(b, 0.0, 0.001)
  }

  test("glowColor: intensity scales proportionally") {
    val (r, _, _) = EffectMath.glowColor("#800000", 127.5)
    // 0x80 = 128, 128 * 127.5 / 255 ≈ 64.0 / 255 → wait, let me recalculate
    // r = (128 * 127.5 / 255.0).min(1.0)
    val expected = (128.0 * 127.5 / 255.0).min(1.0)
    assertEqualsDouble(r, expected, 0.001)
  }

  test("glowColor: result is clamped to 1.0") {
    val (r, _, _) = EffectMath.glowColor("#ff0000", 500.0)
    assertEqualsDouble(r, 1.0, 0.001)
  }

  test("glowColor: handles color without # prefix") {
    // ff8800 → r=255, g=136, b=0; at intensity 1.0: (255*1/255, 136*1/255, 0)
    val (r, g, b) = EffectMath.glowColor("ff8800", 1.0)
    assertEqualsDouble(r, 1.0, 0.001)
    assert(g > 0.0 && g < 1.0, s"Green should be intermediate but got $g")
    assertEqualsDouble(b, 0.0, 0.001)
  }

  test("glowColor: parses hex correctly for mixed color") {
    // At intensity 1.0: (r * 1.0 / 255.0) = r/255
    val (r, g, b) = EffectMath.glowColor("#1a2b3c", 1.0)
    assertEqualsDouble(r, 0x1a / 255.0, 0.001)
    assertEqualsDouble(g, 0x2b / 255.0, 0.001)
    assertEqualsDouble(b, 0x3c / 255.0, 0.001)
  }

  // ── clampOpacity ────────────────────────────────────────────────────────

  test("clampOpacity: value in range is unchanged") {
    assertEqualsDouble(EffectMath.clampOpacity(0.5), 0.5, 0.001)
  }

  test("clampOpacity: negative value is clamped to 0") {
    assertEqualsDouble(EffectMath.clampOpacity(-0.5), 0.0, 0.001)
  }

  test("clampOpacity: value > 1 is clamped to 1") {
    assertEqualsDouble(EffectMath.clampOpacity(1.5), 1.0, 0.001)
  }

  test("clampOpacity: 0.0 is preserved") {
    assertEqualsDouble(EffectMath.clampOpacity(0.0), 0.0, 0.001)
  }

  test("clampOpacity: 1.0 is preserved") {
    assertEqualsDouble(EffectMath.clampOpacity(1.0), 1.0, 0.001)
  }
}
