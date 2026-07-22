package com.github.morotsman.lote.api.support

/** Common easing functions for use with `TickedTransition.Builder.withEasing`.
  *
  * Each function maps a linear progress value `t ∈ [0.0, 1.0]` to an eased
  * value in (approximately) the same range, producing smoother or more
  * expressive animation curves.
  *
  * ==Usage==
  * {{{
  * TickedTransition[F](console, ticker, animationSettings)
  *   .withEasing(Easing.easeInOutCubic)
  *   .buildProgress(800.millis) { (from, to, ctx) =>
  *     // ctx.progress is already eased
  *     ProgressResult.continue(blendFrame(from, to, ctx.progress))
  *   }
  * }}}
  *
  * ==Visual Reference==
  *
  * {{{
  * linear             easeInQuad          easeOutQuad         easeInOutQuad
  * 1 |        ·····   1 |          ··   1 |    ·········   1 |         ····
  *   |     ···         |        ··       |  ···             |       ··
  *   |  ···            |      ·          | ·                |     ·
  *   |···              |    ·            |·                 |   ·
  * 0 +--------→ t    0 +·········→ t   0 +·-------→ t    0 +····-----→ t
  *
  * easeInCubic        easeOutCubic        easeInOutCubic      easeInOutSine
  * 1 |          ·    1 |   ···········   1 |          ····   1 |        ····
  *   |         ·       |  ·               |        ··         |      ··
  *   |        ·        | ·                |      ·            |    ··
  *   |      ·          |·                 |   ·               |  ··
  * 0 +··········→ t  0 +·------→ t      0 +····------→ t   0 +····-----→ t
  *
  * easeInExpo         easeOutExpo         easeOutBounce       easeOutElastic
  * 1 |          ·    1 |  ············   1 |  · · ·  ····   1 | ·
  *   |         ·       | ·                |         ·        |  · ·········
  *   |        ·        |·                 | ·   ·            |   ·
  *   |       ·         |                  |·                 |  ·
  * 0 +··········→ t  0 +·-----→ t       0 +·-------→ t    0 +·--------→ t
  * }}}
  *
  * For interactive curve visualizations, see [[https://easings.net easings.net]].
  */
object Easing {

  /** Linear (identity) — no easing. */
  val linear: Double => Double = t => t

  // ── Quadratic ──────────────────────────────────────────────────

  /** Ease in (slow start, accelerating). */
  val easeInQuad: Double => Double = t => t * t

  /** Ease out (fast start, decelerating). */
  val easeOutQuad: Double => Double = t => t * (2 - t)

  /** Ease in-out (slow start and end). */
  val easeInOutQuad: Double => Double = t =>
    if (t < 0.5) 2 * t * t
    else -1 + (4 - 2 * t) * t

  // ── Cubic ──────────────────────────────────────────────────────

  /** Cubic ease in. */
  val easeInCubic: Double => Double = t => t * t * t

  /** Cubic ease out. */
  val easeOutCubic: Double => Double = t => {
    val u = t - 1
    u * u * u + 1
  }

  /** Cubic ease in-out. */
  val easeInOutCubic: Double => Double = t =>
    if (t < 0.5) 4 * t * t * t
    else {
      val u = 2 * t - 2
      (u * u * u + 2) / 2
    }

  // ── Sine ───────────────────────────────────────────────────────

  /** Sine ease in. */
  val easeInSine: Double => Double = t =>
    1 - Math.cos(t * Math.PI / 2)

  /** Sine ease out. */
  val easeOutSine: Double => Double = t =>
    Math.sin(t * Math.PI / 2)

  /** Sine ease in-out. */
  val easeInOutSine: Double => Double = t =>
    -(Math.cos(Math.PI * t) - 1) / 2

  // ── Exponential ────────────────────────────────────────────────

  /** Exponential ease in. */
  val easeInExpo: Double => Double = t =>
    if (t == 0) 0.0 else Math.pow(2, 10 * (t - 1))

  /** Exponential ease out. */
  val easeOutExpo: Double => Double = t =>
    if (t == 1) 1.0 else 1 - Math.pow(2, -10 * t)

  // ── Bounce ─────────────────────────────────────────────────────

  /** Bounce ease out. */
  val easeOutBounce: Double => Double = { t =>
    if (t < 1.0 / 2.75) {
      7.5625 * t * t
    } else if (t < 2.0 / 2.75) {
      val u = t - 1.5 / 2.75
      7.5625 * u * u + 0.75
    } else if (t < 2.5 / 2.75) {
      val u = t - 2.25 / 2.75
      7.5625 * u * u + 0.9375
    } else {
      val u = t - 2.625 / 2.75
      7.5625 * u * u + 0.984375
    }
  }

  /** Bounce ease in. */
  val easeInBounce: Double => Double = t =>
    1 - easeOutBounce(1 - t)

  // ── Elastic ────────────────────────────────────────────────────

  /** Elastic ease out (spring-like overshoot). */
  val easeOutElastic: Double => Double = { t =>
    if (t == 0 || t == 1) t
    else {
      val p = 0.3
      Math.pow(2, -10 * t) * Math.sin((t - p / 4) * (2 * Math.PI) / p) + 1
    }
  }
}

