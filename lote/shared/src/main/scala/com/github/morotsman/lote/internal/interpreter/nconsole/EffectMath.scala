package com.github.morotsman.lote.internal.interpreter.nconsole

/** Pure math functions extracted from [[WebGLEffectRenderer]] for testability.
  *
  * These functions compute per-frame particle state (opacity, position, scale, rotation) and color values without any
  * side-effects or platform dependencies. They can be tested in the shared MUnit suite.
  */
private[lote] object EffectMath {

  /** Dissolve: per-particle opacity based on global progress and particle-specific delay/speed.
    *
    * Each character starts fading at a different time (`fadeDelay`) and fades out at its own speed (`fadeSpeed`). The
    * result is clamped to `[0, 1]`.
    *
    * @param progress
    *   global effect progress in `[0, 1]`
    * @param fadeDelay
    *   per-particle delay before fading begins, in `[0, 1)`
    * @param fadeSpeed
    *   per-particle fade speed multiplier (typically `1.2` to `2.0`)
    * @return
    *   the particle's opacity for this frame
    */
  def dissolveOpacity(progress: Double, fadeDelay: Double, fadeSpeed: Double): Double = {
    val t = Math.max(0.0, (progress - fadeDelay) / (1.0 - fadeDelay))
    Math.max(0.0, 1.0 - t * fadeSpeed)
  }

  /** Smoke: per-particle position, opacity, scale, and rotation based on progress and particle parameters.
    *
    * Uses an ease-out quadratic curve (`1 - (1 - t)^2`) to give particles a natural deceleration as they drift.
    *
    * @param progress
    *   global effect progress in `[0, 1]`
    * @param startX
    *   initial X position of the particle
    * @param startY
    *   initial Y position of the particle
    * @param driftX
    *   maximum horizontal drift
    * @param driftY
    *   maximum vertical drift
    * @param rotSpeed
    *   rotation speed in radians
    * @param fadeDelay
    *   per-particle delay before fading begins
    * @param shrinkRate
    *   per-particle shrink rate (how fast the particle scales down)
    * @return
    *   the computed frame result containing position, rotation, opacity, and scale
    */
  def smokeFrame(
      progress: Double,
      startX: Double,
      startY: Double,
      driftX: Double,
      driftY: Double,
      rotSpeed: Double,
      fadeDelay: Double,
      shrinkRate: Double
  ): SmokeFrameResult = {
    val t = Math.max(0.0, (progress - fadeDelay) / (1.0 - fadeDelay))
    val eased = 1.0 - Math.pow(1.0 - t, 2.0)

    SmokeFrameResult(
      x = startX + driftX * eased,
      y = startY + driftY * eased,
      rotZ = rotSpeed * eased,
      opacity = Math.max(0.0, 1.0 - t * 1.3),
      scale = Math.max(0.0, 1.0 - t * shrinkRate)
    )
  }

  /** Glow: parse CSS hex color and compute RGB intensity values.
    *
    * Parses a `#RRGGBB` hex color string and scales each channel by `intensity / 255.0`, clamping each component to
    * `[0, 1]`.
    *
    * @param hexColor
    *   a CSS hex color string, e.g. `"#ff8800"` (with or without leading `#`)
    * @param intensity
    *   the glow intensity multiplier
    * @return
    *   `(r, g, b)` each in `[0, 1]`
    */
  def glowColor(hexColor: String, intensity: Double): (Double, Double, Double) = {
    val hex = hexColor.stripPrefix("#")
    val r = java.lang.Integer.parseInt(hex.substring(0, 2), 16)
    val g = java.lang.Integer.parseInt(hex.substring(2, 4), 16)
    val b = java.lang.Integer.parseInt(hex.substring(4, 6), 16)
    (
      (r * intensity / 255.0).min(1.0),
      (g * intensity / 255.0).min(1.0),
      (b * intensity / 255.0).min(1.0)
    )
  }

  /** Fade: clamp opacity to `[0, 1]`.
    *
    * @param opacity
    *   the raw opacity value
    * @return
    *   the clamped opacity
    */
  def clampOpacity(opacity: Double): Double =
    Math.max(0.0, Math.min(1.0, opacity))
}

/** Result of a single smoke particle's per-frame computation. */
case class SmokeFrameResult(
    x: Double,
    y: Double,
    rotZ: Double,
    opacity: Double,
    scale: Double
)
