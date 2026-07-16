package com.github.morotsman.lote.api

/** Visual effects that can be applied by capable terminal backends (e.g., WebGL).
  *
  * Effects are fire-and-forget hints: backends that don't support them silently ignore the call. This lets transitions
  * produce richer visuals on capable platforms while falling back to plain character-grid animation elsewhere.
  *
  * Apply effects via `NConsole.applyEffect(effect)` inside a transition or slide.
  */
sealed trait RenderEffect

object RenderEffect {

  /** Flip the entire terminal view around the horizontal axis (like turning a page).
    *
    * @param progress
    *   animation progress from 0.0 (flat, showing current content) to 1.0 (flat, showing new content). At 0.5 the view
    *   is edge-on.
    */
  case class FlipHorizontal(progress: Double) extends RenderEffect

  /** Flip the entire terminal view around the vertical axis.
    *
    * @param progress
    *   animation progress from 0.0 to 1.0
    */
  case class FlipVertical(progress: Double) extends RenderEffect

  /** Dissolve the current frame — characters fade out with per-cell randomized timing.
    *
    * @param progress
    *   animation progress from 0.0 (fully visible) to 1.0 (fully dissolved)
    */
  case class Dissolve(progress: Double) extends RenderEffect

  /** Characters drift upward and fade, creating a smoke-like dissipation.
    *
    * @param progress
    *   animation progress from 0.0 (characters in place) to 1.0 (fully dissipated)
    */
  case class Smoke(progress: Double) extends RenderEffect

  /** Apply a glow / bloom effect around text.
    *
    * @param color
    *   CSS color for the glow (e.g., "#00ff00")
    * @param intensity
    *   glow strength from 0.0 (none) to 1.0 (maximum)
    */
  case class Glow(color: String, intensity: Double) extends RenderEffect

  /** Fade the entire view to/from a given opacity.
    *
    * @param opacity
    *   target opacity from 0.0 (transparent) to 1.0 (opaque)
    */
  case class Fade(opacity: Double) extends RenderEffect

  /** Rotate the entire terminal view around its center like a spinning panel.
    *
    * At progress 0.0 the current content is fully visible (0° rotation).
    * At progress 0.5 the panel is edge-on (90°) — content should be swapped here.
    * At progress 1.0 the new content is fully visible (180° rotation, i.e. the "back" now faces forward).
    *
    * @param progress
    *   animation progress from 0.0 to 1.0
    */
  case class Rotate(progress: Double) extends RenderEffect

  /** Reset / clear all active effects, returning to normal rendering. */
  case object ClearEffects extends RenderEffect
}

