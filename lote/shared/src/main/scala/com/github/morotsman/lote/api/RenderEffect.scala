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


  /** Move the camera to a new position in 3D space with smooth ease-in-out animation.
    *
    * On WebGL backends this triggers a camera flight from the current position to the target.
    * On terminal backends this is a no-op.
    *
    * @param target
    *   the target slide position (world-space coordinates and rotation)
    */
  case class MoveCameraTo(target: SlidePosition) extends RenderEffect

  /** Initialize spatial layout mode: create one rendering layer per slide, positioned in 3D space.
    *
    * On WebGL backends this creates separate textured planes for each slide.
    * On terminal backends this is a no-op.
    *
    * @param positions
    *   the world-space position for each slide (by index). `None` means the slide shares
    *   the position of the previous slide.
    */
  case class InitSpatialLayout(positions: Vector[Option[SlidePosition]]) extends RenderEffect

  /** Activate a specific slide layer for subsequent `write()` calls.
    *
    * On WebGL backends in spatial mode, `write()` will render to the specified layer's texture.
    * On terminal backends this is a no-op.
    *
    * @param index
    *   the slide index (0-based)
    */
  case class ActivateLayer(index: Int) extends RenderEffect

  /** Reset / clear all active effects, returning to normal rendering. */
  case object ClearEffects extends RenderEffect
}

