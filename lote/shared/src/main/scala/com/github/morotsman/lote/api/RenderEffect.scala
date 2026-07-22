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
    * On WebGL backends this triggers a camera flight from the current position to the target and semantically blocks
    * until the animation completes. On terminal backends this is a no-op.
    *
    * @param target
    *   the target slide position (world-space coordinates and rotation)
    */
  case class MoveCameraTo(target: SlidePosition) extends RenderEffect

  /** Jump the camera instantly to a position (no animation).
    *
    * Used to skip a camera flight when the user interrupts navigation. On terminal backends this is a no-op.
    *
    * @param target
    *   the target slide position (world-space coordinates and rotation)
    */
  case class JumpCameraTo(target: SlidePosition) extends RenderEffect

  /** Initialize spatial layout mode: create one rendering layer per slide, positioned in 3D space.
    *
    * On WebGL backends this creates separate textured planes for each slide. On terminal backends this is a no-op.
    *
    * @param positions
    *   the world-space position for each slide (by index). `None` means the slide shares the position of the previous
    *   slide.
    */
  case class InitSpatialLayout(positions: Vector[Option[SlidePosition]]) extends RenderEffect

  /** Activate a specific slide layer for subsequent `write()` calls.
    *
    * On WebGL backends in spatial mode, `write()` will render to the specified layer's texture. On terminal backends
    * this is a no-op.
    *
    * @param index
    *   the slide index (0-based)
    */
  case class ActivateLayer(index: Int) extends RenderEffect

  /** Reset / clear all active effects, returning to normal rendering. */
  case object ClearEffects extends RenderEffect

  /** A single character to be rendered at a sub-pixel position on a transparent overlay.
    *
    * @param char
    *   the character to draw
    * @param cellX
    *   horizontal position in character cell units (fractional, e.g. 5.3 = 5.3 cells from the left)
    * @param cellY
    *   vertical position in character cell units (fractional, e.g. 2.7 = 2.7 cells from the top)
    * @param fgColor
    *   CSS color for the character foreground
    */
  case class FloatingChar(char: Char, cellX: Double, cellY: Double, fgColor: String)

  /** Render characters at sub-pixel positions on a transparent overlay above the character grid.
    *
    * On WebGL backends this draws each character at a floating-point canvas position, enabling smooth motion that is
    * impossible in the integer-cell character grid. The overlay is automatically layered above the slide's texture
    * mesh.
    *
    * Use this for game elements (like a worm/snake) that need smooth per-frame interpolation between discrete
    * simulation positions.
    *
    * On terminal backends this renders each character at the nearest integer grid position (rounded from the
    * fractional coordinates). The motion will appear quantized to cell boundaries but still functional.
    *
    * @param chars
    *   the characters to render at sub-pixel positions
    */
  case class RenderFloatingChars(chars: Vector[FloatingChar]) extends RenderEffect

  /** Remove the floating-chars overlay, cleaning up its canvas and mesh. */
  case object ClearFloatingChars extends RenderEffect

  /** Set a sub-pixel canvas drawing offset for the active slide layer.
    *
    * On WebGL backends this applies a `canvas.translate()` before rendering subsequent `write()` calls, shifting the
    * character grid content by a fractional cell amount. The mesh position is not affected, so overlays (timer,
    * progress bar) stay in place. Use this for smooth scene scrolling (e.g. a scrolling landscape).
    *
    * The offset persists until changed or reset to (0, 0). On terminal backends this is a no-op.
    *
    * @param cellsX
    *   horizontal offset in character cell units (e.g., -0.3 = shift content left by 30% of a cell)
    * @param cellsY
    *   vertical offset in character cell units
    */
  case class SetCanvasOffset(cellsX: Double, cellsY: Double) extends RenderEffect

  /** Mark specific rows as "fixed" — not affected by `SetCanvasOffset`.
    *
    * When a canvas offset is active, these rows are rendered in a second pass without the translate so that overlay
    * elements like a timer or progress bar stay in place while the scene content scrolls.
    *
    * Rows are added to the existing set (additive). Use `ClearFixedRows` to reset. On terminal backends this is a
    * no-op.
    *
    * @param rows
    *   the 0-based row indices that should remain fixed on screen
    */
  case class SetFixedRows(rows: Set[Int]) extends RenderEffect

  /** Clear all fixed rows, so that all rows are affected by `SetCanvasOffset` again. */
  case object ClearFixedRows extends RenderEffect
}
