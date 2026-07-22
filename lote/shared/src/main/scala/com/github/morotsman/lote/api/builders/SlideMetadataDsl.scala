package com.github.morotsman.lote.api.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{SlidePosition, TransitionType}
import com.github.morotsman.lote.api.spi.Transition

trait SlideMetadataDsl[F[_], Self] {
  def slideContext: SlideContext[F]

  /** Sets the 3D world-space position of this slide.
    *
    * Coordinates are in CSS-pixel-scale world units. A typical 80×24 slide surface is 800 × 480 units (based on
    * `WebGLConfig.cellWidth` × `cellHeight`). Use spacing of ~1800 horizontally and ~1200–2400 vertically to avoid
    * overlap. Positive x = right, positive y = down, positive z = toward camera.
    *
    * On WebGL backends the camera will navigate to this position when the slide becomes active. On terminal backends
    * this is a no-op.
    *
    * @param x
    *   world-space X position in CSS-pixel-scale units
    * @param y
    *   world-space Y position in CSS-pixel-scale units
    * @param z
    *   world-space Z position (positive = toward camera, negative = away)
    *
    * @see
    *   [[com.github.morotsman.lote.api.SlidePosition SlidePosition]] for full coordinate system documentation
    */
  def at(x: Double, y: Double, z: Double): Self

  /** Sets the 3D rotation (Euler angles in degrees) of this slide's surface.
    *
    * On WebGL backends the slide surface is rotated accordingly. On terminal backends this is a no-op.
    */
  def rotatedBy(rx: Double, ry: Double, rz: Double): Self

  /** Sets the full slide position (position + rotation) in one call. */
  def position(pos: SlidePosition): Self

  /** Moves this slide to the right of the previous slide by `d` world-space units.
    *
    * Relative positioning methods accumulate: `.right(1000).down(500)` produces offset `(1000, 500, 0)`. On WebGL
    * backends the camera navigates to the computed position. On terminal backends this is a no-op.
    */
  def right(d: Double): Self

  /** Moves this slide to the left of the previous slide by `d` world-space units. */
  def left(d: Double): Self

  /** Moves this slide down from the previous slide by `d` world-space units. */
  def down(d: Double): Self

  /** Moves this slide up from the previous slide by `d` world-space units. */
  def up(d: Double): Self

  /** Moves this slide toward the camera from the previous slide by `d` world-space units. */
  def forward(d: Double): Self

  /** Moves this slide away from the camera from the previous slide by `d` world-space units. */
  def back(d: Double): Self

  /** Sets a general relative offset from the previous slide's position. Accumulates with other offset calls. */
  def offset(dx: Double, dy: Double, dz: Double): Self

  /** Adds a relative rotation around the X axis (in degrees) on top of the previous slide's rotation. */
  def rotateX(degrees: Double): Self

  /** Adds a relative rotation around the Y axis (in degrees) on top of the previous slide's rotation. */
  def rotateY(degrees: Double): Self

  /** Adds a relative rotation around the Z axis (in degrees) on top of the previous slide's rotation. */
  def rotateZ(degrees: Double): Self

  def transition(transition: Transition[F]): Self

  def transition(transition: Contextual[F, Transition[F]]): Self

  def morphTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  def replaceTransition(replace: Char)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  def fallingCharactersTransition(
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2
  )(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  def grabTransition(stepSize: Int = 2)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  /** Smoke dissolve transition (WebGL) — falls back to falling characters on terminal backends. */
  def smokeTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  /** Dissolve/fade-out transition (WebGL) — falls back to falling characters on terminal backends. */
  def dissolveTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  /** Overrides the default terminal fallback of the current transition.
    *
    * When the platform supports `Effects` (e.g. WebGL) the previously configured transition runs unchanged. On
    * terminal-only platforms the given `fallback` transition is used instead.
    *
    * Must be called after a transition has been set (e.g. `.smokeTransition().withFallback(...)`).
    */
  def withFallback(fallback: Transition[F]): Self

  /** Contextual variant of [[withFallback]] that receives a [[SlideContext]] for constructing the fallback transition.
    */
  def withFallback(fallback: Contextual[F, Transition[F]]): Self

  /** Overrides the default terminal fallback using a built-in [[TransitionType]].
    *
    * {{{
    * .smokeTransition()
    * .withFallback(TransitionType.Grab())
    * }}}
    */
  def withFallback(fallback: TransitionType)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  def title(title: String): Self

  /** Makes this slide's background transparent in spatial mode (WebGL). This allows 3D scene content behind the slide
    * to show through.
    */
  def transparentBackground(): Self
}
