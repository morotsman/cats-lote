package com.github.morotsman.lote.api.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.SlidePosition
import com.github.morotsman.lote.api.spi.Transition

trait SlideMetadataDsl[F[_], Self] {
  def slideContext: SlideContext[F]

  /** Sets the 3D world-space position of this slide.
    *
    * On WebGL backends the camera will navigate to this position when the slide becomes active.
    * On terminal backends this is a no-op.
    */
  def at(x: Double, y: Double, z: Double): Self

  /** Sets the 3D rotation (Euler angles in degrees) of this slide's surface.
    *
    * On WebGL backends the slide surface is rotated accordingly.
    * On terminal backends this is a no-op.
    */
  def rotatedBy(rx: Double, ry: Double, rz: Double): Self

  /** Sets the full slide position (position + rotation) in one call. */
  def position(pos: SlidePosition): Self

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

  def title(title: String): Self

  /** Makes this slide's background transparent in spatial mode (WebGL).
    * This allows 3D scene content behind the slide to show through.
    */
  def transparentBackground(): Self
}
