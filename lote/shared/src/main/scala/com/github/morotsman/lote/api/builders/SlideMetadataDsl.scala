package com.github.morotsman.lote.api.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.spi.Transition

trait SlideMetadataDsl[F[_], Self] {
  def slideContext: SlideContext[F]

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

  /** 3D flip transition (WebGL) — falls back to replace on terminal backends. */
  def flipTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  /** Vertical 3D flip transition (WebGL) — falls back to replace on terminal backends. */
  def flipVerticalTransition()(implicit
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

  /** Rotate transition (WebGL) — the slide spins like a panel to reveal the new slide on the other side.
    * Falls back to replace on terminal backends.
    */
  def rotateTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F]
  ): Self

  def title(title: String): Self
}
