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

  def title(title: String): Self
}
