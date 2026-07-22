package com.github.morotsman.lote.api

/** Describes a built-in transition without exposing implementation types.
  *
  * Pass a value to [[builders.SlideMetadataDsl.withFallback]] or to [[TransitionFactory.create]] to obtain a concrete
  * `Transition[F]`.
  */
sealed trait TransitionType extends Product with Serializable

object TransitionType {

  /** A snake crawls in and drags the content away. */
  final case class Grab(stepSize: Int = 2) extends TransitionType

  /** Characters morph in place through intermediate symbols. */
  case object Morph extends TransitionType

  /** Characters are replaced one by one with the given character. */
  final case class Replace(replace: Char) extends TransitionType

  /** Text falls off the screen with simulated gravity. */
  final case class FallingCharacters(
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2
  ) extends TransitionType

  /** Characters dissolve into smoke particles (WebGL), falling characters on terminal. */
  case object Smoke extends TransitionType

  /** Characters fade out with randomized timing (WebGL), falling characters on terminal. */
  case object Dissolve extends TransitionType
}
