package com.github.morotsman.lote.internal.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.AnimationSettings
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}
import com.github.morotsman.lote.internal.interpreter.transition.{
  FallingCharactersTransition,
  GrabTransition,
  MorphTransition,
  ReplaceTransition
}

private[lote] trait SlideMetadataBuilderOps[F[_], Self] {

  protected def withTransition(transition: Transition[F]): Self

  protected def withTitle(title: String): Self

  final def transition(transition: Transition[F]): Self =
    withTransition(transition)

  final def morphTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(MorphTransition.create(console, ticker, animationSettings))
  }

  final def replaceTransition(replace: Char)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(ReplaceTransition.create(replace, console, ticker, animationSettings))
  }

  final def fallingCharactersTransition(
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2
  )(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(
      FallingCharactersTransition.create(
        gravity,
        selectAccelerator,
        console,
        ticker,
        animationSettings
      )
    )
  }

  final def grabTransition(stepSize: Int = 2)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(GrabTransition.create(stepSize, console, ticker, animationSettings))
  }

  final def title(title: String): Self =
    withTitle(title)
}
