package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, TransitionType}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

/** Creates a `Transition[F]` from a [[TransitionType]] descriptor. */
private[lote] object TransitionFactory {

  def create[F[_]: Temporal: Ref.Make](
      transitionType: TransitionType,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Transition[F] = transitionType match {
    case TransitionType.Grab(stepSize) =>
      GrabTransition.create(stepSize, console, ticker, animationSettings)

    case TransitionType.Morph =>
      MorphTransition.create(console, ticker, animationSettings)

    case TransitionType.Replace(replace) =>
      ReplaceTransition.create(replace, console, ticker, animationSettings)

    case TransitionType.FallingCharacters(gravity, selectAccelerator) =>
      FallingCharactersTransition.create(gravity, selectAccelerator, console, ticker, animationSettings)

    case TransitionType.Smoke =>
      SmokeTransition.create(SmokeTransition.Smoke, console, ticker, animationSettings)

    case TransitionType.Dissolve =>
      SmokeTransition.create(SmokeTransition.Dissolve, console, ticker, animationSettings)
  }
}
