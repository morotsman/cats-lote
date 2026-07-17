package com.github.morotsman.lote.internal.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, SlidePosition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}
import com.github.morotsman.lote.internal.interpreter.transition.{
  FallingCharactersTransition,
  GrabTransition,
  MorphTransition,
  ReplaceTransition,
  SmokeTransition
}

private[lote] trait SlideMetadataBuilderOps[F[_], Self] {

  protected def withTransition(transition: Transition[F]): Self

  protected def withTitle(title: String): Self

  protected def withPosition(position: SlidePosition): Self

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


  final def smokeTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(SmokeTransition.create(SmokeTransition.Smoke, console, ticker, animationSettings))
  }

  final def dissolveTransition()(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withTransition(SmokeTransition.create(SmokeTransition.Dissolve, console, ticker, animationSettings))
  }


  final def title(title: String): Self =
    withTitle(title)

  final def at(x: Double, y: Double, z: Double): Self =
    withPosition(SlidePosition(x, y, z))

  final def rotatedBy(rx: Double, ry: Double, rz: Double): Self =
    withPosition(SlidePosition(rotX = rx, rotY = ry, rotZ = rz))

  final def position(pos: SlidePosition): Self =
    withPosition(pos)
}
