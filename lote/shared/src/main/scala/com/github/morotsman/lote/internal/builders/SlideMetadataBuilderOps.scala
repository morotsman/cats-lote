package com.github.morotsman.lote.internal.builders

import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, SlidePosition, TransitionType}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}
import com.github.morotsman.lote.internal.interpreter.transition.{
  FallingCharactersTransition,
  GrabTransition,
  MorphTransition,
  ReplaceTransition,
  SmokeTransition,
  TransitionFactory,
  WithFallbackTransition
}

private[lote] trait SlideMetadataBuilderOps[F[_], Self] {

  protected def withTransition(transition: Transition[F]): Self

  protected def currentTransitionOpt: Option[Transition[F]]

  protected def withTitle(title: String): Self

  /** Sets an absolute position (clears any relative offset). */
  protected def withPosition(position: SlidePosition): Self

  /** Merges rotation / transparency into the current position without clearing relative offset. */
  protected def withPositionMerge(position: SlidePosition): Self

  /** Sets a relative offset from the previous slide (accumulates). */
  protected def withOffset(dx: Double, dy: Double, dz: Double): Self

  /** Sets a relative rotation offset from the previous slide (accumulates). */
  protected def withRotationOffset(drx: Double, dry: Double, drz: Double): Self

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

  final def withFallback(fallback: Transition[F])(implicit
      console: NConsole[F]
  ): Self =
    currentTransitionOpt match {
      case Some(primary) =>
        withTransition(WithFallbackTransition(primary, fallback, console))
      case None =>
        throw new IllegalStateException(
          "withFallback requires a transition to be set first (e.g. .smokeTransition().withFallback(...))"
        )
    }

  final def withFallback(fallback: TransitionType)(implicit
      temporal: Temporal[F],
      refMake: Ref.Make[F],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Self = {
    val _ = temporal
    withFallback(TransitionFactory.create(fallback, console, ticker, animationSettings))(console)
  }

  final def title(title: String): Self =
    withTitle(title)

  final def at(x: Double, y: Double, z: Double): Self =
    withPosition(SlidePosition(x, y, z))

  final def rotatedBy(rx: Double, ry: Double, rz: Double): Self =
    withPositionMerge(SlidePosition(rotX = rx, rotY = ry, rotZ = rz))

  final def position(pos: SlidePosition): Self =
    withPosition(pos)

  final def transparentBackground(): Self =
    withPositionMerge(SlidePosition(transparentBackground = true))

  /** Internal entry point for the adapter layer to call `withPositionMerge`. */
  private[lote] final def positionMerge(pos: SlidePosition): Self =
    withPositionMerge(pos)

  final def right(d: Double): Self = withOffset(d, 0, 0)
  final def left(d: Double): Self = withOffset(-d, 0, 0)
  final def down(d: Double): Self = withOffset(0, d, 0)
  final def up(d: Double): Self = withOffset(0, -d, 0)
  final def forward(d: Double): Self = withOffset(0, 0, d)
  final def back(d: Double): Self = withOffset(0, 0, -d)
  final def offset(dx: Double, dy: Double, dz: Double): Self = withOffset(dx, dy, dz)

  final def rotateX(degrees: Double): Self = withRotationOffset(degrees, 0, 0)
  final def rotateY(degrees: Double): Self = withRotationOffset(0, degrees, 0)
  final def rotateZ(degrees: Double): Self = withRotationOffset(0, 0, degrees)
}

