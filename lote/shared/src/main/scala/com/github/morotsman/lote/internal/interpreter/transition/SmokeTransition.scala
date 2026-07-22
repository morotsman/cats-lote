package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, PlatformCapability, RenderEffect, UserInput}
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}

/** A transition that dissolves the current slide with a smoke/dissolve effect on WebGL.
  *
  * On platforms that support `Effects`, characters fade out and drift upward (smoke) or simply dissolve (dissolve)
  * before the next slide appears.
  *
  * On terminal-only platforms, falls back to the built-in FallingCharactersTransition.
  */
private[lote] object SmokeTransition {

  sealed trait Style
  case object Smoke extends Style
  case object Dissolve extends Style

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      style: Style = Smoke
  )(implicit animationSettings: AnimationSettings): Transition[F] =
    create(
      style = style,
      console = NConsole[F],
      ticker = Ticker[F],
      animationSettings = animationSettings
    )

  def create[F[_]: Temporal: Ref.Make](
      style: Style = Smoke,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] = new Transition[F] {

    private val supportsEffects: Boolean =
      console.capabilities.contains(PlatformCapability.Effects)

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      if (supportsEffects)
        effectfulTransition(from, to)
      else
        fallbackTransition(from, to)
    }

    /** GPU-accelerated smoke/dissolve transition. */
    private def effectfulTransition(from: Slide[F], to: Slide[F]): F[Unit] = {
      val totalSteps = 30
      val stepIncrement = 1.0 / totalSteps

      TickedTransition(console, ticker, animationSettings)
        .buildWithSetup { (slide1, _slide2, complete) =>
          for {
            progressRef <- Ref[F].of(0.0)
            _           <- console.writeString(slide1)
          } yield TickedTransition.TickHandler(
            onTick = (nrOfSteps: Int, _progress: Double) => {
              for {
                progress <- progressRef.get
                newProgress = Math.min(1.0, progress + stepIncrement * nrOfSteps)
                _ <- progressRef.set(newProgress)
                _ <-
                  if (newProgress >= 1.0) {
                    console.applyEffect(RenderEffect.ClearEffects) *>
                      console.clear() *>
                      complete
                  } else {
                    val effect = style match {
                      case Smoke    => RenderEffect.Smoke(newProgress)
                      case Dissolve => RenderEffect.Dissolve(newProgress)
                    }
                    console.applyEffect(effect)
                  }
              } yield ()
            },
            cleanup = console.applyEffect(RenderEffect.ClearEffects)
          )
        }
        .transition(from, to)
    }

    /** Fallback: use falling characters on terminal platforms. */
    private def fallbackTransition(from: Slide[F], to: Slide[F]): F[Unit] = {
      FallingCharactersTransition
        .create(
          console = console,
          ticker = ticker,
          animationSettings = animationSettings
        )
        .transition(from, to)
    }

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }
}
