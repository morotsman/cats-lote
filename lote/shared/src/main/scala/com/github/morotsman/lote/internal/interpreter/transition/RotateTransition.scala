package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, PlatformCapability, RenderEffect, UserInput}
import com.github.morotsman.lote.api.support.FixedStep
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}

/** A transition that rotates the terminal view like a spinning panel to reveal the next slide.
  *
  * On platforms that support `Transforms3D`, the current slide rotates around a vertical axis
  * until edge-on, the content is swapped, and the panel continues rotating to show the new slide.
  *
  * On terminal-only platforms (JLine, xterm.js), falls back to a simple replace.
  */
private[lote] object RotateTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker]()(implicit
      animationSettings: AnimationSettings
  ): Transition[F] =
    create(
      console = NConsole[F],
      ticker = Ticker[F],
      animationSettings = animationSettings
    )

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Transition[F] = new Transition[F] {

    private val supportsEffects: Boolean =
      console.capabilities.contains(PlatformCapability.Transforms3D)

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      if (supportsEffects)
        effectfulTransition(from, to)
      else
        fallbackTransition(from, to)
    }

    private def effectfulTransition(from: Slide[F], to: Slide[F]): F[Unit] = {
      for {
        slide1 <- from.content
        slide2 <- to.content
        stepperRef <- FixedStep.makeRef[F]
        done <- Deferred[F, Unit]
        progressRef <- Ref[F].of(0.0)
        _ <- console.writeString(slide1)

        totalSteps = 40
        stepIncrement = 1.0 / totalSteps

        onTick = { (nrOfSteps: Int) =>
          for {
            progress <- progressRef.get
            newProgress = Math.min(1.0, progress + stepIncrement * nrOfSteps)
            _ <- progressRef.set(newProgress)
            _ <-
              if (newProgress >= 1.0) {
                console.applyEffect(RenderEffect.ClearEffects) *>
                  console.clear() *>
                  done.complete(()).attempt.void
              } else if (newProgress >= 0.5 && progress < 0.5) {
                // Midpoint — swap to new content while edge-on
                console.clear() *>
                  console.writeString(slide2) *>
                  console.applyEffect(RenderEffect.Rotate(newProgress))
              } else {
                console.applyEffect(RenderEffect.Rotate(newProgress))
              }
          } yield ()
        }

        tickerCallback = FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap(onTick)
        sub <- ticker.subscribe(tickerCallback)
        _ <- ticker.start
        _ <- done.get.guarantee(sub.cancel *> console.applyEffect(RenderEffect.ClearEffects))
        _ <- console.writeString(slide2)
      } yield ()
    }

    private def fallbackTransition(from: Slide[F], to: Slide[F]): F[Unit] = {
      val _ = from
      for {
        slide2 <- to.content
        _ <- console.clear()
        _ <- console.writeString(slide2)
      } yield ()
    }

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }
}

