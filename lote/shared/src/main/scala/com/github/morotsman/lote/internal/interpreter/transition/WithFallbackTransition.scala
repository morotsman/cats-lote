package com.github.morotsman.lote.internal.interpreter.transition

import com.github.morotsman.lote.api.{PlatformCapability, UserInput}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Transition}

/** Wraps a primary transition with a user-specified fallback.
  *
  * When the platform supports `Effects` the primary transition runs as-is. Otherwise the fallback transition is used
  * instead, overriding any default fallback that the primary transition may contain internally.
  */
private[lote] object WithFallbackTransition {

  def apply[F[_]](
      primary: Transition[F],
      fallback: Transition[F],
      console: NConsole[F]
  ): Transition[F] = new Transition[F] {

    private val supportsEffects: Boolean =
      console.capabilities.contains(PlatformCapability.Effects)

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      if (supportsEffects) primary.transition(from, to)
      else fallback.transition(from, to)

    override def userInput(input: UserInput): F[Unit] =
      if (supportsEffects) primary.userInput(input)
      else fallback.userInput(input)
  }
}
