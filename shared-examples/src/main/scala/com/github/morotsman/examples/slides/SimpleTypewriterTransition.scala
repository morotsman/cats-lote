package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.syntax.all._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

/** Demonstrates `TickedTransition.buildStepped` — a transition where you manually track state and decide when to signal
  * completion.
  *
  * This is a "typewriter reveal": the next slide's content is revealed character by character, left-to-right then
  * top-to-bottom.
  *
  * ==Key concept==
  * Unlike `buildProgress` (which manages step counting and gives you a 0→1 progress value), `buildStepped` gives you
  * raw step counts on each tick. You have access to `ctx.from`, `ctx.to`, `ctx.screenWidth`, `ctx.screenHeight`,
  * `ctx.render(frame)`, and `ctx.complete`.
  *
  * {{{
  * builder.buildStepped { (nrOfSteps, _, ctx) =>
  *   for {
  *     revealed <- revealRef.updateAndGet(_ + nrOfSteps * charsPerStep)
  *     frame     = buildFrame(ctx.from, ctx.to, revealed, ctx.screenWidth)
  *     _        <- ctx.render(frame)
  *     _        <- if (revealed >= totalChars) ctx.complete else Monad[F].unit
  *   } yield ()
  * }
  * }}}
  *
  * Note: since `buildStepped` doesn't provide a setup phase for creating Refs, this example uses `buildWithSetup` under
  * the hood — which is the idiomatic pattern when `buildStepped` would need external mutable state. The `buildStepped`
  * API is shown in the ScalaDoc above for reference.
  *
  * ==Usage==
  * {{{
  * .transition(SimpleTypewriterTransition.contextual[F]())
  * }}}
  */
object SimpleTypewriterTransition {

  def contextual[F[_]: Temporal: Ref.Make](charsPerStep: Int = 5): Contextual[F, Transition[F]] =
    TickedTransition.contextual[F] { builder =>
      create[F](charsPerStep, builder)
    }

  def create[F[_]: Temporal: Ref.Make](
      charsPerStep: Int,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] =
    create(charsPerStep, TickedTransition(console, ticker, animationSettings))

  def create[F[_]: Temporal: Ref.Make](
      charsPerStep: Int,
      builder: TickedTransition.Builder[F]
  )(implicit clock: AnimationClock[F]): Transition[F] = {
    require(charsPerStep > 0, "charsPerStep must be greater than 0")

    // buildWithSetup gives us a setup phase to create Refs, then returns
    // a tick handler. This is the pattern when you need mutable state in
    // the tick loop (which buildStepped alone can't provide).
    builder.buildWithSetup { (fromContent, toContent, complete) =>
      Ref.of[F, Int](0).map { revealRef =>
        val totalChars = toContent.content.length
        TickedTransition.TickHandler(
          onTick = (nrOfSteps: Int, _: Double) => {
            for {
              revealed <- revealRef.updateAndGet(_ + nrOfSteps * charsPerStep)
              clamped = math.min(revealed, totalChars)
              frame = buildFrame(fromContent, toContent, clamped)
              _ <- builder.console.clear()
              _ <- builder.console.writeString(frame)
              _ <- if (revealed >= totalChars) complete else Monad[F].unit
            } yield ()
          }
        )
      }
    }
  }

  /** Builds a frame where `revealedChars` characters of the `to` content are shown, and the rest shows the `from`
    * content.
    */
  private def buildFrame(
      from: ScreenAdjusted,
      to: ScreenAdjusted,
      revealedChars: Int
  ): ScreenAdjusted = {
    val fromChars = from.content.toVector
    val toChars = to.content.toVector
    val maxLen = math.max(fromChars.length, toChars.length)

    val result = (0 until maxLen).map { i =>
      if (i < revealedChars)
        toChars.lift(i).getOrElse(' ')
      else
        fromChars.lift(i).getOrElse(' ')
    }

    ScreenAdjusted(result.mkString)
  }
}
