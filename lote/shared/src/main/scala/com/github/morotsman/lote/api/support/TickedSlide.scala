package com.github.morotsman.lote.api.support

import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.all._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, TickerSubscription}

import scala.concurrent.duration.FiniteDuration

/** Eliminates the boilerplate around ticker subscription, FixedStep, and
  * GlideLayer lifecycle that every animated slide repeats.
  *
  * ==Minimal usage (no GlideLayer, no FixedStep)==
  * {{{
  * TickedSlide[F](console, ticker)
  *   .build(
  *     onTick  = myRenderLogic,
  *     onInput = myInputHandler,
  *     onStart = resetMyState
  *   )
  * }}}
  *
  * ==With FixedStep + GlideLayer==
  * {{{
  * TickedSlide[F](console, ticker, animationSettings)
  *   .withGlideLayer(wrapThreshold = 20)
  *   .buildWithGlide(
  *     onTick  = { (steps, glide) => ... },
  *     onInput = myInputHandler,
  *     onStart = resetMyState
  *   )
  * }}}
  *
  * ==Using the contextual factory (inside SessionBuilder)==
  * {{{
  * SessionBuilder[F]()
  *   .addSlideF {
  *     _.addSlideF(
  *       TickedSlide.contextual[F] { builder =>
  *         for {
  *           stateRef <- Ref[F].of(initialState)
  *           slide    <- builder.build(
  *             onTick  = renderLogic(stateRef),
  *             onInput = inputHandler(stateRef),
  *             onStart = stateRef.set(initialState)
  *           )
  *         } yield slide
  *       }
  *     )
  *   }
  * }}}
  */
object TickedSlide {

  // ── Builder entry point ──────────────────────────────────────────

  def apply[F[_]](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings = AnimationSettings.default
  ): Builder[F] = Builder(console, ticker, animationSettings, glideConfig = None)

  /** Create a slide using the `SlideContext` provided by the DSL builder.
    *
    * Symmetric with `TickedTransition.contextual` — the framework injects
    * `NConsole`, `Ticker`, and `AnimationSettings` so the caller only supplies
    * the slide-specific logic via the `Builder`.
    *
    * {{{
    * TickedSlide.contextual[F] { builder =>
    *   for {
    *     stateRef <- Ref[F].of(0)
    *     slide    <- builder.build(
    *       onTick  = renderLogic(stateRef),
    *       onInput = inputHandler(stateRef),
    *       onStart = stateRef.set(0)
    *     )
    *   } yield slide
    * }
    * }}}
    */
  def contextual[F[_]](f: Builder[F] => F[Slide[F]]): ContextualF[F, Slide[F]] =
    ContextualF { ctx =>
      f(Builder(ctx.console, ctx.ticker, ctx.animationSettings, glideConfig = None))
    }

  // ── Configuration ────────────────────────────────────────────────

  /** Configuration for GlideLayer creation. */
  final case class GlideConfig(
      step: Option[FiniteDuration] = None, // defaults to animationSettings.step
      wrapThreshold: Int = 1
  )

  final case class Builder[F[_]](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings,
      glideConfig: Option[GlideConfig]
  ) {

    def withGlideLayer(wrapThreshold: Int = 1): Builder[F] =
      copy(glideConfig = Some(GlideConfig(wrapThreshold = wrapThreshold)))

    def withGlideLayer(config: GlideConfig): Builder[F] =
      copy(glideConfig = Some(config))

    // ── Build variants ───────────────────────────────────────────

    /** Build a slide with a simple tick callback (no FixedStep, no GlideLayer).
      *
      * @param onTick  called every ticker tick
      * @param onInput called on user input
      * @param onStart called when the slide starts (reset custom state here)
      * @param onStop  optional extra cleanup when the slide stops
      */
    def build(
        onTick: F[Unit],
        onInput: UserInput => F[Unit],
        onStart: F[Unit],
        onStop: Option[F[Unit]] = None
    )(implicit F: Monad[F], refMake: Ref.Make[F]): F[Slide[F]] =
      Ref[F].of(Option.empty[TickerSubscription[F]]).map { subRef =>
        new Slide[F] {
          override def content: F[Option[ScreenAdjusted]] = F.pure(None)

          override def startShow: F[Unit] =
            for {
              _ <- onStart
              sub <- ticker.subscribe(onTick)
              _ <- subRef.set(Some(sub))
              _ <- ticker.start
            } yield ()

          override def stopShow: F[Unit] =
            for {
              maybeSub <- subRef.get
              _ <- maybeSub.traverse_(_.cancel)
              _ <- subRef.set(None)
              _ <- onStop.getOrElse(F.unit)
            } yield ()

          override def userInput(input: UserInput): F[Unit] = onInput(input)
        }
      }

    /** Build a slide with FixedStep (discrete simulation steps).
      *
      * The `onTick` callback receives `(steps: Int, progress: Double)` —
      * the number of simulation steps elapsed and the fractional progress
      * toward the next step.
      *
      * @param onTick  called every ticker tick with `(steps, progress)`
      * @param onInput called on user input
      * @param onStart called when the slide starts (reset custom state here)
      * @param onStop  optional extra cleanup when the slide stops
      */
    def buildStepped(
        onTick: (Int, Double) => F[Unit],
        onInput: UserInput => F[Unit],
        onStart: F[Unit],
        onStop: Option[F[Unit]] = None
    )(implicit F: Monad[F], refMake: Ref.Make[F], clock: AnimationClock[F]): F[Slide[F]] =
      for {
        subRef     <- Ref[F].of(Option.empty[TickerSubscription[F]])
        stepperRef <- FixedStep.makeRef[F]
      } yield new Slide[F] {
        private val tickerCallback: F[Unit] =
          FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap {
            case (steps, progress) => onTick(steps, progress)
          }

        override def content: F[Option[ScreenAdjusted]] = F.pure(None)

        override def startShow: F[Unit] =
          for {
            _ <- onStart
            _ <- FixedStep.reset(stepperRef)
            sub <- ticker.subscribe(tickerCallback)
            _ <- subRef.set(Some(sub))
            _ <- ticker.start
          } yield ()

        override def stopShow: F[Unit] =
          for {
            maybeSub <- subRef.get
            _ <- maybeSub.traverse_(_.cancel)
            _ <- subRef.set(None)
            _ <- onStop.getOrElse(F.unit)
          } yield ()

        override def userInput(input: UserInput): F[Unit] = onInput(input)
      }

    /** Build a slide with FixedStep and a GlideLayer.
      *
      * The `onTick` callback receives `(steps: Int, glide: GlideLayer.Overlay[F])`.
      * The GlideLayer is automatically cleared on `stopShow`.
      *
      * @param onTick  called every ticker tick with `(steps, glideLayer)`
      * @param onInput called on user input
      * @param onStart called when the slide starts (reset custom state here)
      * @param onStop  optional extra cleanup (GlideLayer.clear is automatic)
      */
    def buildWithGlide(
        onTick: (Int, GlideLayer.Overlay[F]) => F[Unit],
        onInput: UserInput => F[Unit],
        onStart: F[Unit],
        onStop: Option[F[Unit]] = None
    )(implicit F: Monad[F], refMake: Ref.Make[F], clock: AnimationClock[F]): F[Slide[F]] = {
      val gc = glideConfig.getOrElse(GlideConfig())
      val glideStep = gc.step.getOrElse(animationSettings.step)
      for {
        subRef     <- Ref[F].of(Option.empty[TickerSubscription[F]])
        stepperRef <- FixedStep.makeRef[F]
        glide      <- GlideLayer.make[F](console, glideStep, gc.wrapThreshold)
      } yield new Slide[F] {
        private val tickerCallback: F[Unit] =
          FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap {
            case (steps, _) => onTick(steps, glide)
          }

        override def content: F[Option[ScreenAdjusted]] = F.pure(None)

        override def startShow: F[Unit] =
          for {
            _ <- onStart
            _ <- FixedStep.reset(stepperRef)
            sub <- ticker.subscribe(tickerCallback)
            _ <- subRef.set(Some(sub))
            _ <- ticker.start
          } yield ()

        override def stopShow: F[Unit] =
          for {
            maybeSub <- subRef.get
            _ <- maybeSub.traverse_(_.cancel)
            _ <- subRef.set(None)
            _ <- glide.clear()
            _ <- onStop.getOrElse(F.unit)
          } yield ()

        override def userInput(input: UserInput): F[Unit] = onInput(input)
      }
    }
  }
}
