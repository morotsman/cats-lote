package com.github.morotsman.lote.api.support

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.syntax.all._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.builders.Contextual
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.testkit.SlideTestHarness

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.FiniteDuration

/** Eliminates the boilerplate around ticker subscription, FixedStep, Deferred-based completion signalling, and
  * GlideLayer lifecycle that every animated transition repeats.
  *
  * ==Minimal usage (progress-based)==
  * {{{
  * import scala.concurrent.duration._
  *
  * TickedTransition[F](console, ticker, animationSettings)
  *   .buildProgress(500.millis) { (from, to, ctx) =>
  *     // ctx.progress is 0.0–1.0; ctx.screenWidth/screenHeight are actual dimensions
  *     val frame = blendFrame(from, to, ctx.progress, ctx.screenWidth)
  *     if (visuallyDone) ProgressResult.done(frame)
  *     else              ProgressResult.continue(frame)
  *   }
  * }}}
  *
  * ==Stepped usage (manual state control)==
  * {{{
  * TickedTransition[F](console, ticker, animationSettings)
  *   .buildStepped { ctx =>
  *     // ctx provides from/to content, step info, done signal
  *     for {
  *       _ <- ctx.render(frame)
  *       _ <- if (finished) ctx.complete else Monad[F].unit
  *     } yield ()
  *   }
  * }}}
  *
  * ==Contextual (for use in DSL builders)==
  * {{{
  * TickedTransition.contextual[F] { builder =>
  *   builder.buildProgress(500.millis) { (from, to, ctx) =>
  *     val frame = blendFrame(from, to, ctx.progress, ctx.screenWidth)
  *     ProgressResult.continue(frame)
  *   }
  * }
  * }}}
  *
  * ==Easing==
  * {{{
  * TickedTransition[F](console, ticker, animationSettings)
  *   .withEasing(Easing.easeInOutCubic)
  *   .buildProgress(800.millis) { (from, to, ctx) =>
  *     // ctx.progress is already eased
  *     val frame = blendFrame(from, to, ctx.progress)
  *     ProgressResult.continue(frame)
  *   }
  * }}}
  *
  * ==Skip on input==
  * {{{
  * TickedTransition[F](console, ticker, animationSettings)
  *   .withSkipOnInput
  *   .buildProgress(2.seconds) { (from, to, ctx) =>
  *     ProgressResult.continue(blendFrame(from, to, ctx.progress))
  *   }
  * // pressing any key during the transition jumps to the end
  * }}}
  *
  * ==Testing==
  * {{{
  * for {
  *   harness    <- SlideTestHarness.make[IO](screen = Screen(80, 24))
  *   transition  = TickedTransition.forTest(harness)
  *                   .buildProgress(500.millis) { (from, to, ctx) =>
  *                     ProgressResult.continue(blendFrame(from, to, ctx.progress))
  *                   }
  *   _          <- harness.runWithTicking(transition.transition(fromSlide, toSlide))
  * } yield ()
  * }}}
  */
object TickedTransition {

  // ── Builder entry point ──────────────────────────────────────────

  def apply[F[_]](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings = AnimationSettings.default
  ): Builder[F] = Builder(console, ticker, animationSettings, glideConfig = None, easingFn = None, skipOnInput = false)

  /** Create a transition using the SlideContext provided by the DSL builder. */
  def contextual[F[_]](f: Builder[F] => Transition[F]): Contextual[F, Transition[F]] =
    Contextual { ctx =>
      f(
        Builder(
          ctx.console,
          ctx.ticker,
          ctx.animationSettings,
          glideConfig = None,
          easingFn = None,
          skipOnInput = false
        )
      )
    }

  /** Create a pre-wired builder from a `SlideTestHarness`, eliminating the need to manually thread `AnimationClock[F]`
    * and other implicits in tests.
    *
    * The harness provides `AnimationClock[F]`, `NConsole[F]`, `Ticker[F]`, and `AnimationSettings` — so you don't need
    * to import or wire any of them.
    *
    * ==Example==
    * {{{
    * for {
    *   harness    <- SlideTestHarness.make[IO](screen = Screen(80, 24))
    *   transition  = TickedTransition.forTest(harness)
    *                   .buildProgress(500.millis) { (from, to, ctx) =>
    *                     ProgressResult.continue(blendFrame(from, to, ctx.progress))
    *                   }
    *   _          <- harness.runWithTicking(
    *                   transition.transition(fromSlide, toSlide)
    *                 )
    *   last       <- harness.lastWrittenFrame
    * } yield assert(last.exists(_.contains("TO")))
    * }}}
    */
  def forTest[F[_]](harness: SlideTestHarness[F]): Builder[F] =
    Builder(
      harness.console,
      harness.ticker,
      harness.animationSettings,
      glideConfig = None,
      easingFn = None,
      skipOnInput = false
    )

  // ── Configuration ────────────────────────────────────────────────

  /** Configuration for optional GlideLayer usage. */
  final case class GlideConfig(
      wrapThreshold: Int = 1
  )

  /** Context passed to the `buildProgress` render callback.
    *
    * Bundles the animation progress together with the actual screen dimensions, eliminating the need to hardcode
    * estimated widths/heights.
    */
  final case class ProgressContext(
      /** Animation progress from 0.0 (start) to 1.0 (complete). If an easing function is set, this value is already
        * eased.
        */
      progress: Double,
      /** Actual screen width in characters. */
      screenWidth: Int,
      /** Actual screen height in characters. */
      screenHeight: Int
  )

  /** Result of a progress-based render callback that can signal early completion.
    *
    * Use [[ProgressResult.continue]] to emit a frame and keep animating, or [[ProgressResult.done]] to emit a final
    * frame and complete the transition immediately — useful when the visual effect finishes before the duration expires
    * (e.g. a wipe over short content).
    */
  sealed trait ProgressResult {
    def frame: ScreenAdjusted
    def isDone: Boolean
  }

  object ProgressResult {

    /** Emit the frame and continue animating. */
    def continue(frame: ScreenAdjusted): ProgressResult = Continue(frame)

    /** Emit the frame and signal that the transition is complete. */
    def done(frame: ScreenAdjusted): ProgressResult = Done(frame)

    private final case class Continue(frame: ScreenAdjusted) extends ProgressResult {
      val isDone = false
    }

    private final case class Done(frame: ScreenAdjusted) extends ProgressResult {
      val isDone = true
    }
  }

  /** Context passed to the stepped tick callback. */
  trait StepContext[F[_]] {
    def from: ScreenAdjusted
    def to: ScreenAdjusted
    def screenWidth: Int
    def screenHeight: Int

    /** Render a blended frame (clear + write). */
    def render(frame: ScreenAdjusted): F[Unit]

    /** Signal that the transition is complete. Safe to call multiple times. */
    def complete: F[Unit]
  }

  final case class Builder[F[_]](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings,
      glideConfig: Option[GlideConfig],
      easingFn: Option[Double => Double],
      skipOnInput: Boolean
  ) {

    def withGlideLayer(wrapThreshold: Int = 1): Builder[F] =
      copy(glideConfig = Some(GlideConfig(wrapThreshold = wrapThreshold)))

    /** Apply an easing function to the linear progress value before it is passed to `renderFrame` in `buildProgress`
      * and `buildProgressWithGlide`.
      *
      * Has no effect on `buildStepped` or `buildWithSetup` (which don't use the built-in progress calculation).
      *
      * Common easing functions are provided in [[Easing]]:
      * {{{
      * builder.withEasing(Easing.easeInOutCubic)
      *   .buildProgress(800.millis) { (from, to, ctx) => ... }
      * }}}
      */
    def withEasing(f: Double => Double): Builder[F] =
      copy(easingFn = Some(f))

    /** When enabled, any `userInput` event will immediately skip (fast-forward) the transition to its final state. The
      * final `to` frame is rendered automatically.
      *
      * Works with `buildProgress`, `buildProgressWithGlide`, `buildStepped`, and `buildWithSetup`.
      *
      * {{{
      * builder.withSkipOnInput
      *   .buildProgress(2.seconds) { (from, to, ctx) => ... }
      * // pressing any key during the transition jumps to the end
      * }}}
      */
    def withSkipOnInput: Builder[F] =
      copy(skipOnInput = true)

    private def applyEasing(progress: Double): Double =
      easingFn.fold(progress)(_(progress))

    // ── Build: progress-based (simplest) ─────────────────────────

    /** Build a progress-based transition with a specified duration.
      *
      * The `duration` represents the maximum time the transition takes — the time it would need to sweep the entire
      * screen. If the render callback signals [[ProgressResult.done]] before progress reaches 1.0 the transition
      * completes early, which is the recommended way to handle content that is shorter than the full screen.
      *
      * The `renderFrame` function receives the from/to content and a [[ProgressContext]] containing the (optionally
      * eased) progress value and actual screen dimensions.
      *
      * ==Example==
      * {{{
      * import scala.concurrent.duration._
      *
      * builder.buildProgress(500.millis) { (from, to, ctx) =>
      *   val frame = blendFrame(from, to, ctx.progress, ctx.screenWidth)
      *   if (visuallyDone) ProgressResult.done(frame)
      *   else              ProgressResult.continue(frame)
      * }
      * }}}
      *
      * @param duration
      *   maximum wall-clock duration. The number of simulation steps is computed as
      *   `duration / animationSettings.step`. The transition may finish earlier if the render callback returns
      *   `ProgressResult.done`.
      * @param renderFrame
      *   `(from, to, ctx) => ProgressResult` where `ctx.progress ∈ [0.0, 1.0]` and `ctx.screenWidth`/`ctx.screenHeight`
      *   are actual terminal dimensions.
      */
    def buildProgress(duration: FiniteDuration)(
        renderFrame: (ScreenAdjusted, ScreenAdjusted, ProgressContext) => ProgressResult
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] = {
      val totalSteps = math.max(1, (duration / animationSettings.step).toInt)
      val builder = this
      val skipRef = new AtomicReference[Deferred[F, Unit]](null)

      new Transition[F] {
        override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
          for {
            fromContent <- from.content.map(_.getOrElse(ScreenAdjusted("")))
            toContent <- to.content.map(_.getOrElse(ScreenAdjusted("")))
            screen <- console.context
            stepRef <- Ref[F].of(0)
            stepperRef <- FixedStep.makeRef[F]
            done <- Deferred[F, Unit]
            skip <- Deferred[F, Unit]
            _ = if (builder.skipOnInput) skipRef.set(skip)
            onTick = for {
              stepsAndProgress <- FixedStep.consumeSteps(stepperRef, animationSettings.step)
              (nrOfSteps, fractional) = stepsAndProgress
              currentStep <- stepRef.modify { s =>
                val next = math.min(totalSteps, s + nrOfSteps)
                (next, next)
              }
              rawProgress = math.min(1.0, (currentStep.toDouble + fractional) / totalSteps.toDouble)
              progress = builder.applyEasing(rawProgress)
              ctx = ProgressContext(progress, screen.screenWidth, screen.screenHeight)
              result = renderFrame(fromContent, toContent, ctx)
              _ <- console.clear()
              _ <- console.writeString(result.frame)
              _ <-
                if (currentStep >= totalSteps || result.isDone) done.complete(()).attempt.void
                else Monad[F].unit
            } yield ()
            sub <- ticker.subscribe(onTick)
            _ <- ticker.start
            _ <- (if (builder.skipOnInput) done.get.race(skip.get).void else done.get)
              .guarantee(sub.cancel)
            _ = skipRef.set(null)
            _ <- console.clear() *> console.writeString(toContent)
          } yield ()

        override def userInput(input: UserInput): F[Unit] =
          if (!builder.skipOnInput) Monad[F].unit
          else {
            val d = skipRef.get()
            if (d != null) d.complete(()).attempt.void
            else Monad[F].unit
          }
      }
    }

    // ── Build: stepped (more control) ────────────────────────────

    /** Build a transition with full control over each tick.
      *
      * The `onTick` callback receives a `StepContext` that provides the from/to content, screen dimensions, a `render`
      * helper, and a `complete` signal. Call `ctx.complete` when the transition is finished.
      *
      * @param onTick
      *   `(steps: Int, progress: Double, ctx: StepContext[F]) => F[Unit]` where `steps` is the number of elapsed
      *   simulation steps this tick, `progress` is fractional progress toward the next step, and `ctx` provides
      *   rendering utilities.
      */
    def buildStepped(
        onTick: (Int, Double, StepContext[F]) => F[Unit]
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] = {
      val builder = this
      val skipRef = new AtomicReference[Deferred[F, Unit]](null)

      new Transition[F] {
        override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
          for {
            fromContent <- from.content.map(_.getOrElse(ScreenAdjusted("")))
            toContent <- to.content.map(_.getOrElse(ScreenAdjusted("")))
            screen <- console.context
            done <- Deferred[F, Unit]
            skip <- Deferred[F, Unit]
            _ = if (builder.skipOnInput) skipRef.set(skip)
            stepperRef <- FixedStep.makeRef[F]
            ctx = new StepContext[F] {
              val from: ScreenAdjusted = fromContent
              val to: ScreenAdjusted = toContent
              val screenWidth: Int = screen.screenWidth
              val screenHeight: Int = screen.screenHeight
              def render(frame: ScreenAdjusted): F[Unit] =
                console.clear() *> console.writeString(frame)
              def complete: F[Unit] = done.complete(()).attempt.void
            }
            tickCallback = for {
              stepsAndProgress <- FixedStep.consumeSteps(stepperRef, animationSettings.step)
              (nrOfSteps, progress) = stepsAndProgress
              _ <- onTick(nrOfSteps, progress, ctx)
            } yield ()
            sub <- ticker.subscribe(tickCallback)
            _ <- ticker.start
            _ <- (if (builder.skipOnInput) done.get.race(skip.get).void else done.get)
              .guarantee(sub.cancel)
            _ = skipRef.set(null)
            _ <- console.clear() *> console.writeString(toContent)
          } yield ()

        override def userInput(input: UserInput): F[Unit] =
          if (!builder.skipOnInput) Monad[F].unit
          else {
            val d = skipRef.get()
            if (d != null) d.complete(()).attempt.void
            else Monad[F].unit
          }
      }
    }

    // ── Build: progress with GlideLayer ──────────────────────────

    /** Like `buildProgress` but also provides a `GlideLayer.Overlay` for sub-pixel rendering on WebGL backends.
      *
      * Easing is applied to the progress value before it reaches `renderFrame`.
      *
      * Prefer the duration-based overload (`buildProgressWithGlide(500.millis)`) for consistency with
      * [[buildProgress]]. This step-based variant is retained for backwards compatibility and cases where exact step
      * control is needed.
      *
      * @param totalSteps
      *   how many discrete simulation steps the transition takes.
      * @param renderFrame
      *   `(from, to, progress, glide) => F[ScreenAdjusted]` — use the glide overlay for smooth character interpolation
      *   at the transition edge.
      */
    def buildProgressWithGlide(totalSteps: Int = 20)(
        renderFrame: (ScreenAdjusted, ScreenAdjusted, Double, GlideLayer.Overlay[F]) => F[ScreenAdjusted]
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] =
      buildProgressWithGlideInternal(totalSteps)(renderFrame)

    /** Like `buildProgress` but also provides a `GlideLayer.Overlay` for sub-pixel rendering on WebGL backends.
      *
      * This overload accepts a `FiniteDuration` instead of a step count, making it consistent with [[buildProgress]].
      * The number of simulation steps is computed as `duration / animationSettings.step`.
      *
      * Easing is applied to the progress value before it reaches `renderFrame`.
      *
      * @param duration
      *   the maximum wall-clock time for the transition.
      * @param renderFrame
      *   `(from, to, progress, glide) => F[ScreenAdjusted]` — use the glide overlay for smooth character interpolation
      *   at the transition edge.
      */
    def buildProgressWithGlide(duration: FiniteDuration)(
        renderFrame: (ScreenAdjusted, ScreenAdjusted, Double, GlideLayer.Overlay[F]) => F[ScreenAdjusted]
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] = {
      val totalSteps = math.max(1, (duration / animationSettings.step).toInt)
      buildProgressWithGlideInternal(totalSteps)(renderFrame)
    }

    private def buildProgressWithGlideInternal(totalSteps: Int)(
        renderFrame: (ScreenAdjusted, ScreenAdjusted, Double, GlideLayer.Overlay[F]) => F[ScreenAdjusted]
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] = {
      val gc = glideConfig.getOrElse(GlideConfig())
      val builder = this
      val skipRef = new AtomicReference[Deferred[F, Unit]](null)

      new Transition[F] {
        override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
          for {
            gridLayer <- GlideLayer.make[F](console, animationSettings.step, gc.wrapThreshold)
            fromContent <- from.content.map(_.getOrElse(ScreenAdjusted("")))
            toContent <- to.content.map(_.getOrElse(ScreenAdjusted("")))
            stepRef <- Ref[F].of(0)
            stepperRef <- FixedStep.makeRef[F]
            done <- Deferred[F, Unit]
            skip <- Deferred[F, Unit]
            _ = if (builder.skipOnInput) skipRef.set(skip)
            onTick = for {
              stepsAndProgress <- FixedStep.consumeSteps(stepperRef, animationSettings.step)
              (nrOfSteps, fractional) = stepsAndProgress
              currentStep <- stepRef.modify { s =>
                val next = math.min(totalSteps, s + nrOfSteps)
                (next, next)
              }
              rawProgress = math.min(1.0, (currentStep.toDouble + fractional) / totalSteps.toDouble)
              progress = builder.applyEasing(rawProgress)
              frame <- renderFrame(fromContent, toContent, progress, gridLayer)
              _ <- console.clear()
              _ <- console.writeString(frame)
              _ <-
                if (currentStep >= totalSteps) done.complete(()).attempt.void
                else Monad[F].unit
            } yield ()
            sub <- ticker.subscribe(onTick)
            _ <- ticker.start
            _ <- (if (builder.skipOnInput) done.get.race(skip.get).void else done.get)
              .guarantee(sub.cancel *> gridLayer.clear())
            _ = skipRef.set(null)
            _ <- console.clear() *> console.writeString(toContent)
          } yield ()

        override def userInput(input: UserInput): F[Unit] =
          if (!builder.skipOnInput) Monad[F].unit
          else {
            val d = skipRef.get()
            if (d != null) d.complete(()).attempt.void
            else Monad[F].unit
          }
      }
    }

    // ── Build: with setup (most flexible) ────────────────────────

    /** Build a transition with custom setup and cleanup phases.
      *
      * This is the most flexible variant — it handles the FixedStep, Deferred, and ticker subscribe/cancel lifecycle,
      * while letting you perform arbitrary initialization (create Refs, GlideLayer, write initial frame, etc.) and
      * cleanup (clear effects, clear overlay, etc.).
      *
      * The `setup` function receives the resolved from/to content and a `complete` signal, and must return the tick
      * handler and an optional cleanup effect (run in `guarantee` after the transition finishes).
      *
      * ==Example==
      * {{{
      * builder.buildWithSetup { (from, to, complete) =>
      *   for {
      *     stateRef <- Ref[F].of(initialState)
      *     glide    <- GlideLayer.make[F](console, ...)
      *     _        <- console.writeString(from) // initial frame
      *   } yield TickedTransition.TickHandler(
      *     onTick = (steps, progress) => { ... advance state, render, call complete when done ... },
      *     cleanup = glide.clear()
      *   )
      * }
      * }}}
      */
    def buildWithSetup(
        setup: (ScreenAdjusted, ScreenAdjusted, F[Unit]) => F[TickHandler[F]]
    )(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F] = {
      val builder = this
      val skipRef = new AtomicReference[Deferred[F, Unit]](null)

      new Transition[F] {
        override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
          for {
            fromContent <- from.content.map(_.getOrElse(ScreenAdjusted("")))
            toContent <- to.content.map(_.getOrElse(ScreenAdjusted("")))
            stepperRef <- FixedStep.makeRef[F]
            done <- Deferred[F, Unit]
            skip <- Deferred[F, Unit]
            _ = if (builder.skipOnInput) skipRef.set(skip)
            handler <- setup(fromContent, toContent, done.complete(()).attempt.void)
            tickCallback = for {
              stepsAndProgress <- FixedStep.consumeSteps(stepperRef, animationSettings.step)
              (nrOfSteps, progress) = stepsAndProgress
              _ <- handler.onTick(nrOfSteps, progress)
            } yield ()
            sub <- ticker.subscribe(tickCallback)
            _ <- ticker.start
            _ <- (if (builder.skipOnInput) done.get.race(skip.get).void else done.get)
              .guarantee(sub.cancel *> handler.cleanup)
            _ = skipRef.set(null)
            _ <- console.clear() *> console.writeString(toContent)
          } yield ()

        override def userInput(input: UserInput): F[Unit] =
          if (!builder.skipOnInput) Monad[F].unit
          else {
            val d = skipRef.get()
            if (d != null) d.complete(()).attempt.void
            else Monad[F].unit
          }
      }
    }
  }

  /** The tick handler and cleanup returned by `buildWithSetup`. */
  final case class TickHandler[F[_]](
      onTick: (Int, Double) => F[Unit],
      cleanup: F[Unit]
  )

  object TickHandler {
    def apply[F[_]](onTick: (Int, Double) => F[Unit])(implicit F: Monad[F]): TickHandler[F] =
      TickHandler(onTick, F.unit)
  }
}
