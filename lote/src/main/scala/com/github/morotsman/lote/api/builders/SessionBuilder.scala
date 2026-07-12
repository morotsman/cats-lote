package com.github.morotsman.lote.api.builders

import cats.Monad
import cats.effect.{Async, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Milestone}
import com.github.morotsman.lote.api.spi.{NConsole, Overlay, Ticker}
import com.github.morotsman.lote.internal.builders.{
  SlideBuilder => InternalSlideBuilder,
  TextSlideBuilder => InternalTextSlideBuilder
}
import com.github.morotsman.lote.internal.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter, PresentationExecutorInterpreter}
import com.github.morotsman.lote.internal.interpreter.middleware.{Idle, Middleware, NavigationSubscriber, ProgressBar, QuickNavigation, Timer}
import com.github.morotsman.lote.internal.interpreter.nconsole.{IdleAwareNConsole, NConsoleInterpreter}
import com.github.morotsman.lote.internal.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.internal.model.Presentation

import scala.concurrent.duration.{DurationInt, FiniteDuration, NANOSECONDS}

/** Provides implicit NConsole, Ticker, and AnimationSettings instances for use inside slide builder functions. This is
  * passed to the user's builder lambdas so transitions and other components that require these typeclasses can be
  * constructed.
  */
class SlideContext[F[_]](
    val console: NConsole[F],
    val ticker: Ticker[F],
    val animationSettings: AnimationSettings
) {
  implicit def nConsole: NConsole[F] = console
  implicit def tickerInstance: Ticker[F] = ticker
  implicit def animationSettingsInstance: AnimationSettings = animationSettings
}

/** A high-level builder that encapsulates all the wiring needed to run a presentation with overlays.
  *
  * Instead of manually creating a ticker, idle detector, overlay layer, and presentation executor, you can use the
  * SessionBuilder to configure everything declaratively and then call `run()`.
  *
  * The nested slide-builder lambdas are type-checked:
  *   - `addTextSlide(...)` requires a `TextSlideBuilder` that has had `content(...)` supplied
  *   - `addSlide(...)` / `addSlideF(...)` require a `SlideBuilder` that has had `addSlide(...)` supplied
  *
  * `run()` still performs a runtime check that at least one slide was added to the session.
  *
  * Example:
  * {{{
  * SessionBuilder[IO]()
  *   .withTimer(30.minutes)
  *   .withProgressBar()
  *   .withQuickNavigation()
  *   .withIdleAnimation()
  *   .withFrameRate(60)
  *   .withAnimationFrameRate(25)
  *   .addTextSlide { _ =>
  *     _.content("Hello").title("Intro").replaceTransition(' ')
  *   }
  *   .addTextSlide { _ =>
  *     _.content("World").title("Slide 2")
  *   }
  *   .run()
  * }}}
  */
case class SessionBuilder[F[_]: Async: Ref.Make] private (
    private val slideSteps: List[SessionBuilder.SlideStep[F]],
    private val timerDuration: Option[FiniteDuration],
    private val progressBarEnabled: Boolean,
    private val progressBarMilestones: List[Milestone],
    private val quickNavigationEnabled: Boolean,
    private val idleEnabled: Boolean,
    private val idleDetectorConfig: IdleDetectorConfig,
    private val customOverlays: List[F[Overlay[F]]],
    private val onSlideChange: Option[Int => F[Unit]],
    private val tickerInterval: FiniteDuration,
    private val animationStep: FiniteDuration
) {

  // -- Slide building --

  /** Adds a custom interactive slide using the SlideBuilder DSL.
    *
    * Ordinary slide code should not need to touch `SlideContext` directly. Prefer contextual helper values such as
    * `MyInteractiveSlide.contextual(...)` that the builder can resolve lazily.
    */
  def addSlide(
      slideBuilder: SlideBuilderStart[F] => SlideBuilderReady[F]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.CustomSlide(new SessionBuilder.BuiltCustomSlideStep[F] {
      override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): SlideBuilderReady[F] = {
        val _ = ctx
        slideBuilder(builder)
      }
    }))

  /** Adds an effectful custom slide. Use this for interactive slides that require effectful construction (e.g.,
    * allocating Refs, Queues, etc.).
    *
    * Ordinary slide code should not need to touch `SlideContext` directly. Prefer contextual helper values such as
    * `MyInteractiveSlide.contextual(...)` that the builder can resolve lazily.
    *
    * Example:
    * {{{
    * .addSlideF { builder =>
    *   for {
    *     slide <- MyInteractiveSlide.make[IO]()
    *   } yield builder.addSlide(slide).title("Interactive")
    * }
    * }}}
    */
  def addSlideF(
      slideBuilder: SlideBuilderStart[F] => F[SlideBuilderReady[F]]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.EffectfulSlide(new SessionBuilder.BuiltEffectfulSlideStep[F] {
      override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): F[SlideBuilderReady[F]] = {
        val _ = ctx
        slideBuilder(builder)
      }
    }))

  /** Adds a text-based slide using the TextSlideBuilder DSL.
    *
    * Ordinary slide code should not need to touch `SlideContext` directly. Prefer contextual helper values such as
    * `SweepRightTransition.contextual(...)` that the builder can resolve lazily.
    *
    * Example:
    * {{{
    * .addTextSlide { builder =>
      *   builder.content("Hello").morphTransition()
    * }
    * }}}
    */
  def addTextSlide(
      textSlideBuilder: TextSlideBuilderStart[F] => TextSlideBuilderReady[F]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.TextSlideStep(new SessionBuilder.BuiltTextSlideStep[F] {
      override def build(builder: TextSlideBuilderStart[F])(implicit ctx: SlideContext[F]): TextSlideBuilderReady[F] = {
        val _ = ctx
        textSlideBuilder(builder)
      }
    }))

  // -- Overlay configuration --

  /** Adds a countdown timer overlay showing remaining presentation time. */
  def withTimer(allocatedTime: FiniteDuration): SessionBuilder[F] =
    this.copy(timerDuration = Some(allocatedTime))

  /** Adds a progress bar overlay at the bottom of the screen.
    *
    * @param milestones
    *   optional named markers to display above the bar for sections like "Intro", "Demo", or "Q&A"
    */
  def withProgressBar(milestones: List[Milestone] = List.empty): SessionBuilder[F] =
    this.copy(progressBarEnabled = true, progressBarMilestones = milestones)

  /** Adds a quick navigation overlay (press 'N' to toggle, arrows + Enter to navigate). */
  def withQuickNavigation(): SessionBuilder[F] =
    this.copy(quickNavigationEnabled = true)

  /** Adds an idle screen animation that activates when the presenter stops interacting.
    *
    * @param idleTimeout
    *   how long before the idle animation triggers (default 2 minutes)
    */
  def withIdleAnimation(
      idleTimeout: FiniteDuration = 2.minutes
  ): SessionBuilder[F] =
    this.copy(
      idleEnabled = true,
      idleDetectorConfig = IdleDetectorConfig(idleTimeout = idleTimeout)
    )

  /** Adds a custom overlay created by the user. The effect will be evaluated during session setup.
    *
    * Use this for overlays that require effectful construction (e.g., those that allocate Refs).
    */
  def addOverlay(overlay: F[Overlay[F]]): SessionBuilder[F] =
    this.copy(customOverlays = customOverlays :+ overlay)

  /** Adds a custom overlay that doesn't require effectful construction. */
  def addOverlay(overlay: Overlay[F]): SessionBuilder[F] =
    this.copy(customOverlays = customOverlays :+ Async[F].pure(overlay))


  /** Sets a callback invoked on each slide change with the new slide index. */
  def onSlideChanged(callback: Int => F[Unit]): SessionBuilder[F] =
    this.copy(onSlideChange = Some(callback))

  /** Configures the ticker interval (default 40ms ≈ 25fps).
    *
    * This primarily affects render/update cadence. Built-in transitions advance on a fixed simulation step so their
    * speed stays stable when you change the ticker interval.
    */
  def withTickerInterval(interval: FiniteDuration): SessionBuilder[F] =
    this.copy(tickerInterval = interval)

  /** Configures render cadence in frames per second.
    *
    * This is a convenience alternative to `withTickerInterval(...)` for people who prefer thinking in FPS instead of
    * milliseconds, which is to say, almost everyone.
    */
  def withFrameRate(frameRate: Double): SessionBuilder[F] =
    this.copy(tickerInterval = SessionBuilder.fpsToDuration(frameRate))

  /** Configures how quickly built-in animations advance.
    *
    * This is independent of `withTickerInterval`: a lower animation step makes transitions move faster, while a higher
    * step makes them move slower.
    */
  def withAnimationStep(step: FiniteDuration): SessionBuilder[F] = {
    require(step > DurationInt(0).millis, "Animation step must be greater than 0ms")
    this.copy(animationStep = step)
  }

  /** Configures built-in animation speed in frames per second.
    *
    * This is a convenience alternative to `withAnimationStep(...)`. Higher FPS means built-in transitions and example
    * animations advance faster; lower FPS means they take life a bit less personally.
    */
  def withAnimationFrameRate(frameRate: Double): SessionBuilder[F] =
    this.copy(animationStep = SessionBuilder.fpsToDuration(frameRate))

  // -- Execution --

  /** Builds and runs the presentation session. Returns when the presenter leaves the presentation, typically by
    * pressing `Esc`.
    *
    * This method handles all the infrastructure setup:
    *   - Terminal console with resource management
    *   - Ticker for animation
    *   - Idle detection (if configured)
    *   - Middleware layer with overlays
    *   - Quick navigation wiring
    *   - Progress bar wiring
    *   - Presentation execution loop
    *
    * Requires at least one slide to have been added.
    */
  def run(): F[Unit] = {
    require(slideSteps.nonEmpty, "At least one slide must be added before running the presentation")

    NConsoleInterpreter.resource[F]().use { rawConsole =>
      for {
        ticker <- TickerInterpreter.make[F](tickerInterval)
        idleDetector <- IdleDetectorInterpreter.make[F](idleDetectorConfig)
        baseConsole =
          if (idleEnabled) IdleAwareNConsole.wrap[F](rawConsole, idleDetector)
          else rawConsole

        // Create middleware layer
        consoleWithMiddleware <- Middleware.make[F](baseConsole, ticker)

        // Build the presentation with the middleware-wrapped console
        presentation <- buildPresentation(consoleWithMiddleware, ticker)
        totalSlides = presentation.slideSpecifications.length

        // Build middleware overlays
        timerOverlay <- timerDuration.traverse(d => Timer.make[F](d))
        progressBar <-
          if (progressBarEnabled)
            ProgressBar
              .make[F](totalSlides, milestones = progressBarMilestones)
              .map(Some(_))
          else Monad[F].pure(None: Option[ProgressBar[F]])
        quickNavigation <-
          if (quickNavigationEnabled) QuickNavigation.make[F]().map(Some(_))
          else Monad[F].pure(None: Option[QuickNavigation[F]])
        idleOverlay <-
          if (idleEnabled) Idle.make[F](idleDetector).map(Some(_))
          else Monad[F].pure(None: Option[Idle[F]])
        customOverlayInstances <- customOverlays.sequence

        // Collect all overlays and register with middleware
        allOverlays = List(
          progressBar.map(x => x: Overlay[F]),
          timerOverlay,
          quickNavigation.map(x => x: Overlay[F]),
        ).flatten ++ customOverlayInstances ++ idleOverlay.toList
        _ <- consoleWithMiddleware.addOverlays(allOverlays)

        // Wire up quick navigation titles
        _ <- quickNavigation match {
          case Some(qn) => qn.setTitles(presentation.titles)
          case None     => Monad[F].unit
        }

        // Create executor with slide-change callbacks
        executor <- {
          implicit val mc: NConsole[F] = consoleWithMiddleware
          PresentationExecutorInterpreter.make[F](
            presentation,
            { index =>
              val progressUpdate = progressBar match {
                case Some(pb) => pb.setCurrentSlide(index)
                case None     => Monad[F].unit
              }
              val idleNotify =
                if (idleEnabled) idleDetector.notifyActivity()
                else Monad[F].unit
              val quickNavigationNotify = quickNavigation match {
                case Some(qn) => qn.onSlideChange(index)
                case None     => Monad[F].unit
              }
              val userCallback = onSlideChange.fold(Monad[F].unit)(_(index))
              progressUpdate *> idleNotify *> quickNavigationNotify *> userCallback
            }
          )
        }

        // Wire quick navigation to executor
        _ <- quickNavigation match {
          case Some(qn) =>
            qn.subscribe(NavigationSubscriber(id = 1L, callback = index => executor.setSlide(index))).void
          case None => Monad[F].unit
        }

        // Start the presentation loop
        _ <- executor.start()
      } yield ()
    }
  }

  private def buildPresentation(console: NConsole[F], ticker: Ticker[F]): F[Presentation[F]] = {
    implicit val nc: NConsole[F] = console
    val ctx = new SlideContext[F](console, ticker, AnimationSettings(animationStep))
    implicit val slideContext: SlideContext[F] = ctx

    slideSteps
      .traverse {
        case SessionBuilder.TextSlideStep(f) =>
          val builder = BuilderDslAdapters.textSlideStart(ctx, InternalTextSlideBuilder[F]())
          Monad[F].pure(f.build(builder).buildSpec())
        case SessionBuilder.CustomSlide(f) =>
          val builder = BuilderDslAdapters.slideStart(ctx, InternalSlideBuilder[F]())
          Monad[F].pure(f.build(builder).buildSpec())
        case SessionBuilder.EffectfulSlide(f) =>
          f.build(BuilderDslAdapters.slideStart(ctx, InternalSlideBuilder[F]())).map(_.buildSpec())
      }
      .map { specs =>
        Presentation(
          slideSpecifications = specs,
          overlays = List.empty
        )
      }
  }
}

object SessionBuilder {

  trait BuiltTextSlideStep[F[_]] {
    def build(builder: TextSlideBuilderStart[F])(implicit ctx: SlideContext[F]): TextSlideBuilderReady[F]
  }

  trait BuiltCustomSlideStep[F[_]] {
    def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): SlideBuilderReady[F]
  }

  trait BuiltEffectfulSlideStep[F[_]] {
    def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): F[SlideBuilderReady[F]]
  }

  private[builders] def fpsToDuration(frameRate: Double): FiniteDuration = {
    require(frameRate > 0.0, "Frame rate must be greater than 0")
    require(!frameRate.isNaN, "Frame rate must be a valid number")
    require(!frameRate.isInfinity, "Frame rate must be finite")

    FiniteDuration(Math.round(1000000000.0 / frameRate), NANOSECONDS)
  }

  /** Creates a new SessionBuilder ready for configuration. */
  def apply[F[_]: Async: Ref.Make](): SessionBuilder[F] =
    new SessionBuilder[F](
      slideSteps = List.empty,
      timerDuration = None,
      progressBarEnabled = false,
      progressBarMilestones = List.empty,
      quickNavigationEnabled = false,
      idleEnabled = false,
      idleDetectorConfig = IdleDetectorConfig(),
      customOverlays = List.empty,
      onSlideChange = None,
      tickerInterval = 40.millis,
      animationStep = AnimationSettings.DefaultStep
    )

  // Internal ADT for storing slide configurations
  private[builders] sealed trait SlideStep[F[_]]

  private[builders] case class TextSlideStep[F[_]](
      f: BuiltTextSlideStep[F]
  ) extends SlideStep[F]

  private[builders] case class CustomSlide[F[_]](
      f: BuiltCustomSlideStep[F]
  ) extends SlideStep[F]

  private[builders] case class EffectfulSlide[F[_]](
      f: BuiltEffectfulSlideStep[F]
  ) extends SlideStep[F]
}

