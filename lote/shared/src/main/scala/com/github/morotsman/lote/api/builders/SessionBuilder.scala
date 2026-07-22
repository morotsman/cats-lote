package com.github.morotsman.lote.api.builders

import cats.Monad
import cats.effect.{Async, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Layout, Milestone, PlatformCapability, SlidePosition}
import com.github.morotsman.lote.api.spi.{NConsole, Overlay, Terminal => TerminalAlgebra, Ticker}
import com.github.morotsman.lote.internal.builders.{
  SlideBuilder => InternalSlideBuilder,
  TextSlideBuilder => InternalTextSlideBuilder
}
import com.github.morotsman.lote.internal.algebra.{Feature, IdleDetector, PresentationExecutor}
import com.github.morotsman.lote.internal.interpreter.{
  IdleDetectorConfig,
  IdleDetectorInterpreter,
  PresentationExecutorInterpreter
}
import com.github.morotsman.lote.internal.interpreter.middleware.{
  Idle,
  Middleware,
  NavigationSubscriber,
  ProgressBar,
  QuickNavigation,
  Timer
}
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

  /** The platform capabilities of the underlying terminal backend.
    *
    * Use this to check whether the platform supports effects, sub-pixel rendering, or 3D transforms, and choose
    * transitions accordingly.
    */
  def capabilities: Set[PlatformCapability] = console.capabilities

  /** Returns true if the backend supports the given capability. */
  def supports(capability: PlatformCapability): Boolean = capabilities.contains(capability)

  /** Returns a reference to the shared 3D scene, if the backend supports spatial mode.
    *
    * On WebGL backends in spatial mode, returns `Some(Scene3DRef)`. Cast to `com.github.morotsman.lote.api.Scene3DRef`
    * in JS code to access the shared Three.js scene.
    */
  def scene3DRef: Option[Any] = console.scene3DRef
}

/** A high-level builder that encapsulates all the wiring needed to run a presentation with overlays.
  *
  * Instead of manually creating a ticker, idle detector, overlay layer, and presentation executor, you can use the
  * SessionBuilder to configure everything declaratively and then call `run()`.
  *
  * Features (Timer, ProgressBar, QuickNavigation, custom overlays) are stored in a feature registry. Each feature
  * self-registers its overlay, slide-change callback, and any lifecycle hooks, so the execution loop processes them
  * uniformly without feature-specific wiring.
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
    private val featureFactories: List[Presentation[F] => F[Feature[F]]],
    private val idleEnabled: Boolean,
    private val idleDetectorConfig: IdleDetectorConfig,
    private val onSlideChange: Option[Int => F[Unit]],
    private val tickerInterval: FiniteDuration,
    private val animationStep: FiniteDuration,
    private val customTickerFactory: Option[F[Ticker[F]]] = None
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

  // -- Feature registration --

  /** Adds a group of slides whose positions are computed automatically by the given layout.
    *
    * Slides added inside the section receive positions from the layout. If a slide explicitly calls `.at()`, that
    * overrides the layout-computed position.
    *
    * {{{
    * .addLayoutSection(Layout.grid(cols = 3, spacingX = 1800, spacingY = 1200, origin = SlidePosition(0, 3600, 0))) { section =>
    *   section
    *     .addTextSlide { _.content("Slide 1") }
    *     .addTextSlide { _.content("Slide 2") }
    *     .addTextSlide { _.content("Slide 3") }
    * }
    * }}}
    */
  def addLayoutSection(layout: Layout)(
      sectionBuilder: LayoutSectionBuilder[F] => LayoutSectionBuilder[F]
  ): SessionBuilder[F] = {
    val section = sectionBuilder(new LayoutSectionBuilder[F](Nil))
    val sectionSteps = section.steps
    val positions = layout.positions(sectionSteps.size)
    val wrappedSteps = sectionSteps.zip(positions).map { case (step, pos) =>
      SessionBuilder.wrapWithPosition(step, pos)
    }
    this.copy(slideSteps = slideSteps ++ wrappedSteps)
  }

  /** Adds a countdown timer overlay showing remaining presentation time. */
  def withTimer(allocatedTime: FiniteDuration): SessionBuilder[F] =
    registerFeature(_ => Timer.make[F](allocatedTime).map(Feature.fromOverlay[F]))

  /** Adds a progress bar overlay at the bottom of the screen.
    *
    * @param milestones
    *   optional named markers to display above the bar for sections like "Intro", "Demo", or "Q&A"
    */
  def withProgressBar(milestones: List[Milestone] = List.empty): SessionBuilder[F] =
    registerFeature(presentation =>
      ProgressBar
        .make[F](presentation.slideSpecifications.length, milestones = milestones)
        .map { pb =>
          new Feature[F] {
            val overlay: Overlay[F] = pb
            def onSlideChange(index: Int): F[Unit] = pb.setCurrentSlide(index)
            def onPresentationBuilt(p: Presentation[F]): F[Unit] = Monad[F].unit
            def onExecutorReady(executor: PresentationExecutor[F]): F[Unit] = Monad[F].unit
          }
        }
    )

  /** Adds a quick navigation overlay (press 'N' to toggle, arrows + Enter to navigate). */
  def withQuickNavigation(): SessionBuilder[F] =
    registerFeature(_ =>
      QuickNavigation.make[F]().map { qn =>
        new Feature[F] {
          val overlay: Overlay[F] = qn
          def onSlideChange(index: Int): F[Unit] = qn.onSlideChange(index)
          def onPresentationBuilt(p: Presentation[F]): F[Unit] = qn.setTitles(p.titles)
          def onExecutorReady(executor: PresentationExecutor[F]): F[Unit] =
            qn.subscribe(NavigationSubscriber(1L, index => executor.setSlide(index))).void
        }
      }
    )

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
    registerFeature(_ => overlay.map(Feature.fromOverlay[F]))

  /** Adds a custom overlay that doesn't require effectful construction. */
  def addOverlay(overlay: Overlay[F]): SessionBuilder[F] =
    registerFeature(_ => Async[F].pure(Feature.fromOverlay[F](overlay)))

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

  /** Replaces the default sleep-based ticker with a custom ticker factory.
    *
    * This is primarily useful for WebGL/browser backends where a `requestAnimationFrame`-based ticker provides
    * vsync-aligned rendering at the display's native refresh rate (~60 fps). When a custom ticker is set, the
    * `tickerInterval` / `frameRate` settings are ignored — the custom ticker controls the cadence.
    *
    * Simulation speed is unaffected: `AnimationSettings.step` (configurable via `withAnimationStep`) still controls how
    * fast animations advance, via the `FixedStep` accumulator.
    *
    * Example (Scala.js / WebGL):
    * {{{
    * import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
    *
    * SessionBuilder[IO]()
    *   .withCustomTicker(RafTickerInterpreter.make[IO])
    *   .addSlide(...)
    *   .run()
    * }}}
    */
  def withCustomTicker(factory: F[Ticker[F]]): SessionBuilder[F] =
    this.copy(customTickerFactory = Some(factory))

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

  // -- Internal --

  private def registerFeature(factory: Presentation[F] => F[Feature[F]]): SessionBuilder[F] =
    this.copy(featureFactories = featureFactories :+ factory)

  // -- Execution --

  /** Builds and runs the presentation session. Returns when the presenter leaves the presentation, typically by
    * pressing `Esc`.
    *
    * This method handles all the infrastructure setup:
    *   - Terminal console with resource management
    *   - Ticker for animation
    *   - Idle detection (if configured)
    *   - Middleware layer with overlays
    *   - Feature registry lifecycle
    *   - Presentation execution loop
    *
    * Requires at least one slide to have been added.
    */
  def run()(implicit terminal: TerminalAlgebra[F]): F[Unit] = {
    require(slideSteps.nonEmpty, "At least one slide must be added before running the presentation")

    NConsoleInterpreter.resource[F](terminal).use { rawConsole =>
      for {
        ticker <- customTickerFactory.getOrElse(TickerInterpreter.make[F](tickerInterval))
        idleDetector <- IdleDetectorInterpreter.make[F](idleDetectorConfig)
        baseConsole =
          if (idleEnabled) IdleAwareNConsole.wrap[F](rawConsole, idleDetector)
          else rawConsole
        _ <- runSession(baseConsole, ticker, Some(idleDetector))
      } yield ()
    }
  }

  /** Builds and runs the presentation session using the provided console and ticker.
    *
    * This is the test-friendly counterpart to `run()`. Instead of creating a real terminal and ticker, it accepts
    * external implementations — typically `TestConsole` and `TestTicker` from the testkit.
    *
    * Pre-load user inputs (including an `Esc` key to terminate) via `TestConsole` before calling this method, otherwise
    * the presentation loop will keep reading until it runs out of inputs.
    *
    * Idle detection is not supported in this mode — `withIdleAnimation()` settings are ignored.
    *
    * Example:
    * {{{
    * for {
    *   harness <- SlideTestHarness.make[IO](screen = Screen(80, 24), inputs = List(Key(SpecialKey.Esc)))
    *   _ <- SessionBuilder[IO]()
    *     .addOverlay(InputStatusOverlay.make[IO]())
    *     .addTextSlide(_.content("Hello").title("Slide 1"))
    *     .runWith(harness.console, harness.ticker)
    *   written <- harness.writtenFrames
    * } yield assert(written.nonEmpty)
    * }}}
    *
    * Requires at least one slide to have been added.
    */
  def runWith(console: NConsole[F], ticker: Ticker[F]): F[Unit] = {
    require(slideSteps.nonEmpty, "At least one slide must be added before running the presentation")
    runSession(console, ticker, idleDetector = None)
  }

  private def runSession(
      baseConsole: NConsole[F],
      ticker: Ticker[F],
      idleDetector: Option[IdleDetector[F]]
  ): F[Unit] =
    for {
      // Create middleware layer
      consoleWithMiddleware <- Middleware.make[F](baseConsole, ticker)

      // Build the presentation with the middleware-wrapped console
      presentation <- buildPresentation(consoleWithMiddleware, ticker)

      // Create all registered features
      registeredFeatures <- featureFactories.traverse(_(presentation))

      // Create idle feature if enabled (special: needs IdleDetector from run())
      idleFeature <-
        if (idleEnabled && idleDetector.isDefined)
          Idle.make[F](idleDetector.get, baseConsole).map { idle =>
            Some(new Feature[F] {
              val overlay: Overlay[F] = idle
              def onSlideChange(index: Int): F[Unit] = idleDetector.get.notifyActivity()
              def onPresentationBuilt(p: Presentation[F]): F[Unit] = Monad[F].unit
              def onExecutorReady(executor: PresentationExecutor[F]): F[Unit] = Monad[F].unit
            })
          }
        else Monad[F].pure(None: Option[Feature[F]])

      allFeatures = registeredFeatures ++ idleFeature.toList

      // Register all overlays with middleware
      _ <- consoleWithMiddleware.addOverlays(allFeatures.map(_.overlay))

      // Notify features that presentation is built
      _ <- allFeatures.traverse_(_.onPresentationBuilt(presentation))

      // Create executor with composed slide-change callbacks
      executor <- {
        implicit val mc: NConsole[F] = consoleWithMiddleware
        PresentationExecutorInterpreter.make[F](
          presentation,
          { index =>
            allFeatures.traverse_(_.onSlideChange(index)) *>
              onSlideChange.fold(Monad[F].unit)(_(index))
          }
        )
      }

      // Notify features that executor is ready (e.g., QuickNavigation subscribes here)
      _ <- allFeatures.traverse_(_.onExecutorReady(executor))

      // Start the presentation loop
      _ <- executor.start()
    } yield ()

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
      featureFactories = List.empty,
      idleEnabled = false,
      idleDetectorConfig = IdleDetectorConfig(),
      onSlideChange = None,
      tickerInterval = 40.millis,
      animationStep = AnimationSettings.DefaultStep,
      customTickerFactory = None
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

  /** Wraps a slide step to pre-apply a layout position. The position is set on the builder before the user's lambda
    * runs, so explicit `.at()` calls inside the lambda override the layout position.
    */
  private[builders] def wrapWithPosition[F[_]](step: SlideStep[F], pos: SlidePosition): SlideStep[F] =
    step match {
      case TextSlideStep(f) =>
        TextSlideStep(new BuiltTextSlideStep[F] {
          override def build(builder: TextSlideBuilderStart[F])(implicit
              ctx: SlideContext[F]
          ): TextSlideBuilderReady[F] =
            f.build(builder.at(pos.x, pos.y, pos.z))
        })
      case CustomSlide(f) =>
        CustomSlide(new BuiltCustomSlideStep[F] {
          override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): SlideBuilderReady[F] =
            f.build(builder.at(pos.x, pos.y, pos.z))
        })
      case EffectfulSlide(f) =>
        EffectfulSlide(new BuiltEffectfulSlideStep[F] {
          override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): F[SlideBuilderReady[F]] =
            f.build(builder.at(pos.x, pos.y, pos.z))
        })
    }
}

/** A builder for collecting slides inside a layout section.
  *
  * Slides added here will have their positions computed automatically by the layout. Use with
  * `SessionBuilder.addLayoutSection`.
  */
class LayoutSectionBuilder[F[_]: Async: Ref.Make] private[builders] (
    private[builders] val steps: List[SessionBuilder.SlideStep[F]]
) {

  def addTextSlide(
      textSlideBuilder: TextSlideBuilderStart[F] => TextSlideBuilderReady[F]
  ): LayoutSectionBuilder[F] =
    new LayoutSectionBuilder(steps :+ SessionBuilder.TextSlideStep(new SessionBuilder.BuiltTextSlideStep[F] {
      override def build(builder: TextSlideBuilderStart[F])(implicit ctx: SlideContext[F]): TextSlideBuilderReady[F] = {
        val _ = ctx
        textSlideBuilder(builder)
      }
    }))

  def addSlide(
      slideBuilder: SlideBuilderStart[F] => SlideBuilderReady[F]
  ): LayoutSectionBuilder[F] =
    new LayoutSectionBuilder(steps :+ SessionBuilder.CustomSlide(new SessionBuilder.BuiltCustomSlideStep[F] {
      override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): SlideBuilderReady[F] = {
        val _ = ctx
        slideBuilder(builder)
      }
    }))

  def addSlideF(
      slideBuilder: SlideBuilderStart[F] => F[SlideBuilderReady[F]]
  ): LayoutSectionBuilder[F] =
    new LayoutSectionBuilder(steps :+ SessionBuilder.EffectfulSlide(new SessionBuilder.BuiltEffectfulSlideStep[F] {
      override def build(builder: SlideBuilderStart[F])(implicit ctx: SlideContext[F]): F[SlideBuilderReady[F]] = {
        val _ = ctx
        slideBuilder(builder)
      }
    }))
}
