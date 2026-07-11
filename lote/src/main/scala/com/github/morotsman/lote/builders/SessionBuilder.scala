package com.github.morotsman.lote.builders

import cats.Monad
import cats.effect.{Async, Ref}
import cats.implicits._
import com.github.morotsman.lote.algebra._
import com.github.morotsman.lote.builders.SlideBuilder.{WithContentSlide, WithoutSlide}
import com.github.morotsman.lote.builders.TextSlideBuilder.{WithContent, WithoutContent}
import com.github.morotsman.lote.interpreter.{
  IdleDetectorConfig,
  IdleDetectorInterpreter,
  PresentationExecutorInterpreter
}
import com.github.morotsman.lote.interpreter.middleware.{
  Idle,
  IdleOverlayConfig,
  Middleware,
  NavigationSubscriber,
  ProgressBar,
  QuickNavigation,
  Timer
}
import com.github.morotsman.lote.interpreter.nconsole.{IdleAwareNConsole, NConsoleInterpreter}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model.Presentation

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

/** A high-level builder that encapsulates all the wiring needed to run a presentation with middleware.
  *
  * Instead of manually creating a ticker, idle detector, middleware layer, and presentation executor, you can use the
  * SessionBuilder to configure everything declaratively and then call `run()`.
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
  *   .addTextSlide { implicit ctx => import ctx._
  *     _.content("Hello").title("Intro").transition(ReplaceTransition(' '))
  *   }
  *   .addTextSlide { implicit ctx => import ctx._
  *     _.content("World").title("Slide 2")
  *   }
  *   .run()
  * }}}
  */
case class SessionBuilder[F[_]: Async: Ref.Make] private (
    private val slideSteps: List[SessionBuilder.SlideStep[F]],
    private val timerDuration: Option[FiniteDuration],
    private val progressBarEnabled: Boolean,
    private val quickNavigationEnabled: Boolean,
    private val idleEnabled: Boolean,
    private val idleDetectorConfig: IdleDetectorConfig,
    private val idleOverlayConfig: IdleOverlayConfig,
    private val customOverlays: List[F[Overlay[F]]],
    private val onSlideChange: Option[Int => F[Unit]],
    private val tickerInterval: FiniteDuration,
    private val animationStep: FiniteDuration
) {

  // -- Slide building --

  /** Adds a custom interactive slide using the SlideBuilder DSL. The builder function receives a `SlideContext`
    * providing implicit `NConsole`, `Ticker`, and `AnimationSettings` instances.
    */
  def addSlide(
      slideBuilder: SlideContext[F] => SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.CustomSlide(slideBuilder))

  /** Adds an effectful custom slide. Use this for interactive slides that require effectful construction (e.g.,
    * allocating Refs, Queues, etc.).
    *
    * The function receives a `SlideContext` providing implicit `NConsole`, `Ticker`, and `AnimationSettings`
    * instances. Returns a builder function wrapped in `F[_]`.
    *
    * Example:
    * {{{
    * .addSlideF { implicit ctx => import ctx._
    *   for {
    *     slide <- MyInteractiveSlide.make[IO]()
    *   } yield _.addSlide(slide).title("Interactive")
    * }
    * }}}
    */
  def addSlideF(
      slideBuilder: SlideContext[F] => F[SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.EffectfulSlide(slideBuilder))

  /** Adds a text-based slide using the TextSlideBuilder DSL. The builder function receives a `SlideContext` providing
    * implicit `NConsole`, `Ticker`, and `AnimationSettings` instances needed for creating transitions.
    *
    * Example:
    * {{{
    * .addTextSlide { implicit ctx => import ctx._
    *   _.content("Hello").transition(MorphTransition())
    * }
    * }}}
    */
  def addTextSlide(
      textSlideBuilder: SlideContext[F] => TextSlideBuilder[F, WithoutContent] => TextSlideBuilder[F, WithContent]
  ): SessionBuilder[F] =
    this.copy(slideSteps = slideSteps :+ SessionBuilder.TextSlideStep(textSlideBuilder))

  // -- Middleware configuration --

  /** Adds a countdown timer overlay showing remaining presentation time. */
  def withTimer(allocatedTime: FiniteDuration): SessionBuilder[F] =
    this.copy(timerDuration = Some(allocatedTime))

  /** Adds a progress bar overlay at the bottom of the screen. */
  def withProgressBar(): SessionBuilder[F] =
    this.copy(progressBarEnabled = true)

  /** Adds a quick navigation overlay (press 'N' to toggle, arrows + Enter to navigate). */
  def withQuickNavigation(): SessionBuilder[F] =
    this.copy(quickNavigationEnabled = true)

  /** Adds an idle screen animation that activates when the presenter stops interacting.
    *
    * @param idleTimeout
    *   how long before the idle animation triggers (default 2 minutes)
    * @param overlayConfig
    *   fine-grained idle animation configuration
    */
  def withIdleAnimation(
      idleTimeout: FiniteDuration = 2.minutes,
      overlayConfig: IdleOverlayConfig = IdleOverlayConfig()
  ): SessionBuilder[F] =
    this.copy(
      idleEnabled = true,
      idleDetectorConfig = IdleDetectorConfig(idleTimeout = idleTimeout),
      idleOverlayConfig = overlayConfig
    )

  /** Adds a custom overlay (middleware) created by the user. The effect will be evaluated during session setup.
    *
    * Use this for overlays that require effectful construction (e.g., those that allocate Refs).
    */
  def addMiddleware(overlay: F[Overlay[F]]): SessionBuilder[F] =
    this.copy(customOverlays = customOverlays :+ overlay)

  /** Adds a custom overlay that doesn't require effectful construction. */
  def addMiddleware(overlay: Overlay[F]): SessionBuilder[F] =
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
          if (progressBarEnabled) ProgressBar.make[F](totalSlides).map(Some(_))
          else Monad[F].pure(None: Option[ProgressBar[F]])
        quickNavigation <-
          if (quickNavigationEnabled) QuickNavigation.make[F]().map(Some(_))
          else Monad[F].pure(None: Option[QuickNavigation[F]])
        idleOverlay <-
          if (idleEnabled) Idle.make[F](idleDetector, idleOverlayConfig).map(Some(_))
          else Monad[F].pure(None: Option[Idle[F]])
        customOverlayInstances <- customOverlays.sequence

        // Collect all overlays and register with middleware
        allOverlays = List(
          progressBar.map(x => x: Overlay[F]),
          timerOverlay,
          quickNavigation.map(x => x: Overlay[F]),
          idleOverlay.map(x => x: Overlay[F])
        ).flatten ++ customOverlayInstances
        _ <- consoleWithMiddleware.addOverlays(allOverlays)

        // Wire up quick navigation titles
        _ <- quickNavigation match {
          case Some(qn) => qn.setTiles(presentation.titles)
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
              val userCallback = onSlideChange.fold(Monad[F].unit)(_(index))
              progressUpdate *> idleNotify *> userCallback
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

    slideSteps
      .traverse {
        case SessionBuilder.TextSlideStep(f) =>
          val builder = TextSlideBuilder[F]()
          Monad[F].pure(f(ctx)(builder).build())
        case SessionBuilder.CustomSlide(f) =>
          val builder = SlideBuilder[F]()
          Monad[F].pure(f(ctx)(builder).build())
        case SessionBuilder.EffectfulSlide(f) =>
          f(ctx).map { builderFn =>
            val builder = SlideBuilder[F]()
            builderFn(builder).build()
          }
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
      quickNavigationEnabled = false,
      idleEnabled = false,
      idleDetectorConfig = IdleDetectorConfig(),
      idleOverlayConfig = IdleOverlayConfig(),
      customOverlays = List.empty,
      onSlideChange = None,
      tickerInterval = 40.millis,
      animationStep = AnimationSettings.DefaultStep
    )

  // Internal ADT for storing slide configurations
  private[builders] sealed trait SlideStep[F[_]]

  private[builders] case class TextSlideStep[F[_]](
      f: SlideContext[F] => TextSlideBuilder[F, WithoutContent] => TextSlideBuilder[F, WithContent]
  ) extends SlideStep[F]

  private[builders] case class CustomSlide[F[_]](
      f: SlideContext[F] => SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]
  ) extends SlideStep[F]

  private[builders] case class EffectfulSlide[F[_]](
      f: SlideContext[F] => F[SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]]
  ) extends SlideStep[F]
}
