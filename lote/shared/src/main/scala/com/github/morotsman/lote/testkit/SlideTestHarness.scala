package com.github.morotsman.lote.testkit

import cats.effect.{Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.support.AnimationClock
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

import scala.concurrent.duration.FiniteDuration

/** A complete test harness for unit-testing custom slides, transitions, and overlays.
  *
  * Bundles a `TestConsole` and `TestTicker` together with convenient accessor methods so you don't need to wire up the
  * individual test doubles yourself.
  *
  * Example — testing a custom slide:
  * {{{
  * import cats.effect.IO
  * import com.github.morotsman.lote.api._
  * import com.github.morotsman.lote.testkit.SlideTestHarness
  * import scala.concurrent.duration._
  *
  * for {
  *   harness <- SlideTestHarness.make[IO](screen = Screen(80, 24))
  *   slide   <- MySlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
  *   _       <- slide.startShow
  *   _       <- harness.tick(3)
  *   _       <- slide.userInput(Character('x'))
  *   frames  <- harness.writtenFrames
  *   _       <- slide.stopShow
  * } yield assert(frames.nonEmpty)
  * }}}
  *
  * Example — testing a custom transition (blocks on `Deferred.get`):
  * {{{
  * for {
  *   harness    <- SlideTestHarness.make[IO](screen = Screen(10, 3), tickStep = 5.millis)
  *   fromSlide  = SlideTestHarness.fixedSlide[IO]("FROM")
  *   toSlide    = SlideTestHarness.fixedSlide[IO]("TO")
  *   transition <- MyTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
  *   _          <- harness.runWithTicking(transition.transition(fromSlide, toSlide))
  *   last       <- harness.lastWrittenFrame
  * } yield assert(last.exists(_.contains("TO")))
  * }}}
  */
final class SlideTestHarness[F[_]] private (
    val console: TestConsole[F],
    val ticker: TestTicker[F],
    val animationSettings: AnimationSettings
)(implicit F: Temporal[F]) {

  /** Implicit NConsole instance for use in components that require it. */
  implicit def nConsole: NConsole[F] = console

  /** Implicit Ticker instance for use in components that require it. */
  implicit def tickerInstance: Ticker[F] = ticker

  /** Implicit AnimationSettings instance for use in components that require it. */
  implicit def animationSettingsInstance: AnimationSettings = animationSettings

  /** The simulated clock used by this harness. Advancing it makes `FixedStep` produce simulation steps. */
  def clock: SimulatedClock[F] = ticker.clock

  /** Implicit `AnimationClock[F]` backed by the simulated clock — pass this to `FixedStep` methods. */
  implicit def clockInstance: AnimationClock[F] = clock

  /** The simulated screen dimensions. */
  def screen: Screen = console.screen

  // --- Delegation to TestTicker ---

  /** Fire ticker callbacks the given number of times. */
  def tick(times: Int = 1): F[Unit] = ticker.tick(times)

  // --- Delegation to TestConsole ---

  /** Returns all written frames in reverse chronological order (most recent first). */
  def writtenFrames: F[List[String]] = console.writtenFrames

  /** Returns written frames in chronological order (oldest first). */
  def writtenFramesInOrder: F[List[String]] = console.writtenFramesInOrder

  /** Returns the most recently written frame. */
  def lastWrittenFrame: F[Option[String]] = console.lastWrittenFrame

  /** Returns how many times `clear()` was called. */
  def clearCount: F[Int] = console.clearCount

  /** Enqueue additional user inputs. */
  def enqueueInputs(inputs: List[UserInput]): F[Unit] = console.enqueueInputs(inputs)

  /** Reset recorded state. */
  def reset: F[Unit] = console.reset

  /** Run a blocking operation (e.g. `SessionBuilder.runWith` or `transition.transition`) concurrently with manual
    * ticking.
    *
    * Forks `task` into a background fiber, then fires the ticker one tick at a time with a 1ms sleep between each,
    * giving the task fiber scheduling opportunities to make progress between ticks. After all ticks have fired, waits
    * for the task to complete.
    *
    * Use this for transitions (which block on `Deferred.get`) or full session tests (which block on the executor loop).
    * The explicit tick count keeps the test bounded and predictable.
    *
    * Example:
    * {{{
    * for {
    *   harness <- SlideTestHarness.make[IO](
    *     screen = Screen(80, 10),
    *     inputs = List(Character('a'), Key(SpecialKey.Esc)),
    *     readDelay = 1.millis
    *   )
    *   _ <- harness.runWithTicking(buildSession.runWith(harness.console, harness.ticker), ticks = 20)
    *   frames <- harness.writtenFrames
    * } yield assert(frames.nonEmpty)
    * }}}
    */
  def runWithTicking(task: F[Unit], ticks: Int = 20): F[Unit] = {
    val pause = F.sleep(scala.concurrent.duration.DurationInt(1).millis)
    // Wait until the task fiber has subscribed to the ticker before we start firing ticks.
    // This eliminates the race between fiber setup and the first tick.
    // Uses a bounded retry so transitions that complete without subscribing (e.g. identical slides)
    // don't hang forever.
    val awaitSubscriber: F[Boolean] = {
      def loop(attempts: Int): F[Boolean] =
        if (attempts <= 0) F.pure(false)
        else
          ticker.subscriberCount.flatMap { count =>
            if (count > 0) F.pure(true)
            else F.cede *> pause *> loop(attempts - 1)
          }
      loop(100) // wait up to ~100ms for a subscriber to appear
    }
    for {
      fiber <- task.start
      subscribed <- awaitSubscriber
      _ <-
        if (subscribed)
          (1 to ticks).toList.traverse_(_ => pause *> ticker.tick(1))
        else
          F.unit // task completed (or will complete) without needing ticks
      _ <- fiber.joinWithNever
    } yield ()
  }
}

object SlideTestHarness {

  /** Creates a fully-wired test harness.
    *
    * @param screen
    *   simulated terminal dimensions (default 80×24)
    * @param inputs
    *   pre-loaded user inputs for `console.read()`
    * @param tickStep
    *   duration per tick (used for `IO.sleep` in ticker, default 16ms)
    * @param animationStep
    *   simulation step for `AnimationSettings` (default matches tickStep)
    * @param readDelay
    *   delay before each `console.read()` returns, giving background fibers time to run (default `Duration.Zero`). Set
    *   to e.g. `1.millis` for integration tests that use `SessionBuilder.runWith`.
    */
  def make[F[_]: Temporal: Ref.Make](
      screen: Screen = Screen(80, 24),
      inputs: List[UserInput] = Nil,
      tickStep: FiniteDuration = scala.concurrent.duration.DurationInt(16).millis,
      animationStep: Option[FiniteDuration] = None,
      readDelay: FiniteDuration = scala.concurrent.duration.Duration.Zero
  ): F[SlideTestHarness[F]] =
    for {
      console <- TestConsole.make[F](screen, inputs, readDelay)
      ticker <- TestTicker.make[F](tickStep)
    } yield {
      val resolvedAnimationStep = animationStep.getOrElse(tickStep)
      new SlideTestHarness[F](console, ticker, AnimationSettings(resolvedAnimationStep))
    }

  /** Creates a minimal slide that always returns the given content. Useful as a `from`/`to` stub when testing
    * transitions.
    */
  def fixedSlide[F[_]](slideContent: String)(implicit F: Temporal[F]): Slide[F] =
    new Slide[F] {
      override def content: F[Option[ScreenAdjusted]] =
        F.pure(Some(ScreenAdjusted(slideContent)))
      override def startShow: F[Unit] = F.unit
      override def stopShow: F[Unit] = F.unit
      override def userInput(input: UserInput): F[Unit] = F.unit
    }
}
