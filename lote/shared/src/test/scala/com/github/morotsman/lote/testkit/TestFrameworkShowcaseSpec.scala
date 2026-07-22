package com.github.morotsman.lote.testkit

import cats.Applicative
import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.api.support.{AnimationClock, FixedStep}
import com.github.morotsman.lote.api.spi.{NConsole, Overlay, Slide, Ticker, TickerSubscription}
import munit.CatsEffectSuite

import scala.concurrent.duration._

// ════════════════════════════════════════════════════════════════════════════════
//
//  Test Framework Showcase
//
//  This file demonstrates every feature of the cats-lote test toolkit, one
//  concept at a time. Each section builds on the previous one, so reading
//  top-to-bottom gives you a complete tour.
//
//  The toolkit has four pieces:
//
//    1. TestConsole    — a fake terminal that records writes and injects reads
//    2. TestTicker     — a ticker that only fires when you say
//    3. SimulatedClock — a clock that only advances when you say
//    4. SlideTestHarness — bundles the above into a one-liner
//
//  All four work together so your tests run in milliseconds with zero
//  wall-clock delays and fully deterministic behaviour.
//
// ════════════════════════════════════════════════════════════════════════════════

class TestFrameworkShowcaseSpec extends CatsEffectSuite {

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 1: TestConsole — the fake terminal
  //
  //  TestConsole implements NConsole[F] without a real terminal. It:
  //    • records every frame written via writeString()
  //    • serves pre-loaded inputs from read()
  //    • counts clear() calls
  //    • supports text alignment just like the real console
  // ──────────────────────────────────────────────────────────────────────────

  test("TestConsole — writing and reading back frames") {
    // Create a console with a 10×3 screen. No pre-loaded inputs needed here.
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 3))
      // Write two frames. Each writeString() appends to the recorded list.
      _ <- console.writeString(ScreenAdjusted("frame 1"))
      _ <- console.writeString(ScreenAdjusted("frame 2"))
      // writtenFrames returns most-recent-first (reverse chronological).
      frames <- console.writtenFrames
      // writtenFramesInOrder returns oldest-first (chronological).
      ordered <- console.writtenFramesInOrder
      // lastWrittenFrame is a shortcut for the most recent.
      last <- console.lastWrittenFrame
    } yield {
      assertEquals(frames, List("frame 2", "frame 1"))
      assertEquals(ordered, List("frame 1", "frame 2"))
      assertEquals(last, Some("frame 2"))
    }
  }

  test("TestConsole — pre-loaded user inputs") {
    // Pre-load inputs that will be consumed by successive read() calls.
    // When the queue is exhausted, read() returns Key(SpecialKey.Timeout).
    for {
      console <- TestConsole.make[IO](
        inputs = List(Character('a'), Key(SpecialKey.Right), Key(SpecialKey.Esc))
      )
      first <- console.read()
      second <- console.read()
      third <- console.read()
      fourth <- console.read() // queue is empty now
    } yield {
      assertEquals(first, Character('a'))
      assertEquals(second, Key(SpecialKey.Right))
      assertEquals(third, Key(SpecialKey.Esc))
      assertEquals(fourth, Key(SpecialKey.Timeout)) // sentinel for "no more input"
    }
  }

  test("TestConsole — enqueueInputs adds inputs mid-test") {
    // Start with no inputs, then add some dynamically.
    for {
      console <- TestConsole.make[IO]()
      noInput <- console.read()
      _ <- console.enqueueInputs(List(Character('x'), Character('y')))
      afterAdd1 <- console.read()
      afterAdd2 <- console.read()
    } yield {
      assertEquals(noInput, Key(SpecialKey.Timeout))
      assertEquals(afterAdd1, Character('x'))
      assertEquals(afterAdd2, Character('y'))
    }
  }

  test("TestConsole — clear count tracks how often the screen was cleared") {
    for {
      console <- TestConsole.make[IO]()
      _ <- console.clear()
      _ <- console.clear()
      _ <- console.clear()
      count <- console.clearCount
    } yield assertEquals(count, 3)
  }

  test("TestConsole — reset clears all recorded state") {
    for {
      console <- TestConsole.make[IO]()
      _ <- console.writeString(ScreenAdjusted("old"))
      _ <- console.clear()
      _ <- console.reset
      framesAfter <- console.writtenFrames
      clearedAfter <- console.clearCount
    } yield {
      assertEquals(framesAfter, Nil)
      assertEquals(clearedAfter, 0)
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 2: SimulatedClock — time that only moves when you tell it to
  //
  //  In production, AnimationClock[F] reads the real system clock. SimulatedClock
  //  starts at zero and only advances when you call advance(). This makes
  //  animation timing fully deterministic.
  // ──────────────────────────────────────────────────────────────────────────

  test("SimulatedClock — starts at zero and advances on demand") {
    for {
      clock <- SimulatedClock.make[IO]()
      t0 <- clock.currentTime
      _ <- clock.advance(100.millis)
      t1 <- clock.currentTime
      _ <- clock.advance(50.millis)
      t2 <- clock.currentTime
    } yield {
      assertEquals(t0, Duration.Zero)
      assertEquals(t1, 100.millis)
      assertEquals(t2, 150.millis)
    }
  }

  test("SimulatedClock — set() jumps to an absolute time") {
    for {
      clock <- SimulatedClock.make[IO]()
      _ <- clock.advance(200.millis)
      _ <- clock.set(42.millis)
      t <- clock.currentTime
    } yield assertEquals(t, 42.millis)
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 3: FixedStep + SimulatedClock — deterministic animation steps
  //
  //  FixedStep converts elapsed time into a discrete step count. It needs
  //  an AnimationClock[F] to read the current time. Combined with SimulatedClock,
  //  you control exactly how many steps are produced — no flaky timing.
  //
  //  FixedStep.makeRef creates a Ref that tracks the last-read time and
  //  any accumulated remainder. FixedStep.consumeSteps reads the clock,
  //  computes how many full steps fit in the elapsed time, and returns
  //  that count (consuming the time so future calls start from zero).
  // ──────────────────────────────────────────────────────────────────────────

  test("FixedStep — standalone with explicit clock wiring") {
    // When using FixedStep outside of a harness, you wire the clock yourself.
    // FixedStep.makeRef and consumeSteps both take an implicit AnimationClock[F].
    //
    // You also choose the step size — how much simulated time equals one
    // animation step. The library default is AnimationSettings.DefaultStep
    // (40ms), but you can use any value. Here we use 16ms for easy arithmetic.
    val stepSize = 16.millis
    for {
      clock <- SimulatedClock.make[IO]()
      stepperRef <- {
        // The implicit clock tells FixedStep where to read time from.
        implicit val c: AnimationClock[IO] = clock
        FixedStep.makeRef[IO]
      }
      // At this point the clock is at 0ms and the stepper's "last read" is also 0ms.
      // Advance the clock by 48ms.
      _ <- clock.advance(48.millis)
      // consumeSteps checks how much time has passed since the last call.
      // 48ms / 16ms per step = 3 full steps.
      steps <- {
        implicit val c: AnimationClock[IO] = clock
        FixedStep.consumeSteps(stepperRef, stepSize).map(_._1)
      }
    } yield assertEquals(steps, 3)
  }

  test("FixedStep — via harness (clock is shared with ticker)") {
    // When using SlideTestHarness, the ticker and FixedStep share the same
    // SimulatedClock. Calling harness.tick() advances that shared clock,
    // so FixedStep.consumeSteps sees the elapsed time automatically.
    //
    // The harness also provides animationSettings (defaults to tickStep).
    // The consumeSteps overload that takes AnimationSettings uses it as the
    // step size, so you don't need to repeat the duration manually.
    for {
      harness <- SlideTestHarness.make[IO](
        tickStep = 16.millis // each tick advances the clock by 16ms
        // animationStep defaults to tickStep (16ms) when not overridden
      )
      stepper <- {
        implicit val c: AnimationClock[IO] = harness.clockInstance
        FixedStep.makeRef[IO]
      }
      // 3 ticks × 16ms = 48ms of simulated time on the shared clock.
      _ <- harness.tick(3)
      steps <- {
        // Use the overload that reads the step size from AnimationSettings.
        // No need to pass 16.millis explicitly — it comes from the harness.
        implicit val c: AnimationClock[IO] = harness.clockInstance
        implicit val as: AnimationSettings = harness.animationSettings
        FixedStep.consumeSteps(stepper).map(_._1)
      }
    } yield assertEquals(steps, 3) // 48ms / 16ms = 3 steps
  }

  test("FixedStep — accumulates remainder across calls") {
    for {
      clock <- SimulatedClock.make[IO]()
      stepper <- {
        implicit val c: AnimationClock[IO] = clock
        FixedStep.makeRef[IO]
      }
      // Advance 10ms with a 16ms step → not enough for a full step yet.
      _ <- clock.advance(10.millis)
      steps1 <- {
        implicit val c: AnimationClock[IO] = clock
        FixedStep.consumeSteps(stepper, 16.millis).map(_._1)
      }
      // Advance another 10ms → total accumulated = 20ms → 1 step, 4ms remainder.
      _ <- clock.advance(10.millis)
      steps2 <- {
        implicit val c: AnimationClock[IO] = clock
        FixedStep.consumeSteps(stepper, 16.millis).map(_._1)
      }
    } yield {
      assertEquals(steps1, 0) // 10ms < 16ms, no step yet
      assertEquals(steps2, 1) // 10ms + 10ms = 20ms ≥ 16ms, one step consumed
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 4: TestTicker — manual control over the render loop
  //
  //  TestTicker implements Ticker[F]. Subscribers register callbacks that
  //  fire on each tick. In tests, ticks only happen when you call tick().
  //  Each tick also advances the SimulatedClock by the configured step.
  // ──────────────────────────────────────────────────────────────────────────

  test("TestTicker — subscribers fire on each manual tick") {
    for {
      ticker <- TestTicker.make[IO](step = 10.millis)
      counter <- Ref[IO].of(0)
      _ <- ticker.subscribe(counter.update(_ + 1))
      _ <- ticker.tick(3)
      count <- counter.get
      time <- ticker.clock.currentTime
    } yield {
      assertEquals(count, 3) // callback fired 3 times
      assertEquals(time, 30.millis) // clock advanced 3 × 10ms
    }
  }

  test("TestTicker — cancelling a subscription stops callbacks") {
    for {
      ticker <- TestTicker.make[IO](step = 10.millis)
      counter <- Ref[IO].of(0)
      sub <- ticker.subscribe(counter.update(_ + 1))
      _ <- ticker.tick(2)
      _ <- sub.cancel
      _ <- ticker.tick(3) // these ticks should not fire the cancelled callback
      count <- counter.get
    } yield assertEquals(count, 2) // only the first 2 ticks counted
  }

  test("TestTicker — subscriberCount tracks active subscriptions") {
    for {
      ticker <- TestTicker.make[IO]()
      c0 <- ticker.subscriberCount
      sub1 <- ticker.subscribe(IO.unit)
      _ <- ticker.subscribe(IO.unit)
      c2 <- ticker.subscriberCount
      _ <- sub1.cancel
      c1 <- ticker.subscriberCount
    } yield {
      assertEquals(c0, 0)
      assertEquals(c2, 2)
      assertEquals(c1, 1)
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 5: SlideTestHarness — the all-in-one setup
  //
  //  SlideTestHarness bundles TestConsole, TestTicker, SimulatedClock, and
  //  AnimationSettings into a single object. Create one with make(), then
  //  pass its fields to your component under test.
  //
  //  This is the recommended starting point for most tests.
  // ──────────────────────────────────────────────────────────────────────────

  test("SlideTestHarness — create and inspect the bundled components") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(40, 10),
        tickStep = 5.millis
      )
    } yield {
      // The harness exposes its components for passing to your code.
      assertEquals(harness.screen, Screen(40, 10))
      assertEquals(harness.ticker.step, 5.millis)
      // animationSettings.step defaults to tickStep when not overridden.
      assertEquals(harness.animationSettings.step, 5.millis)
    }
  }

  test("SlideTestHarness — custom animationStep independent of tickStep") {
    for {
      harness <- SlideTestHarness.make[IO](
        tickStep = 16.millis,
        animationStep = Some(32.millis)
      )
    } yield {
      // tickStep controls how often the ticker fires.
      assertEquals(harness.ticker.step, 16.millis)
      // animationStep controls how fast animations advance — decoupled from tick rate.
      assertEquals(harness.animationSettings.step, 32.millis)
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 6: Testing a custom Slide
  //
  //  A Slide has a lifecycle: startShow → (ticks fire, user input arrives)
  //  → stopShow. In tests, you drive each phase manually.
  //
  //  The pattern:
  //    1. Create a harness
  //    2. Create your slide, passing harness.console / harness.ticker
  //    3. Call startShow (which typically starts a background animation)
  //    4. Call tick() to advance time and trigger rendering
  //    5. Assert on writtenFrames
  //    6. Call stopShow to clean up
  // ──────────────────────────────────────────────────────────────────────────

  // A minimal slide that writes a counter on each tick, for demonstration.
  private def counterSlide(console: NConsole[IO], ticker: Ticker[IO]): IO[Slide[IO]] =
    Ref[IO].of(0).map { countRef =>
      new Slide[IO] {
        private var subscription: Option[TickerSubscription[IO]] = None

        override def content: IO[Option[ScreenAdjusted]] =
          IO.pure(Some(ScreenAdjusted("counter: 0")))

        override def startShow: IO[Unit] =
          ticker
            .subscribe(
              countRef.updateAndGet(_ + 1).flatMap(n => console.writeString(ScreenAdjusted(s"counter: $n")))
            )
            .flatMap(sub => IO { subscription = Some(sub) }) *> ticker.start

        override def stopShow: IO[Unit] =
          IO(subscription).flatMap(_.traverse_(_.cancel))

        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
    }

  test("Testing a Slide — startShow, tick, assert, stopShow") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 1),
        tickStep = 10.millis
      )
      slide <- counterSlide(harness.console, harness.ticker)
      // Start the slide (registers a ticker subscriber).
      _ <- slide.startShow
      // Tick 3 times — the slide writes a frame on each tick.
      _ <- harness.tick(3)
      frames <- harness.writtenFramesInOrder
      time <- harness.clock.currentTime
      // Clean up.
      _ <- slide.stopShow
    } yield {
      // 3 ticks produced 3 frames, in order.
      assertEquals(frames, List("counter: 1", "counter: 2", "counter: 3"))
      // 3 ticks × 10ms = 30ms of simulated time. No real time passed.
      assertEquals(time, 30.millis)
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 7: fixedSlide — stub slides for transition tests
  //
  //  When testing transitions, you usually don't care about the slide
  //  implementation — you just need something that returns known content.
  //  fixedSlide creates a minimal Slide whose content is a fixed string,
  //  and whose lifecycle methods are all no-ops.
  // ──────────────────────────────────────────────────────────────────────────

  test("fixedSlide — returns constant content with no-op lifecycle") {
    val slide = SlideTestHarness.fixedSlide[IO]("Hello, world!")

    for {
      content <- slide.content
      // startShow / stopShow / userInput are all no-ops — they won't throw.
      _ <- slide.startShow
      _ <- slide.userInput(Character('x'))
      _ <- slide.stopShow
    } yield assertEquals(content.get.content, "Hello, world!")
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 8: runWithTicking — testing blocking operations
  //
  //  Some operations block until they finish (e.g. transitions that wait
  //  on a Deferred, or SessionBuilder.runWith that loops until Esc).
  //  You can't call tick() after them — it would never be reached.
  //
  //  runWithTicking solves this by forking the blocking task into a
  //  background fiber and firing ticks alongside it. A 1ms real-time
  //  pause between ticks ensures the task fiber gets scheduling time.
  //
  //  The pattern:
  //    harness.runWithTicking(blockingTask, ticks = N)
  //
  //  This fires exactly N ticks, then waits for the task to finish.
  // ──────────────────────────────────────────────────────────────────────────

  // A minimal "transition" that blocks on a Deferred until the ticker drives
  // the reveal to completion — similar to how real transitions work.
  private def blockingTransition(
      console: NConsole[IO],
      ticker: Ticker[IO]
  )(implicit clock: AnimationClock[IO]): IO[Unit] = {
    import cats.effect.Deferred
    for {
      done <- Deferred[IO, Unit]
      stepperRef <- FixedStep.makeRef[IO]
      sub <- ticker.subscribe {
        FixedStep.consumeSteps(stepperRef, 10.millis).flatMap { case (steps, _) =>
          if (steps > 0)
            console.writeString(ScreenAdjusted(s"step $steps")) *>
              done.complete(()).attempt.void
          else IO.unit
        }
      }
      _ <- ticker.start
      _ <- done.get // blocks until a tick produces at least one step
      _ <- sub.cancel
    } yield ()
  }

  test("runWithTicking — drives a blocking operation to completion") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(10, 1),
        tickStep = 10.millis
      )
      // blockingTransition blocks on Deferred.get. Without runWithTicking,
      // the test would hang because no ticks would ever fire.
      _ <- harness.runWithTicking {
        implicit val c: AnimationClock[IO] = harness.clockInstance
        blockingTransition(harness.console, harness.ticker)
      }
      frames <- harness.writtenFrames
    } yield assert(frames.nonEmpty, "Expected the blocking transition to write at least one frame")
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 9: Testing a custom Overlay
  //
  //  An Overlay modifies rendered frames (via applyOverlay) and optionally
  //  reacts to user input (via onUserInput). It's applied by the middleware
  //  after a slide writes content.
  //
  //  To test an overlay in isolation, call applyOverlay directly with a
  //  screen context and slide content.
  // ──────────────────────────────────────────────────────────────────────────

  // A simple overlay that prepends a header line to every frame.
  private val headerOverlay: Overlay[IO] = new Overlay[IO] {
    override def applyOverlay(
        context: Screen,
        screenAdjusted: ScreenAdjusted,
        originalContent: ScreenAdjusted
    ): IO[ScreenAdjusted] = {
      val header = s"[${context.screenWidth}x${context.screenHeight}]"
      IO.pure(ScreenAdjusted(header + "\n" + screenAdjusted.content))
    }
  }

  test("Testing an Overlay — applyOverlay in isolation") {
    val screen = Screen(20, 5)
    val slideContent = ScreenAdjusted("Hello")

    for {
      result <- headerOverlay.applyOverlay(screen, slideContent, slideContent)
    } yield {
      assert(result.content.startsWith("[20x5]"))
      assert(result.content.contains("Hello"))
    }
  }

  // An overlay with state — counts how many inputs it has seen.
  private def countingOverlay: IO[Overlay[IO]] =
    Ref[IO].of(0).map { countRef =>
      new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          countRef.get.map(n => ScreenAdjusted(s"inputs: $n\n${screenAdjusted.content}"))

        override def onUserInput(userInput: UserInput)(implicit F: Applicative[IO]): IO[Unit] =
          countRef.update(_ + 1)
      }
    }

  test("Testing an Overlay — onUserInput updates state for applyOverlay") {
    val screen = Screen(20, 5)
    val content = ScreenAdjusted("slide")

    for {
      overlay <- countingOverlay
      before <- overlay.applyOverlay(screen, content, content)
      _ <- overlay.onUserInput(Character('a'))
      _ <- overlay.onUserInput(Character('b'))
      after <- overlay.applyOverlay(screen, content, content)
    } yield {
      assert(before.content.startsWith("inputs: 0"))
      assert(after.content.startsWith("inputs: 2"))
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 10: Using individual test doubles
  //
  //  SlideTestHarness is convenient but opinionated. If you need more
  //  control, use TestConsole, TestTicker, and SimulatedClock individually.
  //  This is useful when testing components that don't need all three,
  //  or when you want different configurations for each.
  // ──────────────────────────────────────────────────────────────────────────

  test("Individual test doubles — mix and match as needed") {
    for {
      // Create components individually with custom settings.
      console <- TestConsole.make[IO](screen = Screen(40, 10))
      ticker <- TestTicker.make[IO](step = 8.millis)
      clock <- SimulatedClock.make[IO]()
      // Use them independently.
      _ <- console.writeString(ScreenAdjusted("standalone"))
      _ <- clock.advance(100.millis)
      _ <- ticker.tick(2)
      // Each has its own state.
      frame <- console.lastWrittenFrame
      time <- clock.currentTime
      tTime <- ticker.clock.currentTime
    } yield {
      assertEquals(frame, Some("standalone"))
      assertEquals(time, 100.millis) // standalone clock
      assertEquals(tTime, 16.millis) // ticker's own clock: 2 × 8ms
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 11: Multi-phase tests with reset
  //
  //  For tests that need to observe behaviour across multiple phases
  //  (e.g. "before direction change" vs. "after direction change"),
  //  use reset() to clear recorded frames between phases.
  // ──────────────────────────────────────────────────────────────────────────

  test("Multi-phase test — reset clears frames between phases") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(10, 1))
      // Phase 1: write some frames.
      _ <- harness.console.writeString(ScreenAdjusted("phase 1"))
      framesBefore <- harness.writtenFrames
      // Reset: clears written frames and clear count.
      _ <- harness.reset
      framesAfterReset <- harness.writtenFrames
      // Phase 2: write new frames and assert only on the new ones.
      _ <- harness.console.writeString(ScreenAdjusted("phase 2"))
      framesPhase2 <- harness.writtenFrames
    } yield {
      assertEquals(framesBefore, List("phase 1"))
      assertEquals(framesAfterReset, Nil)
      assertEquals(framesPhase2, List("phase 2"))
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  //  Section 12: readDelay — simulating keyboard blocking in integration tests
  //
  //  In production, console.read() blocks until the user presses a key.
  //  In tests, pre-loaded inputs are consumed instantly — so fast that
  //  background fibers (like a slide's animation) never get a chance to run.
  //
  //  readDelay adds a small real-time pause before each read() returns,
  //  giving background fibers scheduling time. Use it with
  //  SessionBuilder.runWith for full integration tests.
  // ──────────────────────────────────────────────────────────────────────────

  test("readDelay — inputs are consumed with a pause between each") {
    for {
      console <- TestConsole.make[IO](
        inputs = List(Character('a'), Character('b')),
        readDelay = 1.millis // 1ms pause before each read returns
      )
      t0 <- IO.monotonic
      _ <- console.read()
      _ <- console.read()
      t1 <- IO.monotonic
      elapsed = t1 - t0
    } yield {
      // Two reads × 1ms delay each ≈ 2ms of real time.
      // Allow some slack for timer imprecision (especially on Scala.js).
      assert(elapsed >= 1.millis, s"Expected at least 1ms, got $elapsed")
    }
  }
}
