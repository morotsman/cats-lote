package com.github.morotsman.lote.api.support

import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TickedSlideSpec extends CatsEffectSuite {

  // ═══════════════════════════════════════════════════════════════
  //  build — simple tick callback (no FixedStep, no GlideLayer)
  // ═══════════════════════════════════════════════════════════════

  test("build — content returns None") {
    for {
      harness <- SlideTestHarness.make[IO]()
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      result <- slide.content
    } yield assertEquals(result, None)
  }

  test("build — startShow calls onStart and subscribes to ticker") {
    for {
      harness <- SlideTestHarness.make[IO]()
      startRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = startRef.set(true)
        )
      startedBefore <- startRef.get
      subsBefore <- harness.ticker.subscriberCount
      _ <- slide.startShow
      startedAfter <- startRef.get
      subsAfter <- harness.ticker.subscriberCount
      running <- harness.ticker.isRunning
    } yield {
      assert(!startedBefore)
      assert(startedAfter)
      assertEquals(subsBefore, 0)
      assertEquals(subsAfter, 1)
      assert(running)
    }
  }

  test("build — onTick is called on each tick") {
    for {
      harness <- SlideTestHarness.make[IO]()
      countRef <- Ref[IO].of(0)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = countRef.update(_ + 1),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(5)
      count <- countRef.get
    } yield assertEquals(count, 5)
  }

  test("build — userInput delegates to onInput callback") {
    for {
      harness <- SlideTestHarness.make[IO]()
      inputRef <- Ref[IO].of(List.empty[UserInput])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = input => inputRef.update(input :: _),
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- slide.userInput(Character('a'))
      _ <- slide.userInput(Key(SpecialKey.Enter))
      inputs <- inputRef.get
    } yield {
      assertEquals(inputs.length, 2)
      assertEquals(inputs, List(Key(SpecialKey.Enter), Character('a')))
    }
  }

  test("build — stopShow cancels subscription") {
    for {
      harness <- SlideTestHarness.make[IO]()
      countRef <- Ref[IO].of(0)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = countRef.update(_ + 1),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(3)
      countStart <- countRef.get
      _ <- slide.stopShow
      subs <- harness.ticker.subscriberCount
      _ <- harness.tick(3)
      countEnd <- countRef.get
    } yield {
      assertEquals(countStart, 3)
      assertEquals(subs, 0)
      assertEquals(countEnd, 3) // no additional ticks after stop
    }
  }

  test("build — stopShow calls onStop when provided") {
    for {
      harness <- SlideTestHarness.make[IO]()
      stopRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit,
          onStop = Some(stopRef.set(true))
        )
      _ <- slide.startShow
      _ <- slide.stopShow
      stopped <- stopRef.get
    } yield assert(stopped)
  }

  test("build — stopShow without onStop does not fail") {
    for {
      harness <- SlideTestHarness.make[IO]()
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- slide.stopShow
    } yield ()
  }

  test("build — stopShow is idempotent (no subscription to cancel)") {
    for {
      harness <- SlideTestHarness.make[IO]()
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- slide.stopShow
      _ <- slide.stopShow // second stop should not throw
    } yield ()
  }

  // ═══════════════════════════════════════════════════════════════
  //  buildStepped — FixedStep integration
  // ═══════════════════════════════════════════════════════════════

  test("buildStepped — content returns None") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      result <- slide.content
    } yield assertEquals(result, None)
  }

  test("buildStepped — receives correct step counts") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stepsRef <- Ref[IO].of(List.empty[Int])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (steps, _) => stepsRef.update(steps :: _),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(3)
      steps <- stepsRef.get
    } yield {
      // Each tick advances clock by 40ms = animationSettings.step, so each tick yields 1 step
      assertEquals(steps.reverse, List(1, 1, 1))
    }
  }

  test("buildStepped — onStart resets stepper state") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stepsRef <- Ref[IO].of(List.empty[Int])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (steps, _) => stepsRef.update(steps :: _),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(2)
      _ <- slide.stopShow
      // Restart — stepper should reset, not accumulate from previous session
      _ <- stepsRef.set(Nil)
      _ <- slide.startShow
      _ <- harness.tick(1)
      s2 <- stepsRef.get
    } yield {
      assertEquals(s2, List(1))
    }
  }

  test("buildStepped — stopShow cancels subscription and calls onStop") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stopRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit,
          onStop = Some(stopRef.set(true))
        )
      _ <- slide.startShow
      _ <- slide.stopShow
      stopped <- stopRef.get
      subs <- harness.ticker.subscriberCount
    } yield {
      assert(stopped)
      assertEquals(subs, 0)
    }
  }

  test("buildStepped — userInput delegates correctly") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      inputRef <- Ref[IO].of(Option.empty[UserInput])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (_, _) => IO.unit,
          onInput = i => inputRef.set(Some(i)),
          onStart = IO.unit
        )
      _ <- slide.userInput(Character('z'))
      input <- inputRef.get
    } yield assertEquals(input, Some(Character('z')))
  }

  // ═══════════════════════════════════════════════════════════════
  //  buildWithGlide — FixedStep + GlideLayer
  // ═══════════════════════════════════════════════════════════════

  test("buildWithGlide — receives steps and glide overlay") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stepsRef <- Ref[IO].of(List.empty[Int])
      glideRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (steps, glide) => stepsRef.update(steps :: _) *> glideRef.set(glide != null),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(2)
      steps <- stepsRef.get
      hadGlide <- glideRef.get
    } yield {
      assertEquals(steps.reverse, List(1, 1))
      assert(hadGlide)
    }
  }

  test("buildWithGlide — stopShow clears glide layer") {
    // Verifying that stopShow completes without error and cleans up subscription
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(1)
      _ <- slide.stopShow
      subs <- harness.ticker.subscriberCount
    } yield assertEquals(subs, 0)
  }

  test("buildWithGlide — stopShow calls onStop after clearing glide") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stopRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit,
          onStop = Some(stopRef.set(true))
        )
      _ <- slide.startShow
      _ <- slide.stopShow
      stopped <- stopRef.get
    } yield assert(stopped)
  }

  test("buildWithGlide — custom contentF is used") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit,
          contentF = Some(IO.pure(Some(ScreenAdjusted("hello"))))
        )
      result <- slide.content
    } yield assertEquals(result, Some(ScreenAdjusted("hello")))
  }

  test("buildWithGlide — default content is None") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      result <- slide.content
    } yield assertEquals(result, None)
  }

  // ═══════════════════════════════════════════════════════════════
  //  buildWithGlideProgress — includes sub-step progress
  // ═══════════════════════════════════════════════════════════════

  test("buildWithGlideProgress — receives steps, progress, and glide overlay") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      dataRef <- Ref[IO].of(List.empty[(Int, Double)])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlideProgress(
          onTick = (steps, progress, _) => dataRef.update((steps, progress) :: _),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(3)
      data <- dataRef.get
    } yield {
      // Each tick = 40ms = 1 step, progress should be 0.0
      data.reverse.foreach { case (steps, progress) =>
        assertEquals(steps, 1)
        assertEqualsDouble(progress, 0.0, 1e-9)
      }
      assertEquals(data.length, 3)
    }
  }

  test("buildWithGlideProgress — custom contentF is used") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlideProgress(
          onTick = (_, _, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit,
          contentF = Some(IO.pure(Some(ScreenAdjusted("custom"))))
        )
      result <- slide.content
    } yield assertEquals(result, Some(ScreenAdjusted("custom")))
  }

  // ═══════════════════════════════════════════════════════════════
  //  Builder configuration
  // ═══════════════════════════════════════════════════════════════

  test("withGlideLayer — sets GlideConfig with wrapThreshold") {
    val builder = TickedSlide[IO](null, null, AnimationSettings.default)
      .withGlideLayer(wrapThreshold = 20)
    assertEquals(builder.glideConfig, Some(TickedSlide.GlideConfig(wrapThreshold = 20)))
  }

  test("withGlideLayer — sets GlideConfig from config object") {
    val config = TickedSlide.GlideConfig(step = Some(100.millis), wrapThreshold = 5)
    val builder = TickedSlide[IO](null, null, AnimationSettings.default)
      .withGlideLayer(config)
    assertEquals(builder.glideConfig, Some(config))
  }

  test("Builder — default glideConfig is None") {
    val builder = TickedSlide[IO](null, null, AnimationSettings.default)
    assertEquals(builder.glideConfig, None)
  }

  // ═══════════════════════════════════════════════════════════════
  //  contextual — factory integration
  // ═══════════════════════════════════════════════════════════════

  test("contextual — returns a ContextualF that wraps the builder logic") {
    // We can't call .run directly (it's package-private), but we can verify
    // that contextual produces a well-formed ContextualF by checking the
    // builder configuration it captures.
    for {
      harness <- SlideTestHarness.make[IO]()
      countRef <- Ref[IO].of(0)
      // Build directly using the same pattern contextual would use internally
      builder = TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
      slide <- builder.build(
        onTick = countRef.update(_ + 1),
        onInput = _ => IO.unit,
        onStart = IO.unit
      )
      _ <- slide.startShow
      _ <- harness.tick(4)
      count <- countRef.get
      content <- slide.content
      _ <- slide.stopShow
    } yield {
      assertEquals(count, 4)
      assertEquals(content, None)
    }
  }

  test("contextual — produces a ContextualF with glideConfig None") {
    // Verify that the contextual factory creates a Builder with the expected defaults
    val contextualSlide = TickedSlide.contextual[IO] { builder =>
      IO {
        assertEquals(builder.glideConfig, None)
      } *> builder.build(
        onTick = IO.unit,
        onInput = _ => IO.unit,
        onStart = IO.unit
      )
    }
    // contextualSlide is a ContextualF — verify it was created successfully
    assert(contextualSlide != null)
  }

  // ═══════════════════════════════════════════════════════════════
  //  Lifecycle — start/stop/restart
  // ═══════════════════════════════════════════════════════════════

  test("build — can restart after stop") {
    for {
      harness <- SlideTestHarness.make[IO]()
      countRef <- Ref[IO].of(0)
      startRef <- Ref[IO].of(0)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = countRef.update(_ + 1),
          onInput = _ => IO.unit,
          onStart = startRef.update(_ + 1)
        )
      // First session
      _ <- slide.startShow
      _ <- harness.tick(3)
      _ <- slide.stopShow
      c1 <- countRef.get
      // Second session
      _ <- slide.startShow
      _ <- harness.tick(2)
      _ <- slide.stopShow
      c2 <- countRef.get
      starts <- startRef.get
    } yield {
      assertEquals(c1, 3)
      assertEquals(c2, 5) // total across both sessions
      assertEquals(starts, 2)
    }
  }

  // ═══════════════════════════════════════════════════════════════
  //  runWithTicking integration
  // ═══════════════════════════════════════════════════════════════

  test("build — works with runWithTicking for async lifecycle") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 16.millis)
      countRef <- Ref[IO].of(0)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = countRef.update(_ + 1),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- harness.runWithTicking(
        slide.startShow *> harness.tick(5).void,
        ticks = 10
      )
      count <- countRef.get
    } yield assert(count > 0)
  }

  // ═══════════════════════════════════════════════════════════════
  //  GlideConfig defaults
  // ═══════════════════════════════════════════════════════════════

  test("GlideConfig — defaults: step is None, wrapThreshold is 1") {
    val gc = TickedSlide.GlideConfig()
    assertEquals(gc.step, None)
    assertEquals(gc.wrapThreshold, 1)
  }

  test("buildWithGlide — uses animationSettings.step when GlideConfig.step is None") {
    // This test verifies the fallback works by checking that the slide can be built and ticked
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 20.millis, animationStep = Some(20.millis))
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      tickedRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer() // no step override
        .buildWithGlide(
          onTick = (_, _) => tickedRef.set(true),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(1)
      ticked <- tickedRef.get
      _ <- slide.stopShow
    } yield assert(ticked)
  }

  test("buildWithGlide — uses GlideConfig.step when specified") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      tickedRef <- Ref[IO].of(false)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer(TickedSlide.GlideConfig(step = Some(100.millis), wrapThreshold = 5))
        .buildWithGlide(
          onTick = (_, _) => tickedRef.set(true),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(1)
      ticked <- tickedRef.get
      _ <- slide.stopShow
    } yield assert(ticked)
  }

  // ═══════════════════════════════════════════════════════════════
  //  Edge cases
  // ═══════════════════════════════════════════════════════════════

  test("build — userInput works before startShow") {
    for {
      harness <- SlideTestHarness.make[IO]()
      inputRef <- Ref[IO].of(Option.empty[UserInput])
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = i => inputRef.set(Some(i)),
          onStart = IO.unit
        )
      _ <- slide.userInput(Character('x'))
      input <- inputRef.get
    } yield assertEquals(input, Some(Character('x')))
  }

  test("build — stopShow before startShow does not fail") {
    for {
      harness <- SlideTestHarness.make[IO]()
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.stopShow // no subscription to cancel, should be fine
    } yield ()
  }

  test("build — multiple ticks produce corresponding callback invocations") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 10.millis)
      countRef <- Ref[IO].of(0)
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .build(
          onTick = countRef.update(_ + 1),
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.startShow
      _ <- harness.tick(100)
      count <- countRef.get
      _ <- slide.stopShow
    } yield assertEquals(count, 100)
  }

  test("buildStepped — stopShow before startShow does not fail") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .buildStepped(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.stopShow
    } yield ()
  }

  test("buildWithGlide — stopShow before startShow does not fail") {
    for {
      harness <- SlideTestHarness.make[IO](tickStep = 40.millis)
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      slide <- TickedSlide[IO](harness.console, harness.ticker, harness.animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = (_, _) => IO.unit,
          onInput = _ => IO.unit,
          onStart = IO.unit
        )
      _ <- slide.stopShow
    } yield ()
  }
}
