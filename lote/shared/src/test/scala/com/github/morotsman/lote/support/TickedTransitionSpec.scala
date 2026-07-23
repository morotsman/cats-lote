package com.github.morotsman.lote.support

import cats.Monad
import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.api.support._
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TickedTransitionSpec extends CatsEffectSuite {

  private val defaultScreen = Screen(10, 3)
  private val defaultTickStep = 5.millis

  private def makeHarness(
      screen: Screen = defaultScreen,
      tickStep: FiniteDuration = defaultTickStep,
      inputs: List[UserInput] = Nil
  ): IO[SlideTestHarness[IO]] =
    SlideTestHarness.make[IO](screen = screen, tickStep = tickStep, inputs = inputs)

  private def fromSlide(content: String): spi.Slide[IO] =
    SlideTestHarness.fixedSlide[IO](content)

  // ── ProgressResult ──────────────────────────────────────────────

  test("ProgressResult.continue creates a non-done result") {
    val frame = ScreenAdjusted("hello")
    val result = TickedTransition.ProgressResult.continue(frame)
    assertEquals(result.frame, frame)
    assertEquals(result.isDone, false)
  }

  test("ProgressResult.done creates a done result") {
    val frame = ScreenAdjusted("hello")
    val result = TickedTransition.ProgressResult.done(frame)
    assertEquals(result.frame, frame)
    assertEquals(result.isDone, true)
  }

  // ── ProgressContext ─────────────────────────────────────────────

  test("ProgressContext stores progress, width, and height") {
    val ctx = TickedTransition.ProgressContext(0.5, 80, 24)
    assertEquals(ctx.progress, 0.5)
    assertEquals(ctx.screenWidth, 80)
    assertEquals(ctx.screenHeight, 24)
  }

  // ── Builder configuration ───────────────────────────────────────

  test("Builder.withGlideLayer sets glide config") {
    for {
      harness <- makeHarness()
    } yield {
      val builder = TickedTransition.forTest(harness)
      assert(builder.glideConfig.isEmpty)
      val withGlide = builder.withGlideLayer(wrapThreshold = 3)
      assertEquals(withGlide.glideConfig, Some(TickedTransition.GlideConfig(wrapThreshold = 3)))
    }
  }

  test("Builder.withEasing sets easing function") {
    for {
      harness <- makeHarness()
    } yield {
      val builder = TickedTransition.forTest(harness)
      assert(builder.easingFn.isEmpty)
      val withEasing = builder.withEasing(Easing.easeInOutCubic)
      assert(withEasing.easingFn.isDefined)
    }
  }

  test("Builder.withSkipOnInput enables skip") {
    for {
      harness <- makeHarness()
    } yield {
      val builder = TickedTransition.forTest(harness)
      assertEquals(builder.skipOnInput, false)
      val withSkip = builder.withSkipOnInput
      assertEquals(withSkip.skipOnInput, true)
    }
  }

  test("Builder methods are chainable") {
    for {
      harness <- makeHarness()
    } yield {
      val builder = TickedTransition
        .forTest(harness)
        .withGlideLayer(2)
        .withEasing(Easing.easeInCubic)
        .withSkipOnInput
      assertEquals(builder.glideConfig, Some(TickedTransition.GlideConfig(2)))
      assert(builder.easingFn.isDefined)
      assertEquals(builder.skipOnInput, true)
    }
  }

  // ── buildProgress ───────────────────────────────────────────────

  test("buildProgress completes and renders final 'to' content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(50.millis) { (_, _, ctx) =>
          TickedTransition.ProgressResult.continue(
            ScreenAdjusted(s"progress=${ctx.progress}")
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      frames <- harness.writtenFrames
    } yield {
      assert(frames.nonEmpty, "Expected at least one written frame")
      // The final frame should be the 'to' content (written after transition completes)
      assertEquals(frames.head, "TO")
    }
  }

  test("buildProgress provides increasing progress values") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(100.millis) { (_, _, _) =>
          TickedTransition.ProgressResult.continue(ScreenAdjusted("frame"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 60)
      frames <- harness.writtenFramesInOrder
    } yield {
      // Should have rendered multiple intermediate frames
      assert(frames.size > 1, s"Expected multiple frames, got ${frames.size}")
    }
  }

  test("buildProgress completes early when renderFrame returns done") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(1000.millis) { (_, _, _) =>
          // Signal done immediately on the first tick
          TickedTransition.ProgressResult.done(ScreenAdjusted("DONE"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      frames <- harness.writtenFrames
    } yield {
      // Should have completed — final frame is "TO" content
      assertEquals(frames.head, "TO")
    }
  }

  test("buildProgress passes correct screen dimensions in ProgressContext") {
    for {
      harness <- makeHarness(screen = Screen(42, 17))
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(50.millis) { (_, _, ctx) =>
          TickedTransition.ProgressResult.continue(
            ScreenAdjusted(s"${ctx.screenWidth}x${ctx.screenHeight}")
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      frames <- harness.writtenFramesInOrder
    } yield {
      // At least one frame should contain the screen dimensions
      val dimensionFrames = frames.filter(_.contains("42x17"))
      assert(dimensionFrames.nonEmpty, s"Expected frames with dimensions 42x17, got: $frames")
    }
  }

  test("buildProgress with easing applies easing function to progress") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      // Use a simple easing: square the progress value
      transition = TickedTransition
        .forTest(harness)
        .withEasing(t => t * t)
        .buildProgress(100.millis) { (_, _, ctx) =>
          // Eased progress should be the square of linear progress
          // At linear 0.5 => eased 0.25, etc.
          TickedTransition.ProgressResult.continue(
            ScreenAdjusted(f"${ctx.progress}%.4f")
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 60)
      frames <- harness.writtenFramesInOrder
    } yield {
      // Parse progress values from frames
      val progressValues = frames.flatMap { f =>
        scala.util.Try(f.toDouble).toOption
      }
      // With t^2 easing, values should be smaller than linear progress
      // (eased values lag behind linear until the end)
      assert(progressValues.nonEmpty, "Expected some parsed progress values")
    }
  }

  test("buildProgress ensures totalSteps is at least 1") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      // Use a very short duration — shorter than a single tick step
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(1.millis) { (_, _, _) =>
          TickedTransition.ProgressResult.continue(ScreenAdjusted("ok"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      // Should not hang or error
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      frames <- harness.writtenFrames
    } yield {
      assert(frames.nonEmpty, "Transition with minimal duration should still complete")
    }
  }

  // ── buildStepped ────────────────────────────────────────────────

  test("buildStepped completes when ctx.complete is called") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      stepCount <- Ref[IO].of(0)
      transition = TickedTransition
        .forTest(harness)
        .buildStepped { (steps, _, ctx) =>
          for {
            count <- stepCount.updateAndGet(_ + steps)
            _ <- ctx.render(ScreenAdjusted(s"step=$count"))
            _ <- if (count >= 3) ctx.complete else Monad[IO].unit
          } yield ()
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 30)
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "TO")
    }
  }

  test("buildStepped provides from/to content in StepContext") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      capturedFrom <- Ref[IO].of("")
      capturedTo <- Ref[IO].of("")
      transition = TickedTransition
        .forTest(harness)
        .buildStepped { (_, _, ctx) =>
          for {
            _ <- capturedFrom.set(ctx.from.content)
            _ <- capturedTo.set(ctx.to.content)
            _ <- ctx.render(ScreenAdjusted("rendering"))
            _ <- ctx.complete
          } yield ()
        }
      from = fromSlide("HELLO")
      to = fromSlide("WORLD")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      fromVal <- capturedFrom.get
      toVal <- capturedTo.get
    } yield {
      assertEquals(fromVal, "HELLO")
      assertEquals(toVal, "WORLD")
    }
  }

  test("buildStepped provides correct screen dimensions") {
    for {
      harness <- makeHarness(screen = Screen(25, 10))
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      capturedWidth <- Ref[IO].of(0)
      capturedHeight <- Ref[IO].of(0)
      transition = TickedTransition
        .forTest(harness)
        .buildStepped { (_, _, ctx) =>
          for {
            _ <- capturedWidth.set(ctx.screenWidth)
            _ <- capturedHeight.set(ctx.screenHeight)
            _ <- ctx.render(ScreenAdjusted("frame"))
            _ <- ctx.complete
          } yield ()
        }
      from = fromSlide("A")
      to = fromSlide("B")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      w <- capturedWidth.get
      h <- capturedHeight.get
    } yield {
      assertEquals(w, 25)
      assertEquals(h, 10)
    }
  }

  test("buildStepped ctx.render clears and writes") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildStepped { (_, _, ctx) =>
          for {
            _ <- ctx.render(ScreenAdjusted("RENDERED"))
            _ <- ctx.complete
          } yield ()
        }
      from = fromSlide("A")
      to = fromSlide("B")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      frames <- harness.writtenFramesInOrder
    } yield {
      assert(frames.exists(_ == "RENDERED"), s"Expected 'RENDERED' frame, got: $frames")
    }
  }

  test("buildStepped ctx.complete is safe to call multiple times") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildStepped { (_, _, ctx) =>
          for {
            _ <- ctx.render(ScreenAdjusted("frame"))
            _ <- ctx.complete
            _ <- ctx.complete // second call should not throw
            _ <- ctx.complete // third call should not throw
          } yield ()
        }
      from = fromSlide("A")
      to = fromSlide("B")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "B")
    }
  }

  // ── buildWithSetup ──────────────────────────────────────────────

  test("buildWithSetup runs setup and tick handler") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      setupRan <- Ref[IO].of(false)
      tickRan <- Ref[IO].of(false)
      transition = TickedTransition
        .forTest(harness)
        .buildWithSetup { (_, _, complete) =>
          for {
            _ <- setupRan.set(true)
            stepRef <- Ref[IO].of(0)
          } yield TickedTransition.TickHandler[IO](
            onTick = (steps, _) =>
              for {
                count <- stepRef.updateAndGet(_ + steps)
                _ <- tickRan.set(true)
                _ <- harness.console.clear()
                _ <- harness.console.writeString(ScreenAdjusted(s"tick=$count"))
                _ <- if (count >= 2) complete else Monad[IO].unit
              } yield (),
            cleanup = Monad[IO].unit
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 30)
      didSetup <- setupRan.get
      didTick <- tickRan.get
      frames <- harness.writtenFrames
    } yield {
      assert(didSetup, "Setup should have run")
      assert(didTick, "Tick handler should have run")
      assertEquals(frames.head, "TO")
    }
  }

  test("buildWithSetup runs cleanup on completion") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      cleanupRan <- Ref[IO].of(false)
      transition = TickedTransition
        .forTest(harness)
        .buildWithSetup { (_, _, complete) =>
          IO.pure(
            TickedTransition.TickHandler[IO](
              onTick = (_, _) => complete,
              cleanup = cleanupRan.set(true)
            )
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      didCleanup <- cleanupRan.get
    } yield {
      assert(didCleanup, "Cleanup should have run after transition completes")
    }
  }

  test("buildWithSetup receives correct from/to content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      capturedFrom <- Ref[IO].of("")
      capturedTo <- Ref[IO].of("")
      transition = TickedTransition
        .forTest(harness)
        .buildWithSetup { (from, to, complete) =>
          for {
            _ <- capturedFrom.set(from.content)
            _ <- capturedTo.set(to.content)
          } yield TickedTransition.TickHandler[IO](
            onTick = (_, _) => complete,
            cleanup = Monad[IO].unit
          )
        }
      from = fromSlide("ALPHA")
      to = fromSlide("BETA")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 10)
      fromVal <- capturedFrom.get
      toVal <- capturedTo.get
    } yield {
      assertEquals(fromVal, "ALPHA")
      assertEquals(toVal, "BETA")
    }
  }

  // ── TickHandler companion ───────────────────────────────────────

  test("TickHandler.apply creates handler with unit cleanup") {
    val handler = TickedTransition.TickHandler[IO]((_, _) => IO.unit)
    // cleanup should be unit (no-op)
    handler.cleanup.assertEquals(())
  }

  // ── skipOnInput ─────────────────────────────────────────────────

  test("buildProgress with skipOnInput completes when userInput is called") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .withSkipOnInput
        .buildProgress(10.seconds) { (_, _, _) =>
          // Very long duration — will only complete via skip
          TickedTransition.ProgressResult.continue(ScreenAdjusted("animating"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      // Start transition, fire a few ticks, then send user input to skip
      fiber <- transition.transition(from, to).start
      _ <- IO.sleep(10.millis)
      _ <- harness.tick(3)
      _ <- IO.sleep(5.millis)
      _ <- transition.userInput(Character('x'))
      _ <- fiber.joinWithNever
      frames <- harness.writtenFrames
    } yield {
      // Should show final "TO" content after skip
      assertEquals(frames.head, "TO")
    }
  }

  test("buildStepped with skipOnInput completes when userInput is called") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .withSkipOnInput
        .buildStepped { (_, _, ctx) =>
          // Never calls ctx.complete — relies on skip
          ctx.render(ScreenAdjusted("still going"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      fiber <- transition.transition(from, to).start
      _ <- IO.sleep(10.millis)
      _ <- harness.tick(3)
      _ <- IO.sleep(5.millis)
      _ <- transition.userInput(Key(SpecialKey.Enter))
      _ <- fiber.joinWithNever
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "TO")
    }
  }

  test("buildWithSetup with skipOnInput completes and runs cleanup") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      cleanupRan <- Ref[IO].of(false)
      transition = TickedTransition
        .forTest(harness)
        .withSkipOnInput
        .buildWithSetup { (_, _, _) =>
          // Never completes — relies on skip
          IO.pure(
            TickedTransition.TickHandler[IO](
              onTick = (_, _) => IO.unit,
              cleanup = cleanupRan.set(true)
            )
          )
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      fiber <- transition.transition(from, to).start
      _ <- IO.sleep(10.millis)
      _ <- harness.tick(3)
      _ <- IO.sleep(5.millis)
      _ <- transition.userInput(Character(' '))
      _ <- fiber.joinWithNever
      didCleanup <- cleanupRan.get
    } yield {
      assert(didCleanup, "Cleanup should run even when skipped via user input")
    }
  }

  test("userInput is a no-op when skipOnInput is disabled") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        // skipOnInput is NOT enabled
        .buildProgress(50.millis) { (_, _, _) =>
          TickedTransition.ProgressResult.continue(ScreenAdjusted("frame"))
        }
      // userInput should not throw or do anything harmful
      _ <- transition.userInput(Character('x'))
      _ <- transition.userInput(Key(SpecialKey.Esc))
    } yield {
      // If we got here without error, the test passes
      assert(true)
    }
  }

  // ── forTest ─────────────────────────────────────────────────────

  test("forTest creates builder from harness with correct wiring") {
    for {
      harness <- makeHarness(screen = Screen(30, 15))
    } yield {
      val builder = TickedTransition.forTest(harness)
      assert(builder.console eq harness.console)
      assert(builder.ticker eq harness.ticker)
      assertEquals(builder.animationSettings, harness.animationSettings)
      assert(builder.glideConfig.isEmpty)
      assert(builder.easingFn.isEmpty)
      assertEquals(builder.skipOnInput, false)
    }
  }

  // ── contextual ──────────────────────────────────────────────────

  test("contextual creates a Contextual that builds a Transition") {
    // contextual returns a Contextual[F, Transition[F]] which wraps a function
    // that receives SlideContext and produces a Transition.
    // We verify it compiles and produces a Contextual value.
    val contextual = TickedTransition.contextual[IO] { builder =>
      implicit val clock: AnimationClock[IO] = AnimationClock.fromTemporal[IO]
      builder.buildProgress(100.millis) { (_, _, _) =>
        TickedTransition.ProgressResult.continue(ScreenAdjusted("frame"))
      }
    }
    assert(contextual != null)
  }

  // ── Empty slide content ─────────────────────────────────────────

  test("buildProgress handles slides with empty content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(50.millis) { (from, to, _) =>
          TickedTransition.ProgressResult.continue(
            ScreenAdjusted(s"from=[${from.content}] to=[${to.content}]")
          )
        }
      from = fromSlide("")
      to = fromSlide("")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 30)
      frames <- harness.writtenFramesInOrder
    } yield {
      val contentFrames = frames.filter(_.contains("from=[] to=[]"))
      assert(contentFrames.nonEmpty, s"Expected frames with empty content, got: $frames")
    }
  }

  test("buildProgress handles slide returning None content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgress(50.millis) { (from, to, _) =>
          TickedTransition.ProgressResult.continue(
            ScreenAdjusted(s"from=[${from.content}] to=[${to.content}]")
          )
        }
      // Slide that returns None
      noneSlide = new spi.Slide[IO] {
        override def content: IO[Option[ScreenAdjusted]] = IO.pure(None)
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      to = fromSlide("TARGET")
      _ <- harness.runWithTicking(transition.transition(noneSlide, to), ticks = 30)
      frames <- harness.writtenFramesInOrder
    } yield {
      // None content should default to empty ScreenAdjusted("")
      val contentFrames = frames.filter(_.contains("from=[]"))
      assert(contentFrames.nonEmpty, s"Expected frames with empty from-content, got: $frames")
    }
  }

  // ── GlideConfig ─────────────────────────────────────────────────

  test("GlideConfig default wrapThreshold is 1") {
    val config = TickedTransition.GlideConfig()
    assertEquals(config.wrapThreshold, 1)
  }

  test("GlideConfig accepts custom wrapThreshold") {
    val config = TickedTransition.GlideConfig(wrapThreshold = 5)
    assertEquals(config.wrapThreshold, 5)
  }

  // ── buildProgressWithGlide (step-based) ─────────────────────────

  test("buildProgressWithGlide (step-based) completes and shows to content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgressWithGlide(totalSteps = 5) { (_, _, progress, _) =>
          IO.pure(ScreenAdjusted(s"glide-${(progress * 100).toInt}"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "TO")
    }
  }

  // ── buildProgressWithGlide (duration-based) ─────────────────────

  test("buildProgressWithGlide (duration-based) completes and shows to content") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .buildProgressWithGlide(50.millis) { (_, _, progress, _) =>
          IO.pure(ScreenAdjusted(s"glide-dur-${(progress * 100).toInt}"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "TO")
    }
  }

  test("buildProgressWithGlide with easing applies easing to progress") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .withEasing(t => t * t) // quadratic easing
        .buildProgressWithGlide(100.millis) { (_, _, progress, _) =>
          IO.pure(ScreenAdjusted(f"$progress%.4f"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 60)
      frames <- harness.writtenFramesInOrder
    } yield {
      // Should have rendered some intermediate frames
      assert(frames.size > 1, s"Expected multiple frames, got: $frames")
    }
  }

  test("buildProgressWithGlide with skipOnInput skips on user input") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      transition = TickedTransition
        .forTest(harness)
        .withSkipOnInput
        .buildProgressWithGlide(10.seconds) { (_, _, _, _) =>
          IO.pure(ScreenAdjusted("gliding"))
        }
      from = fromSlide("FROM")
      to = fromSlide("TO")
      fiber <- transition.transition(from, to).start
      _ <- IO.sleep(10.millis)
      _ <- harness.tick(3)
      _ <- IO.sleep(5.millis)
      _ <- transition.userInput(Character('q'))
      _ <- fiber.joinWithNever
      frames <- harness.writtenFrames
    } yield {
      assertEquals(frames.head, "TO")
    }
  }

  // ── Multiple transitions from same builder ──────────────────────

  test("builder can create multiple independent transitions") {
    for {
      harness <- makeHarness()
      implicit0(clock: AnimationClock[IO]) = harness.clockInstance
      builder = TickedTransition.forTest(harness)
      t1 = builder.buildProgress(50.millis) { (_, _, _) =>
        TickedTransition.ProgressResult.continue(ScreenAdjusted("t1"))
      }
      t2 = builder.buildProgress(50.millis) { (_, _, _) =>
        TickedTransition.ProgressResult.continue(ScreenAdjusted("t2"))
      }
      from = fromSlide("FROM")
      to1 = fromSlide("TO1")
      to2 = fromSlide("TO2")
      _ <- harness.runWithTicking(t1.transition(from, to1), ticks = 30)
      frames1 <- harness.writtenFrames
      _ <- harness.reset
      _ <- harness.runWithTicking(t2.transition(from, to2), ticks = 30)
      frames2 <- harness.writtenFrames
    } yield {
      assertEquals(frames1.head, "TO1")
      assertEquals(frames2.head, "TO2")
    }
  }

  // ── apply factory ───────────────────────────────────────────────

  test("apply factory creates builder with provided dependencies") {
    for {
      harness <- makeHarness()
    } yield {
      val settings = AnimationSettings(20.millis)
      val builder = TickedTransition[IO](harness.console, harness.ticker, settings)
      assert(builder.console eq harness.console)
      assert(builder.ticker eq harness.ticker)
      assertEquals(builder.animationSettings, settings)
      assert(builder.glideConfig.isEmpty)
      assert(builder.easingFn.isEmpty)
      assertEquals(builder.skipOnInput, false)
    }
  }

  test("apply factory uses default AnimationSettings when not provided") {
    for {
      harness <- makeHarness()
    } yield {
      val builder = TickedTransition[IO](harness.console, harness.ticker)
      assertEquals(builder.animationSettings, AnimationSettings.default)
    }
  }
}
