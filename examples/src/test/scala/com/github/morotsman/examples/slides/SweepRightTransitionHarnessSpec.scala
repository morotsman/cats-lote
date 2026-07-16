package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.{Key, Screen, SpecialKey}
import com.github.morotsman.lote.api.support.Clock
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

/** Demonstrates testing a custom transition using the `SlideTestHarness`.
  */
class SweepRightTransitionHarnessSpec extends CatsEffectSuite {

  test("sweep completes and shows target slide content") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(4, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBB")
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty, "Expected at least one written frame")
      assertEquals(written.head, "BBBB")
    }
  }

  test("sweep renders intermediate frames during animation") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(4, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBB")
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield {
      val intermediateFrames = written.drop(1)
      assert(
        intermediateFrames.contains("BBAA"),
        s"Expected intermediate frame 'BBAA', got: $intermediateFrames"
      )
    }
  }

  test("sweep clears screen during animation") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(4, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBB")
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- harness.runWithTicking(transition.transition(from, to))
      cleared <- harness.clearCount
    } yield {
      assert(cleared >= 2, s"Expected at least 2 clear() calls, got $cleared")
    }
  }

  test("userInput is a no-op") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test("sweep only processes columns up to content width, not full screen width") {
    // Screen is 80 wide, but content is only 4 chars per line.
    // With columnsPerStep=2, the sweep should complete in ~2 animation steps (4/2),
    // not ~40 steps (80/2).
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBB")
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield {
      // With 4 content columns and 2 columns/step, we expect ~2 intermediate frames + 1 final = ~3 total.
      // If it swept the full 80 columns, we'd see ~40+ frames.
      assert(
        written.length < 10,
        s"Sweep produced ${written.length} frames — it's sweeping empty space beyond content width"
      )
    }
  }

  test("sweep with multi-line content uses max line width across both slides") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 3),
        tickStep = 5.millis
      )
      // 'from' has lines of width 6, 'to' has lines of width 10
      from = SlideTestHarness.fixedSlide[IO]("AAAAAA\nAA\nAAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB\nBB\nBBBB")
      transition = {
        implicit val clock: Clock[IO] = harness.clockInstance
        SweepRightTransition.create[IO](
          columnsPerStep = 2,
          harness.console,
          harness.ticker,
          harness.animationSettings
        )
      }
      _ <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield {
      // Max content width is 10. At 2 cols/step, we expect ~5 intermediate frames + 1 final = ~6 total.
      // If it swept the full 80 columns, we'd see ~40+ frames.
      assert(
        written.length < 15,
        s"Sweep produced ${written.length} frames — should use max content width (10), not screen width (80)"
      )
    }
  }
}
