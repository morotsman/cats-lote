package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class SimpleEasedWipeEarlyExitSpec extends CatsEffectSuite {

  private def createTransition(harness: SlideTestHarness[IO], duration: FiniteDuration = 10000.millis) = {
    implicit val clock: AnimationClock[IO] = harness.clockInstance
    SimpleEasedWipeTransition.create[IO](duration, TickedTransition.forTest(harness))
  }

  test("early exit: transition completes proportionally for short content") {
    // Screen is 20 rows tall, content is only 3 rows.
    // Duration = 1000ms with 5ms step = 200 totalSteps for full screen (20 rows).
    // For 3 rows of content, wipe should finish at ~3/20 = 15% of progress.
    // With easeInOutCubic, raw progress ~0.27 → eased ~0.15. So ~54 ticks.
    // With 80 ticks we should have plenty of room.
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 20),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAA\nBBB\nCCC")
      to = SlideTestHarness.fixedSlide[IO]("XXX\nYYY\nZZZ")
      transition = createTransition(harness, 1000.millis)
      // 200 totalSteps for full screen, but content is 3/20 rows.
      // Should finish WAY before 200 ticks.
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 80)
      written <- harness.writtenFramesInOrder
    } yield {
      // The transition completed (didn't time out) — that's the main assertion.
      // With 3 rows out of 20, it should have finished in ~30-40 steps, not 200.
      assert(written.nonEmpty, "Expected frames to be written")
      assert(
        written.length < 100,
        s"Expected early exit — got ${written.length} frames (full duration would be ~200)"
      )
    }
  }

  test("full-screen content uses full duration") {
    // When content fills the entire screen, no early exit — uses full duration.
    val fullContent = (1 to 10).map(i => s"LINE_$i").mkString("\n")
    val fullContent2 = (1 to 10).map(i => s"XXXX_$i").mkString("\n")
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO](fullContent)
      to = SlideTestHarness.fixedSlide[IO](fullContent2)
      transition = createTransition(harness, 500.millis)
      // 500ms / 5ms = 100 totalSteps. Content fills all 10 rows = full screen.
      // Should need all ~100 steps.
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 110)
      written <- harness.writtenFramesInOrder
    } yield {
      // Should use close to the full 100 steps
      assert(
        written.length >= 90,
        s"Expected near-full duration for full-screen content — got ${written.length} frames"
      )
    }
  }
}

