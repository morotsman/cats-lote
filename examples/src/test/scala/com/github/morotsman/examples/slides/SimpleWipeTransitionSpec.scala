package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

/** Demonstrates testing a `TickedTransition`-based wipe transition.
  *
  * Same pattern as `SimpleSweepTransitionSpec`: use `TickedTransition.forTest(harness)`
  * and `harness.clockInstance` to wire the transition for testing.
  */
class SimpleWipeTransitionSpec extends CatsEffectSuite {

  private def createTransition(harness: SlideTestHarness[IO], duration: FiniteDuration = 400.millis) = {
    implicit val clock: AnimationClock[IO] = harness.clockInstance
    SimpleWipeTransition.create[IO](duration, TickedTransition.forTest(harness))
  }

  test("wipe completes and shows target slide content") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(10, 3),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA\nAAAAAAAAAA\nAAAAAAAAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB\nBBBBBBBBBB\nBBBBBBBBBB")
      transition = createTransition(harness)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 120)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty, "Expected at least one written frame")
      assertEquals(written.head, "BBBBBBBBBB\nBBBBBBBBBB\nBBBBBBBBBB")
    }
  }

  test("wipe renders intermediate frames with partial row reveal") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(10, 3),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA\nAAAAAAAAAA\nAAAAAAAAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB\nBBBBBBBBBB\nBBBBBBBBBB")
      transition = createTransition(harness)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 120)
      written <- harness.writtenFramesInOrder
    } yield {
      // Should have intermediate frames where some rows are B and others still A
      val intermediates = written.filter { f =>
        val lines = f.split("\n")
        lines.exists(_.contains('A')) && lines.exists(_.contains('B'))
      }
      assert(intermediates.nonEmpty, s"Expected intermediate frames with partial row reveal, got: $written")
    }
  }
}
