package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.support.{AnimationClock, TickedTransition}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

/** Demonstrates testing a `TickedTransition.contextual`-based transition.
  *
  * `SimpleSweepTransition` uses `TickedTransition.contextual[F]` to receive
  * console, ticker, and animation settings from the framework. In tests, we
  * use `TickedTransition.forTest(harness)` to get a pre-wired builder and
  * thread `harness.clockInstance` as the implicit `AnimationClock`.
  *
  * This pattern works for any transition built with `TickedTransition`.
  */
class SimpleSweepTransitionSpec extends CatsEffectSuite {

  /** Helper: creates a SimpleSweepTransition using the harness.
    *
    * The key steps are:
    *  1. Get the implicit `AnimationClock` from `harness.clockInstance`
    *  2. Get a pre-wired `TickedTransition.Builder` via `TickedTransition.forTest(harness)`
    *  3. Pass the builder to the transition's `create` method
    */
  private def createTransition(harness: SlideTestHarness[IO], duration: FiniteDuration = 500.millis) = {
    implicit val clock: AnimationClock[IO] = harness.clockInstance
    SimpleSweepTransition.create[IO](duration, TickedTransition.forTest(harness))
  }

  test("sweep completes and shows target slide content") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(10, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB")
      transition = createTransition(harness)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 150)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty, "Expected at least one written frame")
      assertEquals(written.head, "BBBBBBBBBB")
    }
  }

  test("sweep renders intermediate frames during animation") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(10, 1),
        tickStep = 5.millis
      )
      from = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA")
      to = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB")
      transition = createTransition(harness)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 150)
      written <- harness.writtenFramesInOrder
    } yield {
      // Should have intermediate frames where some columns are B and others still A
      val intermediates = written.filter(f => f.contains('A') && f.contains('B'))
      assert(intermediates.nonEmpty, s"Expected intermediate frames, got: $written")
    }
  }
}
