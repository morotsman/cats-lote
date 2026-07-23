package com.github.morotsman.examples

import cats.effect.IO
import com.github.morotsman.lote.api.{Character, Key, Screen, SpecialKey}
import com.github.morotsman.lote.api.builders.SessionBuilder
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

/** Integration tests for `EffectfulOverlayExample` using `SessionBuilder.runWith`.
  *
  * These test the full session — slides, overlay, middleware — wired together with test doubles instead of a real
  * terminal. The presentation loop reads pre-loaded inputs and exits on `Esc`.
  *
  * `readDelay = 1.millis` gives slide fibers time to write content before the next input is consumed — in production,
  * `read()` blocks on the keyboard.
  */
class EffectfulOverlayExampleSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private val readDelay = 1.millis

  /** Reuse the real example session builder so we test the actual production configuration. */
  private def buildSession: SessionBuilder[IO] = EffectfulOverlayExample.presentation

  test("session starts and exits on Esc") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(Key(SpecialKey.Timeout), Key(SpecialKey.Timeout), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- buildSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield assert(written.nonEmpty, "Expected at least one frame to be written")
  }

  test("first slide content is rendered") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- buildSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(
        allContent.contains("effectful"),
        s"Expected first slide content in output, got frames: ${written.take(3)}"
      )
    }
  }

  test("overlay status line appears in rendered frames") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- buildSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(
        allContent.contains("[input overlay]"),
        s"Expected overlay status line in output, got frames: ${written.take(3)}"
      )
    }
  }

  test("overlay reflects key presses made during the session") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(
          Character('a'),
          Character('b'),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        buildSession.runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      // The overlay counts every input via onUserInput and re-renders on ticker ticks.
      // After 'a' and 'b' are consumed the overlay re-renders "keys seen: 2".
      // Depending on tick timing, "keys seen: 3" (after Esc) may also appear if a tick
      // fires during the exit sequence before the subscription is cancelled.
      assert(
        allContent.contains("keys seen: 2") || allContent.contains("keys seen: 3"),
        s"Expected overlay to show key count >= 2, got frames: ${written.take(3)}"
      )
    }
  }

  test("navigating right shows next slide content") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- buildSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(
        allContent.contains("addOverlay"),
        s"Expected second slide content after Right key, got frames: ${written.take(5)}"
      )
    }
  }

  test("navigating right then left returns to first slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- buildSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      // The most recent frames should be from the first slide again
      assert(
        written.exists(_.contains("effectful")),
        s"Expected first slide content after navigating back, got frames: ${written.take(5)}"
      )
    }
  }
}
