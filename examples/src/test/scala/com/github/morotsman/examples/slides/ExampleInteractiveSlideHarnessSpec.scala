package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.{Character, Key, Screen, SpecialKey}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.util.Random

/** Tests for `ExampleInteractiveSlide` using the `SlideTestHarness`.
  *
  * Covers the full slide pipeline (TickedSlide + game logic) via `ExampleInteractiveSlide.create`.
  */

class ExampleInteractiveSlideHarnessSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  // --- Slide lifecycle tests ---

  test("content returns None for animated slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      content <- slide.content
    } yield assertEquals(content, None)
  }

  // --- Full pipeline tests (TickedSlide + game logic) ---

  test("interactive slide writes frames after ticking") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      _ <- harness.tick(3)
      frames <- harness.writtenFrames
      _ <- slide.stopShow
    } yield {
      assert(frames.nonEmpty, "Expected at least one rendered frame after ticking")
    }
  }

  test("interactive slide responds to direction input") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      _ <- harness.tick(2)
      _ <- harness.reset
      _ <- slide.userInput(Character('d'))
      _ <- harness.tick(5)
      framesAfter <- harness.writtenFrames
      _ <- slide.stopShow
    } yield {
      assert(
        framesAfter.nonEmpty,
        "Expected at least one frame after direction change and ticking"
      )
    }
  }

  test("stopShow cancels the ticker subscription") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      countBefore <- harness.ticker.subscriberCount
      _ <- slide.stopShow
      countAfter <- harness.ticker.subscriberCount
    } yield {
      assert(countBefore > 0, "Expected at least one subscriber while running")
      assertEquals(countAfter, 0)
    }
  }

  test("userInput ignores non-WASD input") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      _ <- slide.userInput(Character('x'))
      _ <- slide.userInput(Key(SpecialKey.Enter))
      // No crash = success; non-WASD keys are silently ignored
      _ <- slide.stopShow
    } yield ()
  }

  // --- Worm wrap integration test ---

  private def positionsOf(frame: String): List[(Int, Int)] =
    frame
      .split("\n", -1)
      .toList
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.zipWithIndex.collect { case ('W', col) =>
          (row, col)
        }
      }

  private def heartIndexesForSeed(seed: Long, screenSize: Int): Set[Int] = {
    val random = new Random(seed)
    List.fill(500)(random.nextInt(screenSize)).toSet
  }

  private def seedWithoutHearts(screenSize: Int, blocked: Set[Int]): Long =
    Iterator
      .from(0)
      .map(_.toLong)
      .find(seed => heartIndexesForSeed(seed, screenSize).intersect(blocked).isEmpty)
      .getOrElse(throw new AssertionError("Could not find deterministic seed without hearts on the wrap path"))

  test("the worm dies when it collides with itself") {
    val width = 80
    val height = 80
    val step = 1.millis
    val screenSize = width * (height - 1)
    val initialHead = screenSize / 2
    val blocked = ((initialHead - 5) to (initialHead + 25)).toSet
    val seed = seedWithoutHearts(screenSize, blocked)

    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(width, height),
        tickStep = step
      )
      _ = Random.setSeed(seed)
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      _ <- harness.tick(2)
      framesBefore <- harness.writtenFrames.map(_.length)
      _ <- slide.userInput(Character('d'))
      _ <- harness.tick(3)
      deathFrame <- harness.lastWrittenFrame
      frameCountAtDeath <- harness.writtenFrames.map(_.length)
      _ <- harness.tick(10)
      frameCountAfter <- harness.writtenFrames.map(_.length)
      _ <- slide.stopShow
    } yield {
      assert(framesBefore > 0, "Expected frames before reversal")
      assert(
        deathFrame.exists(_.contains("▓█████▄  ██▓▓█████")),
        s"Expected 'YOU DIED' art in the death frame, got: ${deathFrame.map(_.take(200))}"
      )
      assertEquals(
        frameCountAfter,
        frameCountAtDeath,
        s"Expected no new frames after death screen"
      )
    }
  }

  test("the worm wraps from the left edge to the same row") {
    val width = 80
    val height = 300
    val step = 1.millis
    val screenSize = width * (height - 1)
    val initialHeadIndex = screenSize / 2
    val initialRow = initialHeadIndex / width
    val rowStart = initialHeadIndex - (initialHeadIndex % width)
    val rowEnd = rowStart + width - 1
    val wrapPath = (rowStart to rowEnd).toSet
    val seed = seedWithoutHearts(screenSize, wrapPath)

    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(width, height),
        tickStep = step
      )
      _ = Random.setSeed(seed)
      slide <- {
        import harness.clockInstance
        ExampleInteractiveSlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _ <- slide.startShow
      _ <- harness.tick(120)
      written <- harness.writtenFramesInOrder
      _ <- slide.stopShow
    } yield {
      val positions = written.map(positionsOf)
      val leftEdgeIndex = positions.indexWhere {
        case List((row, col)) => row == initialRow && col <= 1
        case _                => false
      }
      assert(
        leftEdgeIndex >= 0,
        s"Expected to observe 'W' at or immediately next to the left edge of row $initialRow, got: ${positions.take(80)}"
      )
      val framesAfterLeftEdge = positions.slice(leftEdgeIndex + 1, leftEdgeIndex + 11)
      assert(
        framesAfterLeftEdge.exists {
          case List((row, col)) => row == initialRow && col >= (width * 3) / 4
          case _                => false
        },
        s"Expected a frame after the left edge to keep 'W' on row $initialRow and place it back near the right edge, got: ${positions.slice(leftEdgeIndex, leftEdgeIndex + 10)}"
      )
    }
  }
}
