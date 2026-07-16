package com.github.morotsman.examples.slides

import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api.{Character, Key, Screen, SpecialKey}
import com.github.morotsman.lote.api.support.Clock
import com.github.morotsman.lote.testkit.{SlideTestHarness, TestConsole}
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.util.Random

/** Tests for `ExampleInteractiveSlide` using the `SlideTestHarness`.
  *
  * Covers both the slide ↔ animator delegation (via a recording animator) and the full animator pipeline (via the real
  * `Animator` + `TestTicker`).
  */

private final case class RecordingState(
    animateCalls: Int = 0,
    stopCalls: Int = 0,
    directions: List[Direction] = Nil
)

class ExampleInteractiveSlideHarnessSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private def recordingAnimator(state: Ref[IO, RecordingState]): Animator[IO] =
    new Animator[IO] {
      override def animate(): IO[Unit] =
        state.update(s => s.copy(animateCalls = s.animateCalls + 1))

      override def stop(): IO[Unit] =
        state.update(s => s.copy(stopCalls = s.stopCalls + 1))

      override def changeDirection(input: Direction): IO[Unit] =
        state.update(s => s.copy(directions = s.directions :+ input))
    }

  // --- Slide ↔ Animator delegation tests (recording animator) ---

  test("content returns an empty centered screen") {
    for {
      console <- TestConsole.make[IO](screen = Screen(4, 3))
      state <- Ref[IO].of(RecordingState())
      slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
      content <- slide.content
    } yield assertEquals(content.content, "    \n    \n    ")
  }

  test("startShow delegates to animator.animate") {
    for {
      console <- TestConsole.make[IO]()
      state <- Ref[IO].of(RecordingState())
      slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
      _ <- slide.startShow
      recorded <- state.get
    } yield assertEquals(recorded.animateCalls, 1)
  }

  test("stopShow delegates to animator.stop") {
    for {
      console <- TestConsole.make[IO]()
      state <- Ref[IO].of(RecordingState())
      slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
      _ <- slide.stopShow
      recorded <- state.get
    } yield assertEquals(recorded.stopCalls, 1)
  }

  test("userInput maps WASD keys to directions") {
    for {
      console <- TestConsole.make[IO]()
      state <- Ref[IO].of(RecordingState())
      slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
      _ <- slide.userInput(Character('a'))
      _ <- slide.userInput(Character('w'))
      _ <- slide.userInput(Character('s'))
      _ <- slide.userInput(Character('d'))
      recorded <- state.get
    } yield assertEquals(
      recorded.directions,
      List(
        DirectionLeft(),
        DirectionUp(),
        DirectionDown(),
        DirectionRight()
      )
    )
  }

  test("userInput ignores non-WASD input") {
    for {
      console <- TestConsole.make[IO]()
      state <- Ref[IO].of(RecordingState())
      slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
      _ <- slide.userInput(Character('x'))
      _ <- slide.userInput(Key(SpecialKey.Enter))
      recorded <- state.get
    } yield assertEquals(recorded.directions, Nil)
  }

  // --- Full pipeline tests (real Animator + TestTicker) ---

  test("interactive slide writes frames after ticking") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 10),
        tickStep = 1.millis
      )
      animator <- {
        implicit val clock: Clock[IO] = harness.clockInstance
        Animator.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      slide = ExampleInteractiveSlide.fromAnimator[IO](animator, harness.console)
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
      animator <- {
        implicit val clock: Clock[IO] = harness.clockInstance
        Animator.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      slide = ExampleInteractiveSlide.fromAnimator[IO](animator, harness.console)
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
      animator <- {
        implicit val clock: Clock[IO] = harness.clockInstance
        Animator.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      slide = ExampleInteractiveSlide.fromAnimator[IO](animator, harness.console)
      _ <- slide.startShow
      countBefore <- harness.ticker.subscriberCount
      _ <- slide.stopShow
      countAfter <- harness.ticker.subscriberCount
    } yield {
      assert(countBefore > 0, "Expected at least one subscriber while running")
      assertEquals(countAfter, 0)
    }
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
    // Reverse direction causes the head to move back into the body.
    // On the next tick after the 180° turn, hasSelfCollision detects the overlap
    // and sets running=false. After that, no more frames are rendered.
    val width = 80
    val height = 80
    val step = 1.millis
    val screenSize = width * (height - 1)
    val initialHead = screenSize / 2
    // Block hearts around the worm's initial position and a few cells to the left
    val blocked = ((initialHead - 5) to (initialHead + 25)).toSet
    val seed = seedWithoutHearts(screenSize, blocked)

    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(width, height),
        tickStep = step
      )
      _ = Random.setSeed(seed)
      animator <- {
        implicit val clock: Clock[IO] = harness.clockInstance
        Animator.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      slide = ExampleInteractiveSlide.fromAnimator[IO](animator, harness.console)
      _ <- slide.startShow
      // Let the worm move left for a couple of ticks
      _ <- harness.tick(2)
      framesBefore <- harness.writtenFrames.map(_.length)
      // Reverse: press 'd' (right) while moving left — head collides with body
      _ <- slide.userInput(Character('d'))
      _ <- harness.tick(3)
      deathFrame <- harness.lastWrittenFrame
      frameCountAtDeath <- harness.writtenFrames.map(_.length)
      // Tick more — no new frames after death screen
      _ <- harness.tick(10)
      frameCountAfter <- harness.writtenFrames.map(_.length)
      _ <- slide.stopShow
    } yield {
      assert(framesBefore > 0, "Expected frames before reversal")
      // The last rendered frame should be the death screen
      assert(
        deathFrame.exists(_.contains("▓█████▄  ██▓▓█████")),
        s"Expected 'YOU DIED' art in the death frame, got: ${deathFrame.map(_.take(200))}"
      )
      // No further frames after the death screen
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
      animator <- {
        implicit val clock: Clock[IO] = harness.clockInstance
        Animator.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      slide = ExampleInteractiveSlide.fromAnimator[IO](animator, harness.console)
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
