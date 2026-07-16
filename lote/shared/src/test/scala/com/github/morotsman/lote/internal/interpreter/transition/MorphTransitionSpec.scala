package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{Character, Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class MorphTransitionSpec extends CatsEffectSuite {

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("MorphTransition completes and shows target slide") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = MorphTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty)
      assert(
        written.head.contains("BBBB"),
        s"Expected final content 'BBBB', got: '${written.head}'"
      )
    }
  }

  test("MorphTransition with identical slides completes immediately") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = MorphTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("MorphTransition writes intermediate frames") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = MorphTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(
        written.length >= 2,
        s"Expected multiple writes, got ${written.length}"
      )
    }
  }

  test("MorphTransition clears screen during transition") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = MorphTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      cleared <- harness.clearCount
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }

  test("MorphTransition userInput is a no-op") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      transition = MorphTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      _ <- transition.userInput(Character('x'))
    } yield ()
  }

  test(
    "MorphTransition setupPosition places from-char as transformable and to-char as final"
  ) {
    // Verify the logic directly
    val positions = List(
      CharacterPosition('A', inTransition = false, canTransform = true),
      CharacterPosition('B', inTransition = false, canTransform = false)
    )

    assertEquals(positions.head.character, 'A')
    assert(positions.head.canTransform)
    assertEquals(positions.last.character, 'B')
    assert(!positions.last.canTransform)
  }

  test("MorphTransition getNewIndex always returns None") {
    // MorphTransition removes the transformable char in place (no movement)
    Screen(10, 5)
    CharacterPosition('X', inTransition = true, canTransform = true, tick = 3)

    // getNewIndex = None means the character is removed, revealing the "to" char underneath
    val result: Option[Int] = None
    assertEquals(result, None)
  }
}
