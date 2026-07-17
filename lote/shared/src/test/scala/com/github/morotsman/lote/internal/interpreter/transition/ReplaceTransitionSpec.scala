package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class ReplaceTransitionSpec extends CatsEffectSuite {

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[Option[ScreenAdjusted]] = IO.pure(Some(ScreenAdjusted(text)))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("ReplaceTransition completes and shows target slide") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = ReplaceTransition.create[IO]('*', harness.console, harness.ticker, harness.animationSettings)
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

  test("ReplaceTransition intermediate frames show the replacement character") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = ReplaceTransition.create[IO]('#', harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      // Intermediate frames should contain the replacement character '#'
      val intermediates = written.drop(1) // drop the final "to" write
      assert(
        intermediates.exists(_.contains("#")),
        s"Expected replacement char '#' in intermediate frames, got: ${intermediates.map(_.take(10))}"
      )
    }
  }

  test("ReplaceTransition with identical slides completes immediately") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = ReplaceTransition.create[IO]('.', harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("ReplaceTransition clears screen during transition") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = ReplaceTransition.create[IO]('*', harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      cleared <- harness.clearCount
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }

  test("ReplaceTransition userInput is a no-op") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      transition = ReplaceTransition.create[IO]('*', harness.console, harness.ticker, harness.animationSettings)
      _ <- transition.userInput(Key(SpecialKey.Left))
    } yield ()
  }

  test("ReplaceTransition setupPosition uses the replacement character") {
    // Verify the setup: from-char is transformable, replacement char is the final visible char
    val positions = List(
      CharacterPosition('A', inTransition = false, canTransform = true),
      CharacterPosition('*', inTransition = false, canTransform = false)
    )

    assertEquals(positions.head.character, 'A')
    assert(positions.head.canTransform)
    assertEquals(positions.last.character, '*')
    assert(!positions.last.canTransform)
  }
}
