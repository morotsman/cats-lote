package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class CharactersTransitionSpec extends CatsEffectSuite {

  /** Creates a slide that returns fixed ScreenAdjusted content (no alignment/padding)
    */
  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  // setupPosition: if chars differ, create two entries - one transformable (to be removed) and one final
  private def simpleSetupPosition(
      from: Char,
      to: Char
  ): List[CharacterPosition] = {
    if (from == to) {
      List(CharacterPosition(from, inTransition = false, canTransform = false))
    } else {
      List(
        CharacterPosition(from, inTransition = false, canTransform = true),
        CharacterPosition(to, inTransition = false, canTransform = false)
      )
    }
  }

  // getNewIndex: returns None to remove the transformable character (revealing the final one underneath)
  private def inPlaceNewIndex(
      _screen: Screen,
      _index: Int,
      _cp: CharacterPosition
  ): Option[Int] = {
    val _ = (_screen, _index, _cp)
    None
  }

  test("CharactersTransition completes and shows the target slide") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harness.console,
        ticker = harness.ticker,
        animationSettings = harness.animationSettings
      )
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty, "Expected content to be written")
      // Last write is the final "to" slide
      assert(
        written.head.contains("BBBB"),
        s"Expected final write to contain 'BBBB', got: '${written.head}'"
      )
    }
  }

  test("CharactersTransition writes intermediate frames") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harness.console,
        ticker = harness.ticker,
        animationSettings = harness.animationSettings
      )
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      // initial write + at least one tick write + final write
      assert(
        written.length >= 2,
        s"Expected multiple writes, got ${written.length}"
      )
    }
  }

  test("CharactersTransition with identical slides completes immediately") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harness.console,
        ticker = harness.ticker,
        animationSettings = harness.animationSettings
      )
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("CharactersTransition userInput is a no-op") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      transition = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harness.console,
        ticker = harness.ticker,
        animationSettings = harness.animationSettings
      )
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test("CharactersTransition higher selectAccelerator completes with fewer ticks") {
    for {
      harnessFast <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("ABCD")
      to = fixedSlide("WXYZ")
      transitionFast = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harnessFast.console,
        ticker = harnessFast.ticker,
        animationSettings = harnessFast.animationSettings
      )
      _ <- harnessFast.runWithTicking(transitionFast.transition(from, to), ticks = 50)
      writtenFast <- harnessFast.writtenFrames

      harnessSlow <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      transitionSlow = CharactersTransition.create[IO](
        selectAccelerator = 1.1,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harnessSlow.console,
        ticker = harnessSlow.ticker,
        animationSettings = harnessSlow.animationSettings
      )
      _ <- harnessSlow.runWithTicking(transitionSlow.transition(from, to), ticks = 50)
      writtenSlow <- harnessSlow.writtenFrames
    } yield {
      assert(
        writtenFast.length <= writtenSlow.length,
        s"Expected fast (${writtenFast.length} frames) <= slow (${writtenSlow.length} frames)"
      )
    }
  }

  test("CharactersTransition clears screen during transition") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(4, 1), tickStep = 5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = CharactersTransition.create[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex,
        console = harness.console,
        ticker = harness.ticker,
        animationSettings = harness.animationSettings
      )
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 50)
      cleared <- harness.clearCount
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }
}
