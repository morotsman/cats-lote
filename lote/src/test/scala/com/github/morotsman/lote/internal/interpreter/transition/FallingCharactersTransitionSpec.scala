package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{AnimationSettings, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.internal.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class FallingCharactersTransitionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("FallingCharactersTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 4))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = FallingCharactersTransition.create[IO](
        gravity = 2.0,
        selectAccelerator = 100.0,
        console = console,
        ticker = ticker,
        animationSettings = animationSettings
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
      // Final write should be the "to" slide
      assert(
        written.head.contains("BBBB"),
        s"Expected final content 'BBBB', got: '${written.head}'"
      )
    }
  }

  test(
    "FallingCharactersTransition with identical slides completes immediately"
  ) {
    for {
      console <- TestNConsole.make(screen = Screen(4, 4))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = FallingCharactersTransition.create[IO](
        gravity = 2.0,
        selectAccelerator = 100.0,
        console = console,
        ticker = ticker,
        animationSettings = animationSettings
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
    }
  }

  test(
    "FallingCharactersTransition writes intermediate frames during animation"
  ) {
    for {
      console <- TestNConsole.make(screen = Screen(4, 4))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = FallingCharactersTransition.create[IO](
        gravity = 2.0,
        selectAccelerator = 100.0,
        console = console,
        ticker = ticker,
        animationSettings = animationSettings
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(
        written.length >= 2,
        s"Expected multiple writes, got ${written.length}"
      )
    }
  }

  test("FallingCharactersTransition clears screen during transition") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 4))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = FallingCharactersTransition.create[IO](
        gravity = 2.0,
        selectAccelerator = 100.0,
        console = console,
        ticker = ticker,
        animationSettings = animationSettings
      )
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }

  test("FallingCharactersTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 4))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      transition = FallingCharactersTransition.create[IO](
        gravity = 2.0,
        selectAccelerator = 100.0,
        console = console,
        ticker = ticker,
        animationSettings = animationSettings
      )
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test(
    "FallingCharactersTransition setupPosition creates falling and replacement chars"
  ) {
    def setupPosition(
        fromCharacter: Char,
        toCharacter: Char
    ): List[CharacterPosition] = {
      val _ = toCharacter
      List(
        CharacterPosition(
          fromCharacter,
          inTransition = false,
          canTransform = true
        ),
        CharacterPosition(' ', inTransition = false, canTransform = false)
      )
    }

    val positions = setupPosition('A', 'B')
    assertEquals(positions.length, 2)
    assertEquals(positions.head.character, 'A')
    assert(positions.head.canTransform)
    assertEquals(positions.last.character, ' ')
    assert(!positions.last.canTransform)
  }

  test("FallingCharactersTransition getNewIndex moves characters downward") {
    val screen = Screen(10, 5)
    val gravity = 1.2

    def getNewIndex(currentIndex: Int, cp: CharacterPosition): Option[Int] =
      if (cp.character == ' ') None
      else {
        val acceleration = cp.tick * gravity
        Some(currentIndex + (screen.screenWidth + 1) * acceleration.toInt)
      }

    // At tick 0, acceleration = 0, so character stays in place
    val cp0 = CharacterPosition('X', inTransition = true, canTransform = true)
    assertEquals(getNewIndex(5, cp0), Some(5))

    // At tick 1, acceleration = 1.2, toInt = 1, moves down by (10+1)*1 = 11
    val cp1 =
      CharacterPosition('X', inTransition = true, canTransform = true, tick = 1)
    assertEquals(getNewIndex(5, cp1), Some(16))

    // At tick 2, acceleration = 2.4, toInt = 2, moves down by (10+1)*2 = 22
    val cp2 =
      CharacterPosition('X', inTransition = true, canTransform = true, tick = 2)
    assertEquals(getNewIndex(5, cp2), Some(27))

    // Space characters return None
    val cpSpace =
      CharacterPosition(' ', inTransition = true, canTransform = true, tick = 1)
    assertEquals(getNewIndex(5, cpSpace), None)
  }
}
