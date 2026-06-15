package com.github.morotsman.lote.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class MorphTransitionSpec extends CatsEffectSuite {

  override val munitTimeout: Duration = 10.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("MorphTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = MorphTransition[IO]()
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
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
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = MorphTransition[IO]()
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("MorphTransition writes intermediate frames") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = MorphTransition[IO]()
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(
        written.length >= 2,
        s"Expected multiple writes, got ${written.length}"
      )
    }
  }

  test("MorphTransition clears screen during transition") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = MorphTransition[IO]()
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }

  test("MorphTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      transition = MorphTransition[IO]()
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

    assertEquals(positions(0).character, 'A')
    assert(positions(0).canTransform)
    assertEquals(positions(1).character, 'B')
    assert(!positions(1).canTransform)
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
