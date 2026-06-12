package com.github.morotsman.lote.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration

class CharactersTransitionSpec extends CatsEffectSuite {

  // Override default timeout for transition tests
  override val munitTimeout: Duration = 10.seconds

  /** Creates a slide that returns fixed ScreenAdjusted content (no alignment/padding) */
  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  // setupPosition: if chars differ, create two entries - one transformable (to be removed) and one final
  private def simpleSetupPosition(from: Char, to: Char): List[CharacterPosition] = {
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
  private def inPlaceNewIndex(screen: Screen, index: Int, cp: CharacterPosition): Option[Int] = None

  test("CharactersTransition completes and shows the target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = CharactersTransition[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty, "Expected content to be written")
      // Last write is the final "to" slide
      assert(written.head.contains("BBBB"), s"Expected final write to contain 'BBBB', got: '${written.head}'")
    }
  }

  test("CharactersTransition writes intermediate frames") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = CharactersTransition[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      // initial write + at least one tick write + final write
      assert(written.length >= 2, s"Expected multiple writes, got ${written.length}")
    }
  }

  test("CharactersTransition with identical slides completes immediately") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = CharactersTransition[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex
      )
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("CharactersTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      transition = CharactersTransition[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex
      )
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  private def runTransition(accelerator: Double): IO[FiniteDuration] = for {
    console <- TestNConsole.make(screen = Screen(4, 1))
    nc = console: NConsole[IO]
    ticker <- TickerInterpreter.make[IO](interval = 5.millis)(implicitly, implicitly, implicitly, implicitly)
    from = fixedSlide("ABCD")
    to = fixedSlide("WXYZ")
    transition = CharactersTransition[IO](
      selectAccelerator = accelerator,
      setupPosition = simpleSetupPosition,
      getNewIndex = inPlaceNewIndex
    )(implicitly, implicitly, nc, ticker)
    start <- IO.monotonic
    _ <- transition.transition(from, to)
    end <- IO.monotonic
  } yield end - start

  test("CharactersTransition higher selectAccelerator completes faster") {
    for {
      fastDuration <- runTransition(100.0)
      slowDuration <- runTransition(1.1)
    } yield {
      assert(fastDuration <= slowDuration, s"Expected fast ($fastDuration) <= slow ($slowDuration)")
    }
  }

  test("CharactersTransition clears screen during transition") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = CharactersTransition[IO](
        selectAccelerator = 100.0,
        setupPosition = simpleSetupPosition,
        getNewIndex = inPlaceNewIndex
      )
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }
}


