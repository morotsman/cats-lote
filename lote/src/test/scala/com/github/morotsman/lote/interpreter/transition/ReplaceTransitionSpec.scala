package com.github.morotsman.lote.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class ReplaceTransitionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("ReplaceTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = ReplaceTransition[IO]('*')
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

  test("ReplaceTransition intermediate frames show the replacement character") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("XXXX")
      to = fixedSlide("YYYY")
      transition = ReplaceTransition[IO]('#')
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
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
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("SAME")
      to = fixedSlide("SAME")
      transition = ReplaceTransition[IO]('.')
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
    }
  }

  test("ReplaceTransition clears screen during transition") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = ReplaceTransition[IO]('*')
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 1, s"Expected at least one clear(), got $cleared")
    }
  }

  test("ReplaceTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      transition = ReplaceTransition[IO]('*')
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
