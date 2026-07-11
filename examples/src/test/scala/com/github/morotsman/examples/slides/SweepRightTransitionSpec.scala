package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Slide, Ticker}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model.{Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class SweepRightTransitionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("SweepRightTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      implicit0(animationSettings: AnimationSettings) = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition[IO](columnsPerStep = 2)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
      assertEquals(written.head, "BBBB")
    }
  }

  test("SweepRightTransition renders intermediate sweep frames") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      implicit0(animationSettings: AnimationSettings) = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition[IO](columnsPerStep = 2)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      val intermediateFrames = written.drop(1)
      assert(
        intermediateFrames.contains("BBAA"),
        s"Expected an intermediate sweep frame 'BBAA', got: $intermediateFrames"
      )
    }
  }

  test("SweepRightTransition clears the screen while animating") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      implicit0(animationSettings: AnimationSettings) = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition[IO](columnsPerStep = 2)
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 2, s"Expected at least two clear() calls, got $cleared")
    }
  }

  test("SweepRightTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      implicit0(animationSettings: AnimationSettings) = AnimationSettings(5.millis)
      transition = SweepRightTransition[IO](columnsPerStep = 2)
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }
}


