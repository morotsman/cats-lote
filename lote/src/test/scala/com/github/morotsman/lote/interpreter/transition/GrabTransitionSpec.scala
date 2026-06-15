package com.github.morotsman.lote.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker}
import com.github.morotsman.lote.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class GrabTransitionSpec extends CatsEffectSuite {

  override val munitTimeout: Duration = 15.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  // Create content that fits a small screen with enough rows for the snake (9 rows)
  private def smallScreenContent(width: Int, height: Int): String = {
    val line = "Hello World" + " " * (width - 11)
    (0 until height)
      .map { i =>
        if (i == 0) line.take(width)
        else " " * width
      }
      .mkString("\n")
  }

  test("GrabTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      fromContent = smallScreenContent(40, 12)
      toContent = "Goodbye" + " " * 33 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition[IO](stepSize = 10)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
      assert(
        written.head.contains("Goodbye"),
        s"Expected final content to contain 'Goodbye', got: '${written.head.take(40)}'"
      )
    }
  }

  test("GrabTransition writes multiple frames during animation") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      fromContent = smallScreenContent(40, 12)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition[IO](stepSize = 10)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      // Should have many frames: crawl in + bite + drag out + final
      assert(
        written.length >= 3,
        s"Expected at least 3 writes, got ${written.length}"
      )
    }
  }

  test("GrabTransition clears screen during transition") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      fromContent = smallScreenContent(40, 12)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition[IO](stepSize = 10)
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 2, s"Expected at least 2 clears, got $cleared")
    }
  }

  test("GrabTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      transition = GrabTransition[IO](stepSize = 10)
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test("GrabTransition snake art appears in intermediate frames") {
    for {
      console <- TestNConsole.make(screen = Screen(60, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      implicit0(tk: Ticker[IO]) = ticker
      fromContent = smallScreenContent(60, 12)
      toContent = " " * 60 + ("\n" + " " * 60) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition[IO](stepSize = 5)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      // Snake art contains characters like /^\/^\ and _|__|
      val allFrames = written.mkString("")
      assert(
        allFrames.contains("/^"),
        s"Expected snake art characters in frames"
      )
    }
  }

  test("GrabTransition with larger stepSize completes with fewer frames") {
    for {
      console1 <- TestNConsole.make(screen = Screen(40, 12))
      nc1 = console1: NConsole[IO]
      ticker1 <- TickerInterpreter.make[IO](interval = 5.millis)(
        implicitly,
        implicitly,
        implicitly,
        implicitly
      )
      fromContent = smallScreenContent(40, 12)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from1 = fixedSlide(fromContent)
      to1 = fixedSlide(toContent)
      transition1 = GrabTransition[IO](stepSize = 20)(
        implicitly,
        implicitly,
        nc1,
        ticker1
      )
      _ <- transition1.transition(from1, to1)
      written1 <- console1.writtenRef.get

      console2 <- TestNConsole.make(screen = Screen(40, 12))
      nc2 = console2: NConsole[IO]
      ticker2 <- TickerInterpreter.make[IO](interval = 5.millis)(
        implicitly,
        implicitly,
        implicitly,
        implicitly
      )
      from2 = fixedSlide(fromContent)
      to2 = fixedSlide(toContent)
      transition2 = GrabTransition[IO](stepSize = 2)(
        implicitly,
        implicitly,
        nc2,
        ticker2
      )
      _ <- transition2.transition(from2, to2)
      written2 <- console2.writtenRef.get
    } yield {
      assert(
        written1.length <= written2.length,
        s"Expected larger step (${written1.length} frames) <= smaller step (${written2.length} frames)"
      )
    }
  }
}
