package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{AnimationSettings, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.internal.interpreter.ticker.TickerInterpreter
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class GrabTransitionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 15.seconds

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  // Create content that fits a small screen with enough rows for the snake (9 rows)
  private def smallScreenContent(width: Int): String = {
    val line = "Hello World" + " " * (width - 11)
    (0 until 12)
      .map { i =>
        if (i == 0) line.take(width)
        else " " * width
      }
      .mkString("\n")
  }

  test("GrabTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      fromContent = smallScreenContent(40)
      toContent = "Goodbye" + " " * 33 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, console, ticker, animationSettings)
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
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, console, ticker, animationSettings)
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
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, console, ticker, animationSettings)
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 2, s"Expected at least 2 clears, got $cleared")
    }
  }

  test("GrabTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 12))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      transition = GrabTransition.create[IO](stepSize = 10, console, ticker, animationSettings)
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test("GrabTransition snake art appears in intermediate frames") {
    for {
      console <- TestNConsole.make(screen = Screen(60, 12))
      ticker <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings = AnimationSettings(5.millis)
      fromContent = smallScreenContent(60)
      toContent = " " * 60 + ("\n" + " " * 60) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 5, console, ticker, animationSettings)
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
      ticker1 <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings1 = AnimationSettings.default
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from1 = fixedSlide(fromContent)
      to1 = fixedSlide(toContent)
      transition1 = GrabTransition.create[IO](stepSize = 20, console1, ticker1, animationSettings1)
      _ <- transition1.transition(from1, to1)
      written1 <- console1.writtenRef.get

      console2 <- TestNConsole.make(screen = Screen(40, 12))
      ticker2 <- TickerInterpreter.make[IO](interval = 5.millis)
      animationSettings2 = AnimationSettings.default
      from2 = fixedSlide(fromContent)
      to2 = fixedSlide(toContent)
      transition2 = GrabTransition.create[IO](stepSize = 2, console2, ticker2, animationSettings2)
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
