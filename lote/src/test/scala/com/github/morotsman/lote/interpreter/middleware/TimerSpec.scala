package com.github.morotsman.lote.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TimerSpec extends CatsEffectSuite {

  test("Timer renders time remaining at start of content") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      now <- IO.realTime.map(_.toMillis)
      timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = now)
      content = ScreenAdjusted("X" * 40)
      result <- timer.applyOverlay(Screen(40, 10), content)
    } yield {
      // Timer should render something like "9:59" or "10:00" at the start
      assert(result.content.startsWith("9:") || result.content.startsWith("10:"),
        s"Expected time prefix, got: '${result.content.take(10)}'")
    }
  }

  test("Timer shows decreasing time after some time passes") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      // Simulate starting 5 minutes ago
      now <- IO.realTime.map(_.toMillis)
      startTime = now - 5.minutes.toMillis
      timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = startTime)
      content = ScreenAdjusted("X" * 40)
      result <- timer.applyOverlay(Screen(40, 10), content)
    } yield {
      // Should show approximately 5 minutes or less
      assert(result.content.startsWith("4:") || result.content.startsWith("5:"),
        s"Expected ~5 min remaining, got: '${result.content.take(10)}'")
    }
  }

  test("Timer replaces characters at the beginning of content") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      now <- IO.realTime.map(_.toMillis)
      timer <- Timer.make[IO](allocatedTime = 30.minutes, startTime = now)
      content = ScreenAdjusted("ABCDEFGHIJKLMNOP")
      result <- timer.applyOverlay(Screen(40, 10), content)
    } yield {
      // The content after the timer prefix should contain the tail of original
      assert(result.content.contains("KLMNOP") || result.content.contains("JKLMNOP"),
        s"Expected original content tail preserved, got: '${result.content}'")
      // Total length should remain the same
      assertEquals(result.content.length, content.content.length)
    }
  }

  test("Timer onKeyPress is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      now <- IO.realTime.map(_.toMillis)
      timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = now)
      _ <- timer.onKeyPress(Key(SpecialKey.Right))
    } yield ()
  }

  test("Timer onContentChange is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      now <- IO.realTime.map(_.toMillis)
      timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = now)
      _ <- timer.onContentChange("new content")
    } yield ()
  }
}

