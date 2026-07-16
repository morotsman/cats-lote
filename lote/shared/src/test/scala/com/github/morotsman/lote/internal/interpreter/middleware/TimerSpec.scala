package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.IO
import cats.effect.testkit.TestControl
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted}
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TimerSpec extends CatsEffectSuite {

  private def withTimeControl[A](io: IO[A]): IO[A] =
    TestControl.executeEmbed(io)

  test("Timer renders time remaining at start of content") {
    withTimeControl {
      for {
        now <- IO.realTime.map(_.toMillis)
        timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = now)
        content = ScreenAdjusted("X" * 40)
        result <- timer.applyOverlay(Screen(40, 10), content, content)
      } yield {
        // Timer should render something like "9:59" or "10:00" at the start
        assert(
          result.content.startsWith("9:") || result.content.startsWith("10:"),
          s"Expected time prefix, got: '${result.content.take(10)}'"
        )
      }
    }
  }

  test("Timer shows decreasing time after some time passes") {
    withTimeControl {
      for {
        now <- IO.realTime.map(_.toMillis)
        timer <- Timer.make[IO](allocatedTime = 10.minutes, startTime = now)
        _ <- IO.sleep(5.minutes)
        content = ScreenAdjusted("X" * 40)
        result <- timer.applyOverlay(Screen(40, 10), content, content)
      } yield {
        // Should show approximately 5 minutes or less
        assert(
          result.content.startsWith("4:") || result.content.startsWith("5:"),
          s"Expected ~5 min remaining, got: '${result.content.take(10)}'"
        )
      }
    }
  }

  test("Timer replaces characters at the beginning of content") {
    withTimeControl {
      for {
        now <- IO.realTime.map(_.toMillis)
        timer <- Timer.make[IO](allocatedTime = 30.minutes, startTime = now)
        content = ScreenAdjusted("ABCDEFGHIJKLMNOP")
        result <- timer.applyOverlay(Screen(40, 10), content, content)
      } yield {
        // The content after the timer prefix should contain the tail of original
        assert(
          result.content.contains("KLMNOP") || result.content.contains("JKLMNOP"),
          s"Expected original content tail preserved, got: '${result.content}'"
        )
        // Total length should remain the same
        assertEquals(result.content.length, content.content.length)
      }
    }
  }
}
