package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.IO
import com.github.morotsman.lote.api.{Character, Key, MouseClick, MouseMove, Screen, ScreenAdjusted, SpecialKey}
import com.github.morotsman.lote.internal.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter}
import com.github.morotsman.lote.internal.interpreter.nconsole.IdleAwareNConsole
import com.github.morotsman.lote.testkit.TestConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class IdleAwareNConsoleSpec extends CatsEffectSuite {

  test("read notifies IdleDetector of key press") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](
        screen = Screen(20, 5),
        inputs = List(Character('a'))
      )
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      input <- console.read()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, Character('a'))
      assert(idleBefore, "Should have been idle before read")
      assert(!idleAfter, "Should not be idle after read (key press resets)")
    }
  }

  test("read notifies IdleDetector of mouse click") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](
        screen = Screen(20, 5),
        inputs = List(MouseClick(10, 20))
      )
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      input <- console.read()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, MouseClick(10, 20))
      assert(idleBefore, "Should have been idle before read")
      assert(!idleAfter, "Should not be idle after mouse click")
    }
  }

  test("read notifies IdleDetector of mouse move") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](
        screen = Screen(20, 5),
        inputs = List(MouseMove(5, 10))
      )
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      input <- console.read()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, MouseMove(5, 10))
      assert(idleBefore, "Should have been idle before read")
      assert(!idleAfter, "Should not be idle after mouse move")
    }
  }

  test("readInterruptible does not notify IdleDetector") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Esc))
      )
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      _ <- IO.sleep(100.millis)
      input <- console.readInterruptible()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, Key(SpecialKey.Esc))
      assert(idleAfter, "readInterruptible should NOT reset idle state")
    }
  }

  test("writeString delegates to underlying without notifying IdleDetector") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](screen = Screen(20, 5))
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      _ <- console.writeString(ScreenAdjusted("initial content"))
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      _ <- console.writeString(ScreenAdjusted("new content"))
      idleAfter <- detector.isIdle
    } yield {
      assert(idleBefore, "Should have been idle before write")
      assert(
        idleAfter,
        "Should still be idle - writeString does not reset idle at NConsole level"
      )
    }
  }

  test("delegates context to underlying NConsole") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = 50.millis)
      )
      rawConsole <- TestConsole.make[IO](screen = Screen(42, 13))
      console = IdleAwareNConsole.wrap[IO](rawConsole, detector)
      ctx <- console.context
    } yield {
      assertEquals(ctx, Screen(42, 13))
    }
  }
}
