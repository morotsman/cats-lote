package com.github.morotsman.lote.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.algebra.{IdleDetector, Overlay, Ticker, TickerSubscription}
import com.github.morotsman.lote.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter}
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class MiddlewareSpec extends CatsEffectSuite {

  private def noopIdleDetector: IdleDetector[IO] = IdleDetector.noop[IO]

  /** A no-op Ticker for tests that don't need real ticking */
  private def stubTicker: Ticker[IO] = new Ticker[IO] {
    override def subscribe(callback: IO[Unit]): IO[TickerSubscription[IO]] =
      IO.pure(new TickerSubscription[IO] { override def cancel: IO[Unit] = IO.unit })
    override def start: IO[Unit] = IO.unit
    override def stop: IO[Unit] = IO.unit
  }

  test("Middleware delegates alignText to underlying NConsole") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      result <- middleware.alignText("Hi", Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
    } yield {
      assert(result.content.contains("Hi"))
    }
  }

  test("Middleware delegates clear to underlying NConsole") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      _ <- middleware.clear()
      cleared <- console.clearedRef.get
    } yield {
      assertEquals(cleared, 1)
    }
  }

  test("Middleware delegates context to underlying NConsole") {
    for {
      console <- TestNConsole.make(screen = Screen(42, 13))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      ctx <- middleware.context
    } yield {
      assertEquals(ctx, Screen(42, 13))
    }
  }

  test("Middleware writeString with no overlays writes content unchanged") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      _ <- middleware.writeString(ScreenAdjusted("hello"))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.length, 1)
      assertEquals(written.head, "hello")
    }
  }

  test("Middleware writeString applies overlays") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      overlay = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + " [overlay]"))
      }
      _ <- middleware.addOverlays(List(overlay))
      _ <- middleware.writeString(ScreenAdjusted("content"))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.head, "content [overlay]")
    }
  }

  test("Middleware applies multiple overlays in order") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      overlay1 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[1]"))
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[2]"))
      }
      _ <- middleware.addOverlays(List(overlay1, overlay2))
      _ <- middleware.writeString(ScreenAdjusted("X"))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.head, "X[1][2]")
    }
  }

  test("Middleware read notifies IdleDetector of key press") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Character('a'))
      )
      // Wait to become idle
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      middleware <- Middleware.make[IO](console, stubTicker, detector)
      input <- middleware.read()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, Character('a'))
      assert(idleBefore, "Should have been idle before read")
      assert(!idleAfter, "Should not be idle after read (key press resets)")
    }
  }

  test("Middleware readInterruptible does not notify IdleDetector") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Esc))
      )
      _ <- IO.sleep(100.millis)
      middleware <- Middleware.make[IO](console, stubTicker, detector)
      input <- middleware.readInterruptible()
      idleAfter <- detector.isIdle
    } yield {
      assertEquals(input, Key(SpecialKey.Esc))
      assert(idleAfter, "readInterruptible should NOT reset idle state")
    }
  }

  test("Middleware addOverlays replaces existing overlays") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker, noopIdleDetector)
      overlay1 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("first"))
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("second"))
      }
      _ <- middleware.addOverlays(List(overlay1))
      _ <- middleware.writeString(ScreenAdjusted("x"))
      _ <- middleware.addOverlays(List(overlay2))
      _ <- middleware.writeString(ScreenAdjusted("y"))
      written <- console.writtenRef.get
    } yield {
      // written is in reverse order (newest first)
      assertEquals(written.head, "second")
      assertEquals(written(1), "first")
    }
  }
}

