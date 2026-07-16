package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Screen, ScreenAdjusted, VerticalAlignment}
import com.github.morotsman.lote.api.spi.{Overlay, Ticker, TickerSubscription}
import com.github.morotsman.lote.internal.interpreter.middleware.Middleware
import com.github.morotsman.lote.testkit.TestConsole
import munit.CatsEffectSuite

class MiddlewareSpec extends CatsEffectSuite {

  /** A no-op Ticker for tests that don't need real ticking */
  private def stubTicker: Ticker[IO] = new Ticker[IO] {
    override def subscribe(callback: IO[Unit]): IO[TickerSubscription[IO]] =
      IO.pure(new TickerSubscription[IO] {
        override def cancel: IO[Unit] = IO.unit
      })
    override def start: IO[Unit] = IO.unit
    override def stop: IO[Unit] = IO.unit
  }

  test("Middleware delegates alignText to underlying NConsole") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      result <- middleware.alignText(
        "Hi",
        Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
      )
    } yield {
      assert(result.content.contains("Hi"))
    }
  }

  test("Middleware delegates clear to underlying NConsole") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      _ <- middleware.clear()
      cleared <- console.clearCount
    } yield {
      assertEquals(cleared, 1)
    }
  }

  test("Middleware delegates context to underlying NConsole") {
    for {
      console <- TestConsole.make[IO](screen = Screen(42, 13))
      middleware <- Middleware.make[IO](console, stubTicker)
      ctx <- middleware.context
    } yield {
      assertEquals(ctx, Screen(42, 13))
    }
  }

  test("Middleware writeString with no overlays writes content unchanged") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      _ <- middleware.writeString(ScreenAdjusted("hello"))
      written <- console.writtenFrames
    } yield {
      assertEquals(written.length, 1)
      assertEquals(written.head, "hello")
    }
  }

  test("Middleware writeString applies overlays") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      overlay = new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + " [overlay]"))
      }
      _ <- middleware.addOverlays(List(overlay))
      _ <- middleware.writeString(ScreenAdjusted("content"))
      written <- console.writtenFrames
    } yield {
      assertEquals(written.head, "content [overlay]")
    }
  }

  test("Middleware applies multiple overlays in order") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      overlay1 = new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[1]"))
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[2]"))
      }
      _ <- middleware.addOverlays(List(overlay1, overlay2))
      _ <- middleware.writeString(ScreenAdjusted("X"))
      written <- console.writtenFrames
    } yield {
      assertEquals(written.head, "X[1][2]")
    }
  }

  test("Middleware addOverlays replaces existing overlays") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      overlay1 = new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("first"))
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("second"))
      }
      _ <- middleware.addOverlays(List(overlay1))
      _ <- middleware.writeString(ScreenAdjusted("x"))
      _ <- middleware.addOverlays(List(overlay2))
      _ <- middleware.writeString(ScreenAdjusted("y"))
      written <- console.writtenFrames
    } yield {
      // written is in reverse order (newest first)
      assertEquals(written.head, "second")
      assertEquals(written(1), "first")
    }
  }

  test("Middleware close delegates to underlying NConsole") {
    for {
      console <- TestConsole.make[IO](screen = Screen(20, 5))
      middleware <- Middleware.make[IO](console, stubTicker)
      // close() on TestConsole is a no-op, just verify it doesn't fail
      _ <- middleware.close()
    } yield ()
  }
}
