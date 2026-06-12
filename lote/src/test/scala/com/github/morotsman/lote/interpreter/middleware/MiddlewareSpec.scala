package com.github.morotsman.lote.interpreter.middleware

import cats.effect.IO
import cats.effect.kernel.Ref
import com.github.morotsman.lote.algebra.{NConsole, Overlay, Ticker, TickerSubscription}
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class MiddlewareSpec extends CatsEffectSuite {

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
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      result <- middleware.alignText("Hi", Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
    } yield {
      assert(result.content.contains("Hi"))
    }
  }

  test("Middleware delegates clear to underlying NConsole") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      _ <- middleware.clear()
      cleared <- console.clearedRef.get
    } yield {
      assertEquals(cleared, 1)
    }
  }

  test("Middleware delegates context to underlying NConsole") {
    for {
      console <- TestNConsole.make(screen = Screen(42, 13))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      ctx <- middleware.context
    } yield {
      assertEquals(ctx, Screen(42, 13))
    }
  }

  test("Middleware writeString with no overlays writes content unchanged") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
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
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      overlay = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + " [overlay]"))
        override def onKeyPress(input: UserInput): IO[Unit] = IO.unit
        override def onContentChange(content: String): IO[Unit] = IO.unit
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
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      overlay1 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[1]"))
        override def onKeyPress(input: UserInput): IO[Unit] = IO.unit
        override def onContentChange(content: String): IO[Unit] = IO.unit
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(screenAdjusted.content + "[2]"))
        override def onKeyPress(input: UserInput): IO[Unit] = IO.unit
        override def onContentChange(content: String): IO[Unit] = IO.unit
      }
      _ <- middleware.addOverlays(List(overlay1, overlay2))
      _ <- middleware.writeString(ScreenAdjusted("X"))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.head, "X[1][2]")
    }
  }

  test("Middleware read notifies overlays of key press") {
    for {
      keyPresses <- Ref[IO].of(List.empty[UserInput])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Character('a'))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      overlay = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(screenAdjusted)
        override def onKeyPress(input: UserInput): IO[Unit] =
          keyPresses.update(input :: _)
        override def onContentChange(content: String): IO[Unit] = IO.unit
      }
      _ <- middleware.addOverlays(List(overlay))
      input <- middleware.read()
      presses <- keyPresses.get
    } yield {
      assertEquals(input, Character('a'))
      assertEquals(presses, List(Character('a')))
    }
  }

  test("Middleware readInterruptible delegates without notifying overlays") {
    for {
      keyPresses <- Ref[IO].of(List.empty[UserInput])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Esc))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      overlay = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(screenAdjusted)
        override def onKeyPress(input: UserInput): IO[Unit] =
          keyPresses.update(input :: _)
        override def onContentChange(content: String): IO[Unit] = IO.unit
      }
      _ <- middleware.addOverlays(List(overlay))
      input <- middleware.readInterruptible()
      presses <- keyPresses.get
    } yield {
      assertEquals(input, Key(SpecialKey.Esc))
      assertEquals(presses, Nil) // readInterruptible should NOT notify overlays
    }
  }

  test("Middleware addOverlays replaces existing overlays") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      implicit0(tk: Ticker[IO]) = stubTicker
      middleware <- Middleware.make[IO]()
      overlay1 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("first"))
        override def onKeyPress(input: UserInput): IO[Unit] = IO.unit
        override def onContentChange(content: String): IO[Unit] = IO.unit
      }
      overlay2 = new Overlay[IO] {
        override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("second"))
        override def onKeyPress(input: UserInput): IO[Unit] = IO.unit
        override def onContentChange(content: String): IO[Unit] = IO.unit
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

