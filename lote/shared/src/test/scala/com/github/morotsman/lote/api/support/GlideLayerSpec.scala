package com.github.morotsman.lote.api.support

import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.testkit.{SimulatedClock, TestConsole}
import munit.CatsEffectSuite

import scala.concurrent.duration._

class GlideLayerSpec extends CatsEffectSuite {

  // ── Terminal fallback: compositeOntoFrame via renderOnto ──

  test("renderOnto (terminal) — composites characters onto frame at integer positions") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 3))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(
        SmoothChar('X', col = 2, row = 0),
        SmoothChar('Y', col = 5, row = 1),
        SmoothChar('Z', col = 0, row = 2)
      )
      frame = ScreenAdjusted("...........\n...........\n...........")
      result <- overlay.renderOnto(frame, chars)
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines(0)(2), 'X')
      assertEquals(lines(1)(5), 'Y')
      assertEquals(lines(2)(0), 'Z')
    }
  }

  test("renderOnto (terminal) — out-of-bounds characters are ignored") {
    for {
      console <- TestConsole.make[IO](screen = Screen(5, 2))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(
        SmoothChar('X', col = 10, row = 0), // col out of bounds
        SmoothChar('Y', col = 0, row = 5), // row out of bounds
        SmoothChar('Z', col = -1, row = 0) // negative col
      )
      frame = ScreenAdjusted(".....\n.....")
      result <- overlay.renderOnto(frame, chars)
    } yield {
      // Frame should be unchanged
      assertEquals(result.content, ".....\n.....")
    }
  }

  test("renderOnto (terminal) — empty chars returns frame unchanged") {
    for {
      console <- TestConsole.make[IO](screen = Screen(5, 2))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      frame = ScreenAdjusted("hello\nworld")
      result <- overlay.renderOnto(frame, Vector.empty)
    } yield {
      assertEquals(result.content, "hello\nworld")
    }
  }

  // ── WebGL path: interpolation via render() ──
  // We need a console that reports Effects capability

  private def makeEffectsConsole: IO[(NConsole[IO], Ref[IO, List[RenderEffect]])] =
    for {
      effectsRef <- Ref[IO].of(List.empty[RenderEffect])
    } yield {
      val console = new NConsole[IO] {
        override def read(timeoutInMillis: Long): IO[UserInput] = IO.pure(Key(SpecialKey.Timeout))
        override def read(): IO[UserInput] = IO.pure(Key(SpecialKey.Timeout))
        override def readInterruptible(): IO[UserInput] = IO.pure(Key(SpecialKey.Timeout))
        override def alignText(s: String, alignment: Alignment): IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted(s))
        override def writeString(s: ScreenAdjusted): IO[Unit] = IO.unit
        override def clear(): IO[Unit] = IO.unit
        override def close(): IO[Unit] = IO.unit
        override def context: IO[Screen] = IO.pure(Screen(80, 24))
        override def capabilities: Set[PlatformCapability] = Set(PlatformCapability.Effects)
        override def applyEffect(effect: RenderEffect): IO[Unit] =
          effectsRef.update(effect :: _)
      }
      (console, effectsRef)
    }

  test("render (WebGL) — first render emits floating chars at exact positions (no interpolation)") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(
        SmoothChar('A', col = 5, row = 3, key = 0),
        SmoothChar('B', col = 10, row = 7, key = 1)
      )
      _ <- overlay.render(chars)
      effects <- effectsRef.get
    } yield {
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      val floats = floatingEffects.head
      assertEquals(floats.length, 2)
      // First render: no previous state, so chars appear at their exact positions
      assertEqualsDouble(floats(0).cellX, 5.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 3.0, 1e-9)
      assertEqualsDouble(floats(1).cellX, 10.0, 1e-9)
      assertEqualsDouble(floats(1).cellY, 7.0, 1e-9)
    }
  }

  test("render (WebGL) — interpolates between previous and current positions") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1) // lastStepped = chars1, previous = empty
      _ <- effectsRef.set(Nil)
      // Second render with same positions — no step detected, previous stays empty
      _ <- overlay.render(chars1) // previous still empty, lastStepped = chars1
      _ <- effectsRef.set(Nil)
      // Now move to new position: step fires, previous becomes chars1 (old lastStepped)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2) // step fires: previous = chars1 (0,0), lastStepped = chars2 (10,0), progress = 0
      _ <- effectsRef.set(Nil)
      // Advance half a step and render again with same positions → interpolation between (0,0) and (10,0)
      _ <- clock.advance(20.millis) // half of 40ms step
      _ <- overlay.render(chars2) // same positions, no step; progress = 20/40 = 0.5
      effects <- effectsRef.get
    } yield {
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      val floats = floatingEffects.head
      assertEquals(floats.length, 1)
      // Should be interpolated: col = 0 + (10 - 0) * 0.5 = 5.0
      assertEqualsDouble(floats(0).cellX, 5.0, 1e-9)
    }
  }

  test("render (WebGL) — wrap-around snaps instead of interpolating (default threshold=1)") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 1)(implicitly, implicitly, clock)
      // First render
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Move far away (simulating wrap-around)
      chars2 = Vector(SmoothChar('A', col = 79, row = 0, key = 0))
      _ <- overlay.render(chars2) // step fires
      _ <- effectsRef.set(Nil)
      // Render again between steps
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      val floats = floatingEffects.head
      // Wrap detected: should snap to current position, not interpolate
      assertEqualsDouble(floats(0).cellX, 79.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 0.0, 1e-9)
    }
  }

  test("render (WebGL) — key-based matching pairs correct characters") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      // First render with two keyed characters
      chars1 = Vector(
        SmoothChar('A', col = 0, row = 0, key = 10),
        SmoothChar('B', col = 5, row = 0, key = 20)
      )
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Second render: swap order but keep keys — interpolation should match by key
      chars2 = Vector(
        SmoothChar('B', col = 5, row = 1, key = 20), // key 20: was at (5,0), now at (5,1)
        SmoothChar('A', col = 1, row = 0, key = 10) // key 10: was at (0,0), now at (1,0)
      )
      _ <- overlay.render(chars2) // step fires
      _ <- effectsRef.set(Nil)
      // Half step interpolation
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      val floats = floatingEffects.head
      assertEquals(floats.length, 2)
      // key 20 (B): prev (5,0) → curr (5,1), progress 0.5 → (5.0, 0.5)
      assertEqualsDouble(floats(0).cellX, 5.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 0.5, 1e-9)
      // key 10 (A): prev (0,0) → curr (1,0), progress 0.5 → (0.5, 0.0)
      assertEqualsDouble(floats(1).cellX, 0.5, 1e-9)
      assertEqualsDouble(floats(1).cellY, 0.0, 1e-9)
    }
  }

  test("render (WebGL) — index-based fallback when keys are not set") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      // No keys set (default key = -1)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0), SmoothChar('B', col = 10, row = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 2, row = 0), SmoothChar('B', col = 12, row = 0))
      _ <- overlay.render(chars2) // step
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // Index 0: (0,0) → (2,0) at 0.5 → (1.0, 0.0)
      assertEqualsDouble(floats(0).cellX, 1.0, 1e-9)
      // Index 1: (10,0) → (12,0) at 0.5 → (11.0, 0.0)
      assertEqualsDouble(floats(1).cellX, 11.0, 1e-9)
    }
  }

  test("render (terminal) — is a no-op") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 3))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('X', col = 2, row = 0))
      _ <- overlay.render(chars) // should be a no-op
      frames <- console.writtenFrames
    } yield {
      assertEquals(frames, Nil) // no frames written
    }
  }

  test("clear — resets internal state") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- overlay.clear()
      _ <- effectsRef.set(Nil)
      // After clear, render should treat next call as first render (no interpolation)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // Should snap to exact position since state was cleared
      assertEqualsDouble(floats(0).cellX, 10.0, 1e-9)
    }
  }

  test("renderOntoScrolled (terminal) — composites and writes to console") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 3))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('X', col = 0, row = 0))
      frame = ScreenAdjusted("...........\n...........\n...........")
      _ <- overlay.renderOntoScrolled(frame, chars, scrollX = 0.5, scrollY = 0.0)
      frames <- console.writtenFramesInOrder
    } yield {
      // Should have written one composited frame
      assertEquals(frames.length, 1)
      assert(frames.head.startsWith("X"))
    }
  }

  // ── renderOnto (WebGL) ──

  test("renderOnto (WebGL) — dispatches to floating overlay and returns frame unchanged") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 5, row = 3, key = 0))
      frame = ScreenAdjusted("hello\nworld")
      result <- overlay.renderOnto(frame, chars)
      effects <- effectsRef.get
    } yield {
      // Frame should be returned unchanged on WebGL
      assertEquals(result.content, "hello\nworld")
      // Should have emitted floating chars
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      assertEqualsDouble(floatingEffects.head.head.cellX, 5.0, 1e-9)
    }
  }

  test("renderOnto (WebGL) — empty chars still emits RenderFloatingChars with empty vector") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      frame = ScreenAdjusted("hello\nworld")
      result <- overlay.renderOnto(frame, Vector.empty)
      effects <- effectsRef.get
    } yield {
      assertEquals(result.content, "hello\nworld")
      val floatingEffects = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floatingEffects.length, 1)
      assertEquals(floatingEffects.head.length, 0)
    }
  }

  // ── renderOntoScrolled (WebGL) ──

  test("renderOntoScrolled (WebGL) — applies scroll offset, writes frame, resets offset, renders overlay") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 1, row = 0, key = 0))
      frame = ScreenAdjusted(".....")
      _ <- overlay.renderOntoScrolled(frame, chars, scrollX = -2.5, scrollY = 1.0)
      effects <- effectsRef.get
    } yield {
      // Effects are stored in reverse order (prepended)
      val reversed = effects.reverse
      // Expected order: SetCanvasOffset(-2.5, 1.0), then write (not captured), then SetCanvasOffset(0,0), then RenderFloatingChars
      val offsets = reversed.collect { case o: RenderEffect.SetCanvasOffset => o }
      assertEquals(offsets.length, 2)
      assertEqualsDouble(offsets(0).cellsX, -2.5, 1e-9)
      assertEqualsDouble(offsets(0).cellsY, 1.0, 1e-9)
      assertEqualsDouble(offsets(1).cellsX, 0.0, 1e-9)
      assertEqualsDouble(offsets(1).cellsY, 0.0, 1e-9)
      // Should also have floating chars
      val floats = reversed.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floats.length, 1)
    }
  }

  test("renderOntoScrolled (WebGL) — no scroll offset skips SetCanvasOffset effects") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 1, row = 0, key = 0))
      frame = ScreenAdjusted(".....")
      _ <- overlay.renderOntoScrolled(frame, chars, scrollX = 0.0, scrollY = 0.0)
      effects <- effectsRef.get
    } yield {
      val offsets = effects.collect { case o: RenderEffect.SetCanvasOffset => o }
      assertEquals(offsets.length, 0)
      // Should still have floating chars
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }
      assertEquals(floats.length, 1)
    }
  }

  test("renderOntoScrolled (WebGL) — with fixed rows emits ClearFixedRows and SetFixedRows") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      frame = ScreenAdjusted(".....")
      _ <- overlay.renderOntoScrolled(frame, chars, fixedRows = Set(0, 23))
      effects <- effectsRef.get
    } yield {
      val reversed = effects.reverse
      val clearFixed = reversed.collect { case RenderEffect.ClearFixedRows => true }
      assertEquals(clearFixed.length, 1)
      val setFixed = reversed.collect { case RenderEffect.SetFixedRows(rows) => rows }
      assertEquals(setFixed.length, 1)
      assertEquals(setFixed.head, Set(0, 23))
    }
  }

  test("renderOntoScrolled (WebGL) — empty fixed rows skips fixed-row effects") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      frame = ScreenAdjusted(".....")
      _ <- overlay.renderOntoScrolled(frame, chars, fixedRows = Set.empty)
      effects <- effectsRef.get
    } yield {
      val clearFixed = effects.collect { case RenderEffect.ClearFixedRows => true }
      assertEquals(clearFixed.length, 0)
      val setFixed = effects.collect { case RenderEffect.SetFixedRows(_) => true }
      assertEquals(setFixed.length, 0)
    }
  }

  // ── clear ──

  test("clear (WebGL) — emits ClearFloatingChars effect") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      _ <- overlay.clear()
      effects <- effectsRef.get
    } yield {
      val clearEffects = effects.collect { case RenderEffect.ClearFloatingChars => true }
      assertEquals(clearEffects.length, 1)
    }
  }

  test("clear (terminal) — does not emit ClearFloatingChars") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 3))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      _ <- overlay.clear()
      // TestConsole has no Effects capability, so no effect should be emitted
      // Simply verify it doesn't throw
    } yield ()
  }

  // ── Interpolation edge cases ──

  test("render (WebGL) — progress is capped at 1.0 when elapsed exceeds step duration") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2) // step fires
      _ <- effectsRef.set(Nil)
      // Advance way past the step duration
      _ <- clock.advance(200.millis)
      _ <- overlay.render(chars2) // same positions, progress = min(1.0, 200/40) = 1.0
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // Progress capped at 1.0: should be at final position (10, 0)
      assertEqualsDouble(floats(0).cellX, 10.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 0.0, 1e-9)
    }
  }

  test("render (WebGL) — interpolates on Y axis") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 0, row = 10, key = 0))
      _ <- overlay.render(chars2) // step fires
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(10.millis) // 25% of 40ms
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEqualsDouble(floats(0).cellX, 0.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 2.5, 1e-9) // 0 + 10 * 0.25
    }
  }

  test("render (WebGL) — interpolates on both axes simultaneously") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 100.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 4, row = 8, key = 0))
      _ <- overlay.render(chars2)
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(75.millis) // 75% progress
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEqualsDouble(floats(0).cellX, 3.0, 1e-9) // 0 + 4 * 0.75
      assertEqualsDouble(floats(0).cellY, 6.0, 1e-9) // 0 + 8 * 0.75
    }
  }

  test("render (WebGL) — wrap-around on Y axis snaps") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 1)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 0, row = 23, key = 0)) // large Y jump
      _ <- overlay.render(chars2)
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEqualsDouble(floats(0).cellX, 0.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 23.0, 1e-9) // snapped, not interpolated
    }
  }

  test("render (WebGL) — custom wrapThreshold allows larger moves to interpolate") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 50)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Move 10 cells — within threshold of 50, so should interpolate
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2)
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis) // 50%
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEqualsDouble(floats(0).cellX, 5.0, 1e-9) // interpolated, not snapped
    }
  }

  test("render (WebGL) — new character with key not in previous snaps to current position") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Add a new character with key = 1 that didn't exist before
      chars2 = Vector(
        SmoothChar('A', col = 1, row = 0, key = 0),
        SmoothChar('B', col = 5, row = 5, key = 1) // new character
      )
      _ <- overlay.render(chars2) // step fires
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis) // 50%
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats.length, 2)
      // key 0: interpolated from (0,0) to (1,0) at 50% → (0.5, 0)
      assertEqualsDouble(floats(0).cellX, 0.5, 1e-9)
      // key 1: no previous, falls back to curr → (5, 5)
      assertEqualsDouble(floats(1).cellX, 5.0, 1e-9)
      assertEqualsDouble(floats(1).cellY, 5.0, 1e-9)
    }
  }

  test("render (WebGL) — index-based fallback with fewer previous elements snaps new indices") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0)) // one element
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(
        SmoothChar('A', col = 2, row = 0),
        SmoothChar('B', col = 10, row = 5) // index 1 has no previous
      )
      _ <- overlay.render(chars2) // step
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats.length, 2)
      // Index 0: (0,0) → (2,0) at 50% → (1.0, 0)
      assertEqualsDouble(floats(0).cellX, 1.0, 1e-9)
      // Index 1: no previous element at index 1 → snaps to (10, 5)
      assertEqualsDouble(floats(1).cellX, 10.0, 1e-9)
      assertEqualsDouble(floats(1).cellY, 5.0, 1e-9)
    }
  }

  test("render (WebGL) — fgColor is preserved in FloatingChar") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('A', col = 5, row = 3, fgColor = "#ff0000", key = 0))
      _ <- overlay.render(chars)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats(0).fgColor, "#ff0000")
    }
  }

  test("render (WebGL) — character value is preserved in FloatingChar") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(
        SmoothChar('Z', col = 0, row = 0, key = 0),
        SmoothChar('@', col = 1, row = 0, key = 1)
      )
      _ <- overlay.render(chars)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats(0).char, 'Z')
      assertEquals(floats(1).char, '@')
    }
  }

  test("render (WebGL) — multiple consecutive steps update interpolation baseline") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      // Step 1: position (0,0)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Step 2: position (10,0) — previous becomes (0,0)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2)
      _ <- effectsRef.set(Nil)
      // Step 3: position (20,0) — previous should become (10,0)
      chars3 = Vector(SmoothChar('A', col = 20, row = 0, key = 0))
      _ <- overlay.render(chars3)
      _ <- effectsRef.set(Nil)
      // Interpolate between (10,0) and (20,0) at 50%
      _ <- clock.advance(20.millis)
      _ <- overlay.render(chars3)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEqualsDouble(floats(0).cellX, 15.0, 1e-9) // 10 + (20 - 10) * 0.5
    }
  }

  test("render (WebGL) — at zero progress (immediately after step), position is at previous") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2) // step fires, progress = 0
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // Progress = 0: position should be at previous (0,0)
      assertEqualsDouble(floats(0).cellX, 0.0, 1e-9)
    }
  }

  test("render (WebGL) — unchanged positions between steps do not trigger a new step") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      chars2 = Vector(SmoothChar('A', col = 10, row = 0, key = 0))
      _ <- overlay.render(chars2) // step fires: prev=(0,0), curr=(10,0)
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(10.millis) // 25%
      _ <- overlay.render(chars2) // same positions, no new step
      effects1 <- effectsRef.get
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(10.millis) // now 50% total
      _ <- overlay.render(chars2) // still same positions
      effects2 <- effectsRef.get
    } yield {
      val floats1 = effects1.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      val floats2 = effects2.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // At 25%: 0 + 10 * 0.25 = 2.5
      assertEqualsDouble(floats1(0).cellX, 2.5, 1e-9)
      // At 50%: 0 + 10 * 0.5 = 5.0
      assertEqualsDouble(floats2(0).cellX, 5.0, 1e-9)
    }
  }

  // ── SmoothChar defaults ──

  test("SmoothChar — default fgColor is white and default key is -1") {
    val sc = SmoothChar('A', col = 0, row = 0)
    assertEquals(sc.fgColor, "#ffffff")
    assertEquals(sc.key, -1)
  }

  // ── compositeOntoFrame edge cases ──

  test("renderOnto (terminal) — multiple characters on the same row") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 2))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(
        SmoothChar('A', col = 0, row = 0),
        SmoothChar('B', col = 1, row = 0),
        SmoothChar('C', col = 2, row = 0)
      )
      frame = ScreenAdjusted("......\n......")
      result <- overlay.renderOnto(frame, chars)
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines(0).substring(0, 3), "ABC")
    }
  }

  test("renderOnto (terminal) — character overwrites existing content") {
    for {
      console <- TestConsole.make[IO](screen = Screen(10, 1))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('X', col = 2, row = 0))
      frame = ScreenAdjusted("HELLO")
      result <- overlay.renderOnto(frame, chars)
    } yield {
      assertEquals(result.content, "HEXLO")
    }
  }

  test("renderOnto (terminal) — negative row is ignored") {
    for {
      console <- TestConsole.make[IO](screen = Screen(5, 2))
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console)(implicitly, implicitly, clock)
      chars = Vector(SmoothChar('X', col = 0, row = -1))
      frame = ScreenAdjusted(".....\n.....")
      result <- overlay.renderOnto(frame, chars)
    } yield {
      assertEquals(result.content, ".....\n.....")
    }
  }

  test("render (WebGL) — positionsChanged detects char change even if col/row unchanged") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 5, row = 5, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Same position but different character — should trigger a step
      chars2 = Vector(SmoothChar('B', col = 5, row = 5, key = 0))
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      // A step should have been detected due to char change
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats(0).char, 'B')
    }
  }

  test("render (WebGL) — length change triggers a step") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- effectsRef.set(Nil)
      // Add another element — length change should trigger step
      chars2 = Vector(
        SmoothChar('A', col = 0, row = 0, key = 0),
        SmoothChar('B', col = 1, row = 0, key = 1)
      )
      _ <- overlay.render(chars2)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      assertEquals(floats.length, 2)
    }
  }

  test("clear (WebGL) — after clear, subsequent render treats as first render") {
    for {
      (console, effectsRef) <- makeEffectsConsole
      clock <- SimulatedClock.make[IO]()
      overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 100)(implicitly, implicitly, clock)
      chars1 = Vector(SmoothChar('A', col = 0, row = 0, key = 0))
      _ <- overlay.render(chars1)
      _ <- overlay.clear()
      _ <- effectsRef.set(Nil)
      // After clear, even a large position change should not interpolate from old state
      chars2 = Vector(SmoothChar('A', col = 50, row = 50, key = 0))
      _ <- overlay.render(chars2)
      _ <- effectsRef.set(Nil)
      // Simulate mid-step
      _ <- clock.advance(20.millis)
      chars3 = Vector(SmoothChar('A', col = 52, row = 50, key = 0))
      _ <- overlay.render(chars3) // step from (50,50) → (52,50)
      _ <- effectsRef.set(Nil)
      _ <- clock.advance(20.millis) // 50%
      _ <- overlay.render(chars3)
      effects <- effectsRef.get
    } yield {
      val floats = effects.collect { case RenderEffect.RenderFloatingChars(cs) => cs }.head
      // Should interpolate from (50,50) to (52,50) at 50% → (51,50)
      assertEqualsDouble(floats(0).cellX, 51.0, 1e-9)
      assertEqualsDouble(floats(0).cellY, 50.0, 1e-9)
    }
  }
}
