package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.api.{Key, Screen, ScreenAdjusted, SpecialKey}
import com.github.morotsman.lote.internal.algebra.IdleDetector
import com.github.morotsman.lote.internal.interpreter.{IdleDetectorConfig, IdleDetectorInterpreter}
import com.github.morotsman.lote.internal.util.Colors
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class IdleSpec extends CatsEffectSuite {

  private val shortIdleTimeout = 50.millis

  private val shortOverlayConfig = IdleOverlayConfig(
    firstBugDelay = 0.seconds,
    spawnInterval = 20.millis,
    bugsPerSpawn = 2,
    maxBugs = 10,
    bugChars = List("@")
  )

  private def makeIdleWithDetector(
      timeout: FiniteDuration = shortIdleTimeout,
      overlayConfig: IdleOverlayConfig = shortOverlayConfig
  ): IO[(IdleDetector[IO], Idle[IO])] =
    for {
      detector <- IdleDetectorInterpreter.make[IO](
        IdleDetectorConfig(idleTimeout = timeout)
      )
      idle <- Idle.make[IO](detector, overlayConfig)
    } yield (detector, idle)

  test("Idle does not modify content before timeout") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (_, idle) <- makeIdleWithDetector(timeout = 10.minutes)
      content = ScreenAdjusted("Hello World         ")
      result <- idle.applyOverlay(Screen(20, 5), content, content)
    } yield {
      assertEquals(result.content, content.content)
    }
  }

  test("Idle modifies content after timeout elapses") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (_, idle) <- makeIdleWithDetector()
      content = ScreenAdjusted(
        "Hello World         \n" * 4 + "Hello World         "
      )
      _ <- IO.sleep(100.millis)
      result <- idle.applyOverlay(Screen(20, 5), content, content)
    } yield {
      assert(
        result.content != content.content || result.content.contains("@"),
        s"Expected content modification after idle, got: '${result.content}'"
      )
    }
  }

  test("Idle resets on key press (via IdleDetector)") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (detector, idle) <- makeIdleWithDetector()
      content = ScreenAdjusted(
        "Hello World         \n" * 4 + "Hello World         "
      )
      _ <- IO.sleep(100.millis)
      // Simulate user activity through the detector (as Middleware would)
      _ <- detector.onKeyPress(Key(SpecialKey.Right))
      result <- idle.applyOverlay(Screen(20, 5), content, content)
    } yield {
      assertEquals(result.content, content.content)
    }
  }

  test("Idle resets on content change (via IdleDetector)") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (detector, idle) <- makeIdleWithDetector()
      content1 = ScreenAdjusted(
        "First content       \n" * 4 + "First content       "
      )
      _ <- detector.onContentChange(content1.content)
      _ <- IO.sleep(100.millis)
      // Change content through the detector (as Middleware would)
      content2 = ScreenAdjusted(
        "New content here    \n" * 4 + "New content here    "
      )
      _ <- detector.onContentChange(content2.content)
      result <- idle.applyOverlay(Screen(20, 5), content2, content2)
    } yield {
      assertEquals(result.content, content2.content)
    }
  }

  test("Idle notifyActivity on detector resets idle state") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (detector, idle) <- makeIdleWithDetector()
      content = ScreenAdjusted(
        "Test content        \n" * 4 + "Test content        "
      )
      _ <- IO.sleep(100.millis)
      _ <- detector.notifyActivity()
      result <- idle.applyOverlay(Screen(20, 5), content, content)
    } yield {
      assertEquals(result.content, content.content)
    }
  }

  test("Idle spawns bugs that appear as overlay characters") {
    for {
      _ <- TestNConsole.make(screen = Screen(30, 8))
      (_, idle) <- makeIdleWithDetector(
        timeout = 10.millis,
        overlayConfig = shortOverlayConfig.copy(
          firstBugDelay = 0.seconds,
          spawnInterval = 10.millis,
          bugsPerSpawn = 5
        )
      )
      content = ScreenAdjusted(
        ("Hello World" + " " * 19 + "\n") * 7 + "Hello World" + " " * 19
      )
      _ <- IO.sleep(80.millis)
      _ <- idle.applyOverlay(Screen(30, 8), content, content)
      _ <- IO.sleep(30.millis)
      result <- idle.applyOverlay(Screen(30, 8), content, content)
    } yield {
      assert(
        result.content.contains("@"),
        s"Expected bugs (@) in output after idle period"
      )
    }
  }

  test("Idle with same content does not reset idle timer") {
    for {
      _ <- TestNConsole.make(screen = Screen(20, 5))
      (detector, idle) <- makeIdleWithDetector()
      content = ScreenAdjusted(
        "Same content        \n" * 4 + "Same content        "
      )
      _ <- detector.onContentChange(content.content)
      _ <- IO.sleep(30.millis)
      // Same content again - should NOT reset timer
      _ <- detector.onContentChange(content.content)
      _ <- IO.sleep(40.millis)
      result <- idle.applyOverlay(Screen(20, 5), content, content)
    } yield {
      assert(
        result.content != content.content || result.content.contains("@"),
        "Expected idle state after same content repeated"
      )
    }
  }

  test("Idle ANSI support treats colored stolen text as fully blank") {
    val coloredLine = s"${Colors.bright}Hello${Colors.reset}"

    val blanked = IdleAnsiSupport.applyStolen(
      Array(coloredLine),
      List(StolenWord(0, 0, 5))
    )

    assertEquals(IdleAnsiSupport.stripAnsi(blanked.head), "     ")
    assert(!IdleAnsiSupport.hasVisibleText(blanked.head))
  }

  test("Idle ANSI support overlays bug text using visible columns") {
    val coloredLine = s"${Colors.bright}ABCDE${Colors.reset}"

    val overlaid = IdleAnsiSupport.overlayText(
      coloredLine,
      startCol = 1,
      text = "@Z",
      screenWidth = 5
    )

    assertEquals(IdleAnsiSupport.stripAnsi(overlaid), "A@ZDE")
  }
}
