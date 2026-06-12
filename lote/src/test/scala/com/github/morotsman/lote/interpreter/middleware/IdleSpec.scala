package com.github.morotsman.lote.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class IdleSpec extends CatsEffectSuite {

  private val shortIdleConfig = IdleConfig(
    idleTimeout = 50.millis,
    firstBugDelay = 0.seconds,
    spawnInterval = 20.millis,
    bugsPerSpawn = 2,
    maxBugs = 10,
    bugChars = List("@"),
    bugSpeed = 1
  )

  test("Idle does not modify content before timeout") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = IdleConfig(idleTimeout = 10.minutes))
      content = ScreenAdjusted("Hello World         ")
      result <- idle.applyOverlay(Screen(20, 5), content)
    } yield {
      // Not idle yet, so content should be unchanged
      assertEquals(result.content, content.content)
    }
  }

  test("Idle modifies content after timeout elapses") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig)
      content = ScreenAdjusted("Hello World         \n" * 4 + "Hello World         ")
      // Wait for idle timeout
      _ <- IO.sleep(100.millis)
      result <- idle.applyOverlay(Screen(20, 5), content)
    } yield {
      // After idle timeout, bugs should appear or text should be modified
      // The result should differ from original (bugs spawned or stolen words)
      assert(result.content != content.content || result.content.contains("@"),
        s"Expected content modification after idle, got: '${result.content}'")
    }
  }

  test("Idle resets on key press (notifyActivity via onKeyPress)") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig)
      content = ScreenAdjusted("Hello World         \n" * 4 + "Hello World         ")
      // Wait for idle
      _ <- IO.sleep(100.millis)
      // Simulate user activity
      _ <- idle.onKeyPress(Key(SpecialKey.Right))
      // Now it should no longer be idle
      result <- idle.applyOverlay(Screen(20, 5), content)
    } yield {
      assertEquals(result.content, content.content)
    }
  }

  test("Idle resets on content change") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig)
      content1 = ScreenAdjusted("First content       \n" * 4 + "First content       ")
      // Establish initial content
      _ <- idle.onContentChange(content1.content)
      // Wait for idle
      _ <- IO.sleep(100.millis)
      // Change content - should reset idle state
      content2 = ScreenAdjusted("New content here    \n" * 4 + "New content here    ")
      _ <- idle.onContentChange(content2.content)
      result <- idle.applyOverlay(Screen(20, 5), content2)
    } yield {
      // After content change, idle resets so output should be unchanged
      assertEquals(result.content, content2.content)
    }
  }

  test("Idle notifyActivity resets idle state") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig)
      content = ScreenAdjusted("Test content        \n" * 4 + "Test content        ")
      _ <- IO.sleep(100.millis)
      _ <- idle.notifyActivity()
      result <- idle.applyOverlay(Screen(20, 5), content)
    } yield {
      assertEquals(result.content, content.content)
    }
  }

  test("Idle spawns bugs that appear as overlay characters") {
    for {
      console <- TestNConsole.make(screen = Screen(30, 8))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig.copy(
        idleTimeout = 10.millis,
        firstBugDelay = 0.seconds,
        spawnInterval = 10.millis,
        bugsPerSpawn = 5
      ))
      content = ScreenAdjusted(("Hello World" + " " * 19 + "\n") * 7 + "Hello World" + " " * 19)
      _ <- IO.sleep(80.millis)
      // Apply overlay multiple times to let bugs move
      _ <- idle.applyOverlay(Screen(30, 8), content)
      _ <- IO.sleep(30.millis)
      result <- idle.applyOverlay(Screen(30, 8), content)
    } yield {
      // Bugs use "@" character
      assert(result.content.contains("@"),
        s"Expected bugs (@) in output after idle period")
    }
  }

  test("Idle with same content does not reset idle timer") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      idle <- Idle.make[IO](config = shortIdleConfig)
      content = ScreenAdjusted("Same content        \n" * 4 + "Same content        ")
      _ <- idle.onContentChange(content.content)
      _ <- IO.sleep(30.millis)
      // Same content again - should NOT reset timer
      _ <- idle.onContentChange(content.content)
      _ <- IO.sleep(40.millis)
      // Total elapsed > 50ms idle timeout, and same content didn't reset
      result <- idle.applyOverlay(Screen(20, 5), content)
    } yield {
      // Should be idle now since same content doesn't reset
      assert(result.content != content.content || result.content.contains("@"),
        "Expected idle state after same content repeated")
    }
  }
}

