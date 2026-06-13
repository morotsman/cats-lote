package com.github.morotsman.lote.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class ProgressBarSpec extends CatsEffectSuite {

  test("ProgressBar shows first slide as current") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      bar <- ProgressBar.make[IO](totalSlides = 5)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = lines(7) // screenHeight - 3
      assert(progressLine.contains("0"), s"Expected current indicator '0' in: '$progressLine'")
      assert(progressLine.contains("-"), s"Expected future indicator '-' in: '$progressLine'")
    }
  }

  test("ProgressBar updates current slide indicator") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      bar <- ProgressBar.make[IO](totalSlides = 3)
      _ <- bar.setCurrentSlide(1)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = lines(7)
      // Should show: # 0 -  (first passed, second current, third future)
      assert(progressLine.contains("#"), s"Expected past indicator '#' in: '$progressLine'")
      assert(progressLine.contains("0"), s"Expected current indicator '0' in: '$progressLine'")
      assert(progressLine.contains("-"), s"Expected future indicator '-' in: '$progressLine'")
    }
  }

  test("ProgressBar at last slide shows all passed except current") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      bar <- ProgressBar.make[IO](totalSlides = 3)
      _ <- bar.setCurrentSlide(2)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = lines(7)
      // Should show: # # 0
      assert(!progressLine.contains("-"), s"Expected no future indicator in: '$progressLine'")
      assert(progressLine.contains("#"))
      assert(progressLine.contains("0"))
    }
  }

  test("ProgressBar with milestones renders milestone labels") {
    for {
      console <- TestNConsole.make(screen = Screen(60, 12))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      bar <- ProgressBar.make[IO](
        totalSlides = 5,
        milestones = List(Milestone("Intro", 0), Milestone("Main", 2))
      )
      content = ScreenAdjusted((" " * 60 + "\n") * 11 + " " * 60)
      result <- bar.applyOverlay(Screen(60, 12), content)
    } yield {
      val lines = result.content.split("\n", -1)
      val milestoneLine = lines(8) // progressRowIndex - 1 = (12-3) - 1 = 8
      // Strip ANSI codes to check labels
      val stripped = milestoneLine.replaceAll("\u001b\\[[0-9;]*m", "")
      assert(stripped.contains("Intro"), s"Expected 'Intro' in: '$stripped'")
      assert(stripped.contains("Main"), s"Expected 'Main' in: '$stripped'")
    }
  }
}

