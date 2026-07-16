package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.IO
import com.github.morotsman.lote.api.{Milestone, Screen, ScreenAdjusted}
import com.github.morotsman.lote.internal.interpreter.middleware.ProgressBar
import munit.CatsEffectSuite

class ProgressBarSpec extends CatsEffectSuite {

  private val bright = "\u001b[97m"
  private val bold = "\u001b[1m"
  private val reset = "\u001b[0m"

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001b\\[[0-9;]*m", "")

  private def markerIndexes(progressLine: String): List[Int] =
    stripAnsi(progressLine).zipWithIndex.collect {
      case ('●', idx) => idx
      case ('◉', idx) => idx
      case ('○', idx) => idx
    }.toList

  test("ProgressBar shows first slide as current") {
    for {
      bar <- ProgressBar.make[IO](totalSlides = 5)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = stripAnsi(lines(7)) // screenHeight - 3
      val indexes = markerIndexes(progressLine)
      assert(
        progressLine.contains("◉"),
        s"Expected current indicator '◉' in: '$progressLine'"
      )
      assert(
        progressLine.contains("○"),
        s"Expected future indicator '○' in: '$progressLine'"
      )
      assert(progressLine.contains("─"), s"Expected connector line in: '$progressLine'")
      assertEquals(indexes.length, 5)
      assert(indexes.last - indexes.head > 10, s"Expected markers to spread across the width in: '$progressLine'")
    }
  }

  test("ProgressBar updates current slide indicator") {
    for {
      bar <- ProgressBar.make[IO](totalSlides = 3)
      _ <- bar.setCurrentSlide(1)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val rawProgressLine = lines(7)
      val progressLine = stripAnsi(rawProgressLine)
      val indexes = markerIndexes(progressLine)
      assert(
        progressLine.contains("●"),
        s"Expected past indicator '●' in: '$progressLine'"
      )
      assert(
        progressLine.contains("◉"),
        s"Expected current indicator '◉' in: '$progressLine'"
      )
      assert(
        rawProgressLine.contains(s"${bright}●${reset}"),
        s"Expected passed indicator to use bright styling in: '$rawProgressLine'"
      )
      assert(
        rawProgressLine.contains(s"${bold}${bright}◉${reset}"),
        s"Expected current indicator to use bold bright styling in: '$rawProgressLine'"
      )
      assert(
        progressLine.contains("○"),
        s"Expected future indicator '○' in: '$progressLine'"
      )
      assert(progressLine.contains("━") || progressLine.contains("─"), s"Expected connector line in: '$progressLine'")
      assertEquals(indexes.length, 3)
      assert(indexes.last - indexes.head > 10, s"Expected markers to spread across the width in: '$progressLine'")
    }
  }

  test("ProgressBar at last slide shows all passed except current") {
    for {
      bar <- ProgressBar.make[IO](totalSlides = 3)
      _ <- bar.setCurrentSlide(2)
      content = ScreenAdjusted((" " * 40 + "\n") * 9 + " " * 40)
      result <- bar.applyOverlay(Screen(40, 10), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = stripAnsi(lines(7))
      val indexes = markerIndexes(progressLine)
      assert(
        !progressLine.contains("○"),
        s"Expected no future indicator in: '$progressLine'"
      )
      assert(progressLine.contains("●"))
      assert(progressLine.contains("◉"))
      assert(progressLine.contains("━"), s"Expected passed connector line in: '$progressLine'")
      assertEquals(indexes.length, 3)
      assert(indexes.last - indexes.head > 10, s"Expected markers to spread across the width in: '$progressLine'")
    }
  }

  test("ProgressBar keeps the last marker inside the visible screen") {
    for {
      bar <- ProgressBar.make[IO](totalSlides = 10)
      _ <- bar.setCurrentSlide(9)
      content = ScreenAdjusted((" " * 120 + "\n") * 9 + " " * 120)
      result <- bar.applyOverlay(Screen(120, 10), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val progressLine = stripAnsi(lines(7))
      val indexes = markerIndexes(progressLine)

      assertEquals(progressLine.length, 120)
      assertEquals(indexes.length, 10)
      assert(
        indexes.last < progressLine.length - 1,
        s"Expected the last marker to leave one trailing column in: '$progressLine'"
      )
      assertEquals(progressLine.charAt(indexes.last), '◉')
      assert(
        progressLine.drop(indexes.last + 1).forall(_ == ' '),
        s"Expected only trailing padding after the last marker in: '$progressLine'"
      )
    }
  }

  test("ProgressBar with milestones renders milestone labels") {
    for {
      bar <- ProgressBar.make[IO](
        totalSlides = 5,
        milestones = List(Milestone("Intro", 0), Milestone("Main", 2))
      )
      content = ScreenAdjusted((" " * 60 + "\n") * 11 + " " * 60)
      result <- bar.applyOverlay(Screen(60, 12), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val milestoneLine = lines(8) // progressRowIndex - 1 = (12-3) - 1 = 8
      // Strip ANSI codes to check labels
      val stripped = stripAnsi(milestoneLine)
      assert(stripped.contains("Intro"), s"Expected 'Intro' in: '$stripped'")
      assert(stripped.contains("Main"), s"Expected 'Main' in: '$stripped'")
    }
  }

  test("ProgressBar spreads milestone labels across the available width") {
    for {
      bar <- ProgressBar.make[IO](
        totalSlides = 10,
        milestones = List(
          Milestone("Start", 0),
          Milestone("Transitions", 3),
          Milestone("Overlays", 5),
          Milestone("Interactive", 8),
          Milestone("Bye", 9)
        )
      )
      content = ScreenAdjusted((" " * 120 + "\n") * 11 + " " * 120)
      result <- bar.applyOverlay(Screen(120, 12), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val milestoneLine = stripAnsi(lines(8))

      val startIndex = milestoneLine.indexOf("Start")
      val transitionsIndex = milestoneLine.indexOf("Transitions")
      val overlaysIndex = milestoneLine.indexOf("Overlays")
      val interactiveIndex = milestoneLine.indexOf("Interactive")
      val byeIndex = milestoneLine.indexOf("Bye")

      assert(startIndex >= 0, s"Expected 'Start' in: '$milestoneLine'")
      assert(transitionsIndex > startIndex, s"Expected 'Transitions' after 'Start' in: '$milestoneLine'")
      assert(overlaysIndex > transitionsIndex, s"Expected 'Overlays' after 'Transitions' in: '$milestoneLine'")
      assert(interactiveIndex > overlaysIndex, s"Expected 'Interactive' after 'Overlays' in: '$milestoneLine'")
      assert(byeIndex > interactiveIndex, s"Expected 'Bye' after 'Interactive' in: '$milestoneLine'")

      assert(
        transitionsIndex - startIndex >= 8,
        s"Expected visible spacing between Start and Transitions in: '$milestoneLine'"
      )
      assert(
        overlaysIndex - transitionsIndex >= 8,
        s"Expected visible spacing between Transitions and Overlays in: '$milestoneLine'"
      )
      assert(
        interactiveIndex - overlaysIndex >= 8,
        s"Expected visible spacing between Overlays and Interactive in: '$milestoneLine'"
      )
    }
  }

  test("ProgressBar keeps the last milestone inside the visible screen") {
    for {
      bar <- ProgressBar.make[IO](
        totalSlides = 10,
        milestones = List(
          Milestone("Start", 0),
          Milestone("Transitions", 3),
          Milestone("Overlays", 5),
          Milestone("Interactive", 8),
          Milestone("Bye", 9)
        )
      )
      content = ScreenAdjusted((" " * 120 + "\n") * 11 + " " * 120)
      result <- bar.applyOverlay(Screen(120, 12), content, content)
    } yield {
      val lines = result.content.split("\n", -1)
      val milestoneLine = stripAnsi(lines(8))
      val byeIndex = milestoneLine.indexOf("Bye")

      assert(byeIndex >= 0, s"Expected 'Bye' in: '$milestoneLine'")
      assertEquals(milestoneLine.length, 120)
      assert(
        byeIndex + "Bye".length < milestoneLine.length,
        s"Expected at least one trailing column after 'Bye' in: '$milestoneLine'"
      )
      assert(
        milestoneLine.drop(byeIndex + "Bye".length).forall(_ == ' '),
        s"Expected only trailing padding after 'Bye' in: '$milestoneLine'"
      )
    }
  }
}
