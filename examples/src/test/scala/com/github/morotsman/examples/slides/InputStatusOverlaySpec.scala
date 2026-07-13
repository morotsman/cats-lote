package com.github.morotsman.examples.slides

import cats.effect.IO
import cats.implicits._
import com.github.morotsman.lote.api.{Character, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

class InputStatusOverlaySpec extends CatsEffectSuite {

  private val smallScreen = Screen(60, 3)

  private def blankContent(screen: Screen): ScreenAdjusted =
    ScreenAdjusted(List.fill(screen.screenHeight)(" " * screen.screenWidth).mkString("\n"))

  test("initial state shows zero keys and 'none' as last input") {
    for {
      harness <- SlideTestHarness.make[IO](screen = smallScreen)
      overlay <- InputStatusOverlay.make[IO]()
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(harness.screen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(firstLine.contains("keys seen: 0"), s"Expected 'keys seen: 0', got: '$firstLine'")
      assert(firstLine.contains("last input: none"), s"Expected 'last input: none', got: '$firstLine'")
    }
  }

  test("onUserInput increments the key count") {
    for {
      harness <- SlideTestHarness.make[IO](screen = smallScreen)
      overlay <- InputStatusOverlay.make[IO]()
      _ <- overlay.onUserInput(Key(SpecialKey.Right))
      _ <- overlay.onUserInput(Key(SpecialKey.Left))
      _ <- overlay.onUserInput(Character('x'))
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(harness.screen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(firstLine.contains("keys seen: 3"), s"Expected 'keys seen: 3', got: '$firstLine'")
    }
  }

  test("onUserInput updates last input label for arrow keys") {
    for {
      harness <- SlideTestHarness.make[IO](screen = smallScreen)
      overlay <- InputStatusOverlay.make[IO]()
      _ <- overlay.onUserInput(Key(SpecialKey.Up))
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(harness.screen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(firstLine.contains("last input: Up"), s"Expected 'last input: Up', got: '$firstLine'")
    }
  }

  test("onUserInput updates last input label for characters") {
    for {
      harness <- SlideTestHarness.make[IO](screen = smallScreen)
      overlay <- InputStatusOverlay.make[IO]()
      _ <- overlay.onUserInput(Character('z'))
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(harness.screen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(
        firstLine.contains("last input: character 'z'"),
        s"Expected \"last input: character 'z'\", got: '$firstLine'"
      )
    }
  }

  test("onUserInput tracks special keys correctly") {
    val cases: List[(UserInput, String)] = List(
      Key(SpecialKey.Enter) -> "Enter",
      Key(SpecialKey.Space) -> "Space",
      Key(SpecialKey.Down) -> "Down",
      Key(SpecialKey.Esc) -> "Esc",
      Key(SpecialKey.Timeout) -> "Timeout"
    )

    cases.traverse_ { case (input, expectedLabel) =>
      for {
        overlay <- InputStatusOverlay.make[IO]()
        _ <- overlay.onUserInput(input)
        base = blankContent(smallScreen)
        result <- overlay.applyOverlay(smallScreen, base, base)
      } yield {
        val firstLine = result.content.split("\n", -1).head
        assert(
          firstLine.contains(s"last input: $expectedLabel"),
          s"For $input: expected 'last input: $expectedLabel', got: '$firstLine'"
        )
      }
    }
  }

  test("overlay replaces only the first line") {
    for {
      overlay <- InputStatusOverlay.make[IO]()
      base = ScreenAdjusted(
        "original line 1                                             \n" +
          "original line 2                                             \n" +
          "original line 3                                             "
      )
      result <- overlay.applyOverlay(smallScreen, base, base)
    } yield {
      val lines = result.content.split("\n", -1)
      assert(lines(0).contains("[input overlay]"), s"First line should be the status, got: '${lines(0)}'")
      assertEquals(lines(1), "original line 2                                             ")
      assertEquals(lines(2), "original line 3                                             ")
    }
  }

  test("status line is padded to screen width") {
    for {
      overlay <- InputStatusOverlay.make[IO]()
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(smallScreen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assertEquals(
        firstLine.length,
        smallScreen.screenWidth,
        s"Status line should be exactly ${smallScreen.screenWidth} chars, got ${firstLine.length}"
      )
    }
  }

  test("overlay pads missing lines when content is shorter than screen height") {
    for {
      overlay <- InputStatusOverlay.make[IO]()
      base = ScreenAdjusted("short")
      result <- overlay.applyOverlay(smallScreen, base, base)
    } yield {
      val lines = result.content.split("\n", -1)
      assertEquals(lines.length, smallScreen.screenHeight, "Output should have exactly screenHeight lines")
    }
  }

  test("last input reflects the most recent call, not earlier ones") {
    for {
      overlay <- InputStatusOverlay.make[IO]()
      _ <- overlay.onUserInput(Key(SpecialKey.Left))
      _ <- overlay.onUserInput(Key(SpecialKey.Right))
      _ <- overlay.onUserInput(Character('q'))
      base = blankContent(smallScreen)
      result <- overlay.applyOverlay(smallScreen, base, base)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(
        firstLine.contains("last input: character 'q'"),
        s"Should show last input, got: '$firstLine'"
      )
      assert(
        !firstLine.contains("Left") && !firstLine.contains("Right"),
        s"Should not show earlier inputs, got: '$firstLine'"
      )
    }
  }
}

