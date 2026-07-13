package com.github.morotsman.examples.slides

import cats.effect.IO
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

class CornerLabelOverlaySpec extends CatsEffectSuite {

  test("overlay places label in the top-right corner") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 3))
      overlay = CornerLabelOverlay[IO]("v1.0")
      baseContent = ScreenAdjusted(
        List.fill(3)(" " * 19).mkString("\n")
      )
      result <- overlay.applyOverlay(harness.screen, baseContent, baseContent)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(
        firstLine.endsWith("[v1.0]"),
        s"Expected first line to end with '[v1.0]', got: '$firstLine'"
      )
    }
  }

  test("overlay preserves content not overlapping with label") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 3))
      overlay = CornerLabelOverlay[IO]("Hi")
      baseContent = ScreenAdjusted(
        "Hello World         \n" +
          "                    \n" +
          "                    "
      )
      result <- overlay.applyOverlay(harness.screen, baseContent, baseContent)
    } yield {
      val lines = result.content.split("\n", -1)
      assert(lines(0).contains("Hello"), s"Content should be preserved, got: '${lines(0)}'")
      assert(lines(0).endsWith("[Hi]"), s"Label should be at end, got: '${lines(0)}'")
      assertEquals(lines(1).trim, "")
      assertEquals(lines(2).trim, "")
    }
  }

  test("overlay truncates label if wider than screen") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(5, 2))
      overlay = CornerLabelOverlay[IO]("very long label")
      baseContent = ScreenAdjusted("    \n    ")
      result <- overlay.applyOverlay(harness.screen, baseContent, baseContent)
    } yield {
      val firstLine = result.content.split("\n", -1).head
      assert(
        firstLine.length <= 4,
        s"First line should fit within screen width, got length ${firstLine.length}: '$firstLine'"
      )
    }
  }
}

