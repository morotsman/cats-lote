package com.github.morotsman.lote.api.builders

import cats.effect.IO
import com.github.morotsman.lote.api.{Milestone, Screen, ScreenAdjusted}
import com.github.morotsman.lote.api.spi.{Overlay, Slide}
import munit.FunSuite

import scala.concurrent.duration._

class SessionBuilderSpec extends FunSuite {

  test("fpsToDuration converts 60 FPS to about 16.67 ms") {
    val duration = SessionBuilder.fpsToDuration(60.0)

    assertEquals(duration, 16666667.nanos)
  }

  test("fpsToDuration converts 25 FPS to 40 ms") {
    val duration = SessionBuilder.fpsToDuration(25.0)

    assertEquals(duration, 40.millis)
  }

  test("withFrameRate rejects non-positive values") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(0.0)
    }

    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(-10.0)
    }
  }

  test("withAnimationFrameRate rejects non-positive values") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withAnimationFrameRate(0.0)
    }

    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withAnimationFrameRate(-10.0)
    }
  }

  test("withFrameRate accepts typical values") {
    SessionBuilder[IO]().withFrameRate(60.0)
    SessionBuilder[IO]().withFrameRate(30.0)
    SessionBuilder[IO]().withFrameRate(25.0)
  }

  test("withAnimationFrameRate accepts typical values") {
    SessionBuilder[IO]().withAnimationFrameRate(60.0)
    SessionBuilder[IO]().withAnimationFrameRate(30.0)
    SessionBuilder[IO]().withAnimationFrameRate(25.0)
  }

  test("addOverlay accepts a custom overlay") {
    val overlay = new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] = {
        val _ = context
        val _ = originalContent
        IO.pure(screenAdjusted)
      }
    }

    SessionBuilder[IO]().addOverlay(overlay)
  }

  test("withProgressBar stores milestone configuration") {
    val builder = SessionBuilder[IO]().withProgressBar(
      List(Milestone("Intro", 0), Milestone("Demo", 2))
    )

    assertEquals(builder.productElement(2), true)
    assertEquals(
      builder.productElement(3),
      List(Milestone("Intro", 0), Milestone("Demo", 2))
    )
  }

  test("addTextSlide accepts a direct builder lambda") {
    SessionBuilder[IO]().addTextSlide { builder =>
      builder.content("Hello").title("Intro")
    }
  }

  test("addTextSlide accepts staged text content") {
    SessionBuilder[IO]().addTextSlide { builder =>
      builder
        .content("Hello")
        .separator("\n")
        .step("World")
        .hint("[next]")
        .title("Intro")
    }
  }

  test("addSlideF can build slides from an effectful custom slide") {
    val slide = new Slide[IO] {
      override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Hello"))
      override def startShow: IO[Unit] = IO.unit
      override def stopShow: IO[Unit] = IO.unit
      override def userInput(input: com.github.morotsman.lote.api.UserInput): IO[Unit] = {
        val _ = input
        IO.unit
      }
    }

    SessionBuilder[IO]().addSlideF { builder =>
      IO.pure(builder.addSlide(slide).title("Interactive"))
    }
  }
}


