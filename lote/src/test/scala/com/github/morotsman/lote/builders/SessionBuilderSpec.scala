package com.github.morotsman.lote.builders

import cats.effect.IO
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
}

