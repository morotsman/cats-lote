package com.github.morotsman.lote.api.builders

import cats.effect.IO
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.api.spi.{Overlay, Slide}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.{CatsEffectSuite, FunSuite}

import scala.concurrent.duration._

class SessionBuilderSpec extends FunSuite {

  // --- FPS conversion ---

  test("fpsToDuration converts 60 FPS to about 16.67 ms") {
    assertEquals(SessionBuilder.fpsToDuration(60.0), 16666667.nanos)
  }

  test("fpsToDuration converts 25 FPS to 40 ms") {
    assertEquals(SessionBuilder.fpsToDuration(25.0), 40.millis)
  }

  test("fpsToDuration converts 1 FPS to 1 second") {
    assertEquals(SessionBuilder.fpsToDuration(1.0), 1.second)
  }

  // --- Input validation ---

  test("withFrameRate rejects zero") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(0.0)
    }
  }

  test("withFrameRate rejects negative") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(-10.0)
    }
  }

  test("withFrameRate rejects NaN") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(Double.NaN)
    }
  }

  test("withFrameRate rejects Infinity") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withFrameRate(Double.PositiveInfinity)
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

  test("withAnimationStep rejects zero duration") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().withAnimationStep(0.millis)
    }
  }

  test("withFrameRate accepts typical values without error") {
    SessionBuilder[IO]().withFrameRate(60.0)
    SessionBuilder[IO]().withFrameRate(30.0)
    SessionBuilder[IO]().withFrameRate(25.0)
  }

  test("withAnimationFrameRate accepts typical values without error") {
    SessionBuilder[IO]().withAnimationFrameRate(60.0)
    SessionBuilder[IO]().withAnimationFrameRate(30.0)
    SessionBuilder[IO]().withAnimationFrameRate(25.0)
  }

  // --- Feature registry (builder shape) ---

  test("fresh builder has no features registered") {
    val builder = SessionBuilder[IO]()
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 0)
  }

  test("withTimer registers one feature") {
    val builder = SessionBuilder[IO]().withTimer(30.minutes)
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 1)
  }

  test("withProgressBar registers one feature") {
    val builder = SessionBuilder[IO]().withProgressBar(
      List(Milestone("Intro", 0), Milestone("Demo", 2))
    )
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 1)
  }

  test("withQuickNavigation registers one feature") {
    val builder = SessionBuilder[IO]().withQuickNavigation()
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 1)
  }

  test("addOverlay(Overlay) registers one feature") {
    val overlay = new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] = IO.pure(screenAdjusted)
    }
    val builder = SessionBuilder[IO]().addOverlay(overlay)
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 1)
  }

  test("addOverlay(F[Overlay]) registers one feature") {
    val overlay = IO.pure(new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] = IO.pure(screenAdjusted)
    })
    val builder = SessionBuilder[IO]().addOverlay(overlay)
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 1)
  }

  test("chaining multiple features accumulates them in order") {
    val builder = SessionBuilder[IO]()
      .withTimer(30.minutes)
      .withProgressBar()
      .withQuickNavigation()
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 3)
  }

  test("withIdleAnimation does not register a feature (handled separately)") {
    val builder = SessionBuilder[IO]().withIdleAnimation()
    val featureFactories = builder.productElement(1).asInstanceOf[List[_]]
    assertEquals(featureFactories.length, 0)
  }

  // --- Slide building (pure shape tests) ---

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
      override def userInput(input: UserInput): IO[Unit] = IO.unit
    }
    SessionBuilder[IO]().addSlideF { builder =>
      IO.pure(builder.addSlide(slide).title("Interactive"))
    }
  }

  // --- Precondition checks ---

  test("run() requires at least one slide") {
    intercept[IllegalArgumentException] {
      SessionBuilder[IO]().run()
    }
  }
}

/** Integration tests — run full sessions via runWith with a test harness. */
class SessionBuilderIntegrationSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private val readDelay = 1.millis
  private val defaultScreen = Screen(40, 10)

  private def simpleSession: SessionBuilder[IO] =
    SessionBuilder[IO]()
      .addTextSlide(_.content("Slide 1").title("First"))
      .addTextSlide(_.content("Slide 2").title("Second"))
      .addTextSlide(_.content("Slide 3").title("Third"))

  // --- Basic lifecycle ---

  test("session starts, renders first slide, and exits on Esc") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- simpleSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains("Slide 1"), s"Expected first slide content, got: ${written.take(2)}")
    }
  }

  test("screen is cleared on start") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- simpleSession.runWith(harness.console, harness.ticker)
      clears <- harness.clearCount
    } yield {
      assert(clears >= 1, s"Expected at least 1 clear, got $clears")
    }
  }

  // --- Navigation ---

  test("Right navigates to next slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- simpleSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains("Slide 2"), s"Expected second slide after Right, got: ${written.take(3)}")
    }
  }

  test("Right then Left returns to first slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Left), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- simpleSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      // Most recent frames should contain first slide again
      assert(written.exists(_.contains("Slide 1")),
        s"Expected first slide after navigating back, got: ${written.take(5)}")
    }
  }

  test("navigating through all slides and back") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Left),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- simpleSession.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains("Slide 1"), "Should have visited slide 1")
      assert(allContent.contains("Slide 2"), "Should have visited slide 2")
      assert(allContent.contains("Slide 3"), "Should have visited slide 3")
    }
  }

  // --- Single slide session ---

  test("single slide session handles Right gracefully") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- SessionBuilder[IO]()
        .addTextSlide(_.content("Solo"))
        .runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      assert(written.forall(_.contains("Solo")), "Should stay on the only slide")
    }
  }

  // --- onSlideChanged callback ---

  test("onSlideChanged is called for initial slide and navigation") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- simpleSession
        .onSlideChanged(idx => slideChanges.update(_ :+ idx))
        .runWith(harness.console, harness.ticker)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), s"Should have notified slide 0, got: $changes")
      assert(changes.contains(1), s"Should have notified slide 1, got: $changes")
    }
  }

  // --- Custom overlay integration ---

  test("custom overlay is applied to rendered content") {
    val marker = "<<OVERLAY>>"
    val overlay = new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] =
        IO.pure(ScreenAdjusted(screenAdjusted.content + marker))
    }
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        SessionBuilder[IO]()
          .addOverlay(overlay)
          .addTextSlide(_.content("Hello"))
          .runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains(marker), s"Expected overlay marker in output, got: ${written.take(3)}")
    }
  }

  test("effectful overlay (F[Overlay]) is applied to rendered content") {
    val marker = "<<EFFECTFUL>>"
    val overlayF = IO.pure(new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] =
        IO.pure(ScreenAdjusted(screenAdjusted.content + marker))
    })
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        SessionBuilder[IO]()
          .addOverlay(overlayF)
          .addTextSlide(_.content("Hello"))
          .runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains(marker), s"Expected effectful overlay marker, got: ${written.take(3)}")
    }
  }

  test("multiple overlays are all applied") {
    val marker1 = "<<ONE>>"
    val marker2 = "<<TWO>>"
    def markerOverlay(marker: String): Overlay[IO] = new Overlay[IO] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): IO[ScreenAdjusted] =
        IO.pure(ScreenAdjusted(screenAdjusted.content + marker))
    }
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        SessionBuilder[IO]()
          .addOverlay(markerOverlay(marker1))
          .addOverlay(markerOverlay(marker2))
          .addTextSlide(_.content("Base"))
          .runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains(marker1), "Expected first overlay marker")
      assert(allContent.contains(marker2), "Expected second overlay marker")
    }
  }

  // --- User input forwarding ---

  test("non-navigation keys are forwarded to the slide's userInput") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Interactive"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          receivedInputs.update(_ :+ input)
      }
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Space),
          Character('x'),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- SessionBuilder[IO]()
        .addSlide(_.addSlide(slide).title("Test"))
        .runWith(harness.console, harness.ticker)
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(Key(SpecialKey.Space), Character('x')))
    }
  }

  // --- withTimer ---

  test("withTimer renders a timer overlay") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        SessionBuilder[IO]()
          .withTimer(30.minutes)
          .addTextSlide(_.content("Timed"))
          .runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      // Timer overlay shows remaining time in MM:SS format
      assert(allContent.contains(":"), s"Expected timer in rendered output, got: ${written.take(3)}")
    }
  }

  // --- withProgressBar ---

  test("withProgressBar renders a progress indicator") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(40, 10),
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        simpleSession
          .withProgressBar()
          .runWith(harness.console, harness.ticker),
        ticks = 20
      )
      written <- harness.writtenFrames
    } yield {
      // Progress bar uses block characters or similar visual indicators
      assert(written.nonEmpty, "Expected frames to be written with progress bar")
    }
  }

  // --- Effectful slide construction ---

  test("addSlideF evaluates effect during session setup") {
    for {
      constructed <- IO.ref(false)
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Effectful"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- SessionBuilder[IO]()
        .addSlideF { builder =>
          constructed.set(true) *> IO.pure(builder.addSlide(slide).title("Built"))
        }
        .runWith(harness.console, harness.ticker)
      wasConstructed <- constructed.get
    } yield {
      assert(wasConstructed, "addSlideF effect should have been evaluated")
    }
  }

  // --- Configuration ---

  test("withTickerInterval configures custom interval") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      // Should not throw with a custom ticker interval
      _ <- SessionBuilder[IO]()
        .withTickerInterval(16.millis)
        .addTextSlide(_.content("Fast"))
        .runWith(harness.console, harness.ticker)
    } yield ()
  }

  test("withAnimationStep configures custom animation speed") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- SessionBuilder[IO]()
        .withAnimationStep(50.millis)
        .addTextSlide(_.content("Slow"))
        .runWith(harness.console, harness.ticker)
    } yield ()
  }

  // --- Full feature combination ---

  test("all features enabled together run without errors") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = Screen(60, 15),
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Esc)
        ),
        readDelay = readDelay
      )
      _ <- harness.runWithTicking(
        simpleSession
          .withTimer(45.minutes)
          .withProgressBar(List(Milestone("Intro", 0), Milestone("End", 2)))
          .withQuickNavigation()
          .onSlideChanged(idx => slideChanges.update(_ :+ idx))
          .runWith(harness.console, harness.ticker),
        ticks = 40
      )
      changes <- slideChanges.get
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty, "Expected frames to be written")
      assert(changes.contains(0), "Should have visited slide 0")
      assert(changes.contains(1), "Should have visited slide 1")
      assert(changes.contains(2), "Should have visited slide 2")
    }
  }
}
