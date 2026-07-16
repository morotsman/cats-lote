package com.github.morotsman.lote.internal.interpreter

import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Transition}
import com.github.morotsman.lote.internal.TextSlide
import com.github.morotsman.lote.internal.model.{Presentation, SlideSpecification}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class PresentationExecutorCommandPatternSpec extends CatsEffectSuite {

  private val defaultAlignment = Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
  private val defaultScreen = Screen(30, 5)

  private def textSlide(text: String)(implicit nc: NConsole[IO]): Slide[IO] =
    TextSlide[IO](text, defaultAlignment)

  private def trackingSlide(
      name: String,
      startRef: Ref[IO, Int],
      stopRef: Ref[IO, Int],
      inputRef: Ref[IO, List[UserInput]]
  ): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(name))
    override def startShow: IO[Unit] = startRef.update(_ + 1)
    override def stopShow: IO[Unit] = stopRef.update(_ + 1)
    override def userInput(input: UserInput): IO[Unit] = inputRef.update(_ :+ input)
  }

  // --- Navigation: Right ---

  test("navigates right through all slides sequentially") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Slide A"), out = None),
          SlideSpecification(slide = textSlide("Slide B"), out = None),
          SlideSpecification(slide = textSlide("Slide C"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 30)
      changes <- slideChanges.get
    } yield {
      assertEquals(changes, List(0, 1, 2))
    }
  }

  test("right at last slide stays on last slide") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Right), // beyond last
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("First"), out = None),
          SlideSpecification(slide = textSlide("Last"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 30)
      changes <- slideChanges.get
    } yield {
      // After going right twice from index 0, we reach index 1 and stay there
      assert(changes.contains(0))
      assert(changes.contains(1))
      assert(!changes.contains(2), "Should never reach index 2")
    }
  }

  // --- Navigation: Left ---

  test("navigates left back to previous slide") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Slide A"), out = None),
          SlideSpecification(slide = textSlide("Slide B"), out = None),
          SlideSpecification(slide = textSlide("Slide C"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 40)
      changes <- slideChanges.get
    } yield {
      assertEquals(changes, List(0, 1, 2, 1))
    }
  }

  test("left at first slide stays on first slide") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Left), // already at 0
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Only"), out = None),
          SlideSpecification(slide = textSlide("Second"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      changes <- slideChanges.get
    } yield {
      // Should visit slide 0 twice (initial + after left bounces back)
      assert(changes.forall(_ == 0), s"Should only visit slide 0, got: $changes")
      assert(changes.length >= 2, s"Expected at least 2 visits to slide 0, got: $changes")
    }
  }

  // --- Exit ---

  test("Esc terminates the execution loop") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Hello"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      // Should complete without hanging
      _ <- harness.runWithTicking(executor.start(), ticks = 10)
    } yield {
      // If we reach here without timeout, the test passed
      assert(true)
    }
  }

  // --- Slide lifecycle (startShow / stopShow) ---

  test("startShow is called when entering a slide") {
    for {
      startCount <- IO.ref(0)
      stopCount <- IO.ref(0)
      inputLog <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      slide = trackingSlide("Tracked", startCount, stopCount, inputLog)
      presentation = Presentation[IO](
        slideSpecifications = List(SlideSpecification(slide = slide, out = None))
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 10)
      starts <- startCount.get
      stops <- stopCount.get
    } yield {
      assert(starts >= 1, s"startShow should be called at least once, got $starts")
      assert(stops >= 1, s"stopShow should be called on exit, got $stops")
    }
  }

  test("stopShow is called when navigating away from a slide") {
    for {
      stopCount <- IO.ref(0)
      startCount <- IO.ref(0)
      inputLog <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      trackedSlide = trackingSlide("First", startCount, stopCount, inputLog)
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = trackedSlide, out = None),
          SlideSpecification(slide = textSlide("Second"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      stops <- stopCount.get
    } yield {
      assert(stops >= 1, s"stopShow should be called when navigating away, got $stops")
    }
  }

  // --- User input delegation ---

  test("non-navigation keys are forwarded to the current slide") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Space),
          Key(SpecialKey.Enter),
          Character('x'),
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Interactive"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          receivedInputs.update(_ :+ input)
      }
      presentation = Presentation[IO](
        slideSpecifications = List(SlideSpecification(slide = slide, out = None))
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(Key(SpecialKey.Space), Key(SpecialKey.Enter), Character('x')))
    }
  }

  test("Character input is forwarded to current slide") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Character('a'), Character('b'), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("CharSlide"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          receivedInputs.update(_ :+ input)
      }
      presentation = Presentation[IO](
        slideSpecifications = List(SlideSpecification(slide = slide, out = None))
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(Character('a'), Character('b')))
    }
  }

  // --- onSlideChange callback ---

  test("onSlideChange is called in order for each slide visited") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("A"), out = None),
          SlideSpecification(slide = textSlide("B"), out = None),
          SlideSpecification(slide = textSlide("C"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 40)
      changes <- slideChanges.get
    } yield {
      assertEquals(changes, List(0, 1, 2, 1))
    }
  }

  test("onSlideChange is not called when no navigation occurs") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Space), // delegated input, no navigation
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Only"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      changes <- slideChanges.get
    } yield {
      // Only the initial slide entry should trigger the callback
      assertEquals(changes, List(0))
    }
  }

  // --- setSlide (external navigation) ---

  test("setSlide navigates to the specified index on next loop iteration") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Space), // triggers userInput, during which we'll call setSlide
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      executorRef <- IO.ref(Option.empty[com.github.morotsman.lote.internal.algebra.PresentationExecutor[IO]])
      // Create a slide that triggers setSlide when it gets input
      jumpSlide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Jump"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          executorRef.get.flatMap(_.fold(IO.unit)(_.setSlide(2)))
      }
      presentationWithJump = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = jumpSlide, out = None),
          SlideSpecification(slide = textSlide("Slide 1"), out = None),
          SlideSpecification(slide = textSlide("Slide 2"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentationWithJump,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- executorRef.set(Some(executor))
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(2), s"Expected slide 2 to be visited via setSlide, got: $changes")
    }
  }

  test("setSlide clamps to valid range (negative index)") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("A"), out = None),
          SlideSpecification(slide = textSlide("B"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- executor.setSlide(-5) // Should clamp to 0
      _ <- harness.runWithTicking(executor.start(), ticks = 10)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), s"Negative index should clamp to 0, got: $changes")
    }
  }

  test("setSlide clamps to valid range (index beyond last)") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("A"), out = None),
          SlideSpecification(slide = textSlide("B"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- executor.setSlide(100) // Should clamp to 1 (last index)
      _ <- harness.runWithTicking(executor.start(), ticks = 10)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(1), s"Large index should clamp to last, got: $changes")
    }
  }

  // --- Transition playback ---

  test("transition is played when navigating right with an out transition") {
    for {
      transitionPlayed <- IO.ref(false)
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      transition = new Transition[IO] {
        override def transition(from: Slide[IO], to: Slide[IO]): IO[Unit] =
          transitionPlayed.set(true)
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("From"), out = Some(transition)),
          SlideSpecification(slide = textSlide("To"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      played <- transitionPlayed.get
    } yield {
      assert(played, "Transition should have been played when navigating right")
    }
  }

  test("no transition is played when out is None") {
    for {
      transitionPlayed <- IO.ref(false)
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("From"), out = None),
          SlideSpecification(slide = textSlide("To"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      played <- transitionPlayed.get
    } yield {
      assert(!played, "No transition should be played when out is None")
    }
  }

  // --- Screen clearing ---

  test("screen is cleared on start and on each navigation") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("A"), out = None),
          SlideSpecification(slide = textSlide("B"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      clears <- harness.clearCount
    } yield {
      // At least: 1 (start) + 1 (right nav) + 1 (esc exit) = 3
      assert(clears >= 3, s"Expected at least 3 clears, got $clears")
    }
  }

  // --- Single slide presentation ---

  test("single slide presentation handles all navigation gracefully") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right), // no-op at boundary
          Key(SpecialKey.Left),  // no-op at boundary
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("Solo"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 30)
      changes <- slideChanges.get
    } yield {
      // All visits should be to slide 0
      assert(changes.forall(_ == 0), s"All visits should be to slide 0, got: $changes")
    }
  }

  // --- MouseClick input delegation ---

  test("MouseClick is forwarded to current slide") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(MouseClick(5, 3), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Clickable"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          receivedInputs.update(_ :+ input)
      }
      presentation = Presentation[IO](
        slideSpecifications = List(SlideSpecification(slide = slide, out = None))
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(MouseClick(5, 3)))
    }
  }

  // --- Rapid navigation ---

  test("rapid right-left-right navigation maintains correct slide index") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = defaultScreen,
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Left),
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Esc)
        ),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(slide = textSlide("A"), out = None),
          SlideSpecification(slide = textSlide("B"), out = None),
          SlideSpecification(slide = textSlide("C"), out = None)
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(_ :+ idx)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 50)
      changes <- slideChanges.get
    } yield {
      // Expected path: 0 -> 1 -> 0 -> 1 -> 2
      assertEquals(changes, List(0, 1, 0, 1, 2))
    }
  }
}



