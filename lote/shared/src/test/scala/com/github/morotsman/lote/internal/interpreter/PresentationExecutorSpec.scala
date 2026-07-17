package com.github.morotsman.lote.internal.interpreter

import cats.effect.IO
import com.github.morotsman.lote.api.spi.{NConsole, Slide}
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.internal.TextSlide
import com.github.morotsman.lote.internal.model.{Presentation, SlideSpecification}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class PresentationExecutorSpec extends CatsEffectSuite {

  test("PresentationExecutor starts and shows first slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 1",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      written <- harness.writtenFrames
      cleared <- harness.clearCount
    } yield {
      assert(written.nonEmpty, "Expected content to be written")
      assert(cleared > 0, "Expected screen to be cleared at least once")
    }
  }

  test("PresentationExecutor navigates right") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 1",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          ),
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 2",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(idx :: _)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), "Should have visited slide 0")
      assert(changes.contains(1), "Should have visited slide 1")
    }
  }

  test("PresentationExecutor navigates left") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Left), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 1",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          ),
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 2",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(idx :: _)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 30)
      changes <- slideChanges.get
    } yield {
      val slide0Count = changes.count(_ == 0)
      assert(
        slide0Count >= 2,
        s"Expected slide 0 visited at least twice, got changes: ${changes.reverse}"
      )
    }
  }

  test("PresentationExecutor does not go past last slide on Right") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
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
          SlideSpecification(
            slide = TextSlide[IO](
              "Only Slide",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      written <- harness.writtenFrames
    } yield {
      assert(written.forall(_.contains("Only Slide")))
    }
  }

  test("PresentationExecutor forwards Space special key to the current slide") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Space), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      slide = new Slide[IO] {
        override def content: IO[Option[ScreenAdjusted]] = IO.pure(Some(ScreenAdjusted("Slide 1")))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] =
          receivedInputs.update(_ :+ input)
      }
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(
            slide = slide,
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(Key(SpecialKey.Space)))
    }
  }

  test("PresentationExecutor calls onSlideChange callback") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      harness <- SlideTestHarness.make[IO](
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = 1.millis
      )
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      presentation = Presentation[IO](
        slideSpecifications = List(
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 1",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          ),
          SlideSpecification(
            slide = TextSlide[IO](
              "Slide 2",
              Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
            ),
            out = None
          )
        )
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(idx :: _)
      )
      _ <- harness.runWithTicking(executor.start(), ticks = 20)
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), "Should have notified slide 0")
      assert(changes.contains(1), "Should have notified slide 1")
    }
  }
}
