package com.github.morotsman.lote.interpreter

import cats.effect.IO
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Key, Screen, ScreenAdjusted, SpecialKey, UserInput, VerticalAlignment}
import com.github.morotsman.lote.api.spi.{NConsole, Slide}
import com.github.morotsman.lote.internal.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.internal.TextSlide
import com.github.morotsman.lote.internal.model.{Presentation, SlideSpecification}
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class PresentationExecutorSpec extends CatsEffectSuite {

  test("PresentationExecutor starts and shows first slide") {
    def waitForWrite(console: TestNConsole, attemptsLeft: Int): IO[Unit] =
      console.writtenRef.get.flatMap { written =>
        if (written.nonEmpty) IO.unit
        else if (attemptsLeft <= 0) IO.raiseError(new RuntimeException("Timed out waiting for first slide render"))
        else IO.sleep(10.millis) *> waitForWrite(console, attemptsLeft - 1)
      }

    for {
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = Nil
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      runFiber <- executor.start().start
      _ <- waitForWrite(console, attemptsLeft = 50)
      _ <- console.inputsRef.set(List(Key(SpecialKey.Esc)))
      _ <- runFiber.joinWithNever
      written <- console.writtenRef.get
      cleared <- console.clearedRef.get
    } yield {
      assert(written.nonEmpty, "Expected content to be written")
      assert(cleared > 0, "Expected screen to be cleared at least once")
    }
  }

  test("PresentationExecutor navigates right") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      _ <- executor.start()
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), "Should have visited slide 0")
      assert(changes.contains(1), "Should have visited slide 1")
    }
  }

  test("PresentationExecutor navigates left") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Left), Key(SpecialKey.Esc))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      _ <- executor.start()
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
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(
          Key(SpecialKey.Right),
          Key(SpecialKey.Right),
          Key(SpecialKey.Esc)
        )
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      _ <- executor.start()
      written <- console.writtenRef.get
    } yield {
      assert(written.forall(_.contains("Only Slide")))
    }
  }

  test("PresentationExecutor forwards Space special key to the current slide") {
    for {
      receivedInputs <- IO.ref(List.empty[UserInput])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Space), Key(SpecialKey.Esc))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted("Slide 1"))

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
      _ <- executor.start()
      inputs <- receivedInputs.get
    } yield {
      assertEquals(inputs, List(Key(SpecialKey.Space)))
    }
  }

  test("PresentationExecutor calls onSlideChange callback") {
    for {
      slideChanges <- IO.ref(List.empty[Int])
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc))
      )
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      _ <- executor.start()
      changes <- slideChanges.get
    } yield {
      assert(changes.contains(0), "Should have notified slide 0")
      assert(changes.contains(1), "Should have notified slide 1")
    }
  }
}
