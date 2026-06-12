package com.github.morotsman.lote.interpreter

import cats.effect.IO
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class PresentationExecutorSpec extends CatsEffectSuite {

  test("PresentationExecutor starts and shows first slide") {
    for {
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Esc))
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
        ),
        exitSlide = None
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- executor.start()
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
        ),
        exitSlide = None
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
        ),
        exitSlide = None
      )
      executor <- PresentationExecutorInterpreter.make[IO](
        presentation,
        onSlideChange = (idx: Int) => slideChanges.update(idx :: _)
      )
      _ <- executor.start()
      changes <- slideChanges.get
    } yield {
      val slide0Count = changes.count(_ == 0)
      assert(slide0Count >= 2, s"Expected slide 0 visited at least twice, got changes: ${changes.reverse}")
    }
  }

  test("PresentationExecutor does not go past last slide on Right") {
    for {
      console <- TestNConsole.make(
        screen = Screen(20, 5),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Right), Key(SpecialKey.Esc))
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
        ),
        exitSlide = None
      )
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- executor.start()
      written <- console.writtenRef.get
    } yield {
      assert(written.forall(_.contains("Only Slide")))
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
        ),
        exitSlide = None
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

