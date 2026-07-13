package com.github.morotsman.lote.api.builders

import cats.effect.IO
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Screen, ScreenAdjusted, UserInput, VerticalAlignment}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Transition}
import com.github.morotsman.lote.internal.builders.{SlideBuilder, TextSlideBuilder}
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

class BuilderSpec extends CatsEffectSuite {

  test("TextSlideBuilder builds slide with custom alignment") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      assert(lines(0).startsWith("Test"))
    }
  }

  test("TextSlideBuilder uses Center/Center alignment by default") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Hi")
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      assert(lines(2).contains("Hi"))
    }
  }

  test("SlideBuilder builds slide specification with transition") {
    for {
      _ <- IO.unit
      slide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("content"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      transition = new Transition[IO] {
        override def transition(from: Slide[IO], to: Slide[IO]): IO[Unit] =
          IO.unit
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      spec = SlideBuilder[IO]()
        .addSlide(slide)
        .transition(transition)
        .build()
    } yield {
      assert(spec.out.isDefined)
      assertEquals(spec.slide, slide)
    }
  }

  test("TextSlideBuilder with no transition results in None") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .build()
    } yield {
      assertEquals(spec.out, None)
    }
  }
}
