package com.github.morotsman.lote.builders

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class BuilderSpec extends CatsEffectSuite {

  test("PresentationBuilder builds a presentation with slides") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      presentation = PresentationBuilder[IO]()
        .addTextSlide(_.content("Slide 1"))
        .addTextSlide(_.content("Slide 2"))
        .build()
    } yield {
      assertEquals(presentation.slideSpecifications.length, 2)
      assertEquals(presentation.exitSlide, None)
    }
  }

  test("PresentationBuilder preserves slide order") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      presentation = PresentationBuilder[IO]()
        .addTextSlide(_.content("First"))
        .addTextSlide(_.content("Second"))
        .addTextSlide(_.content("Third"))
        .build()
    } yield {
      assertEquals(presentation.slideSpecifications.length, 3)
    }
  }

  test("PresentationBuilder supports exit slide") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      exitSlide = new Slide[IO] {
        override def content: IO[ScreenAdjusted] =
          IO.pure(ScreenAdjusted("Bye"))
        override def startShow: IO[Unit] = IO.unit
        override def stopShow: IO[Unit] = IO.unit
        override def userInput(input: UserInput): IO[Unit] = IO.unit
      }
      presentation = PresentationBuilder[IO]()
        .addTextSlide(_.content("Slide 1"))
        .addExitSlide(exitSlide)
        .build()
    } yield {
      assert(presentation.exitSlide.isDefined)
    }
  }

  test("TextSlideBuilder builds slide with custom alignment") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
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
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .build()
    } yield {
      assertEquals(spec.out, None)
    }
  }
}
