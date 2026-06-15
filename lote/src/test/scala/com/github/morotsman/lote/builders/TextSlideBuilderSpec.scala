package com.github.morotsman.lote.builders

import cats.effect.IO
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class TextSlideBuilderSpec extends CatsEffectSuite {

  private def makeTransition(): Transition[IO] = new Transition[IO] {
    override def transition(from: Slide[IO], to: Slide[IO]): IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("TextSlideBuilder.content sets slide content") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Hello World")
        .build()
      content <- spec.slide.content
    } yield {
      assert(content.content.contains("Hello World"))
    }
  }

  test("TextSlideBuilder uses Center/Center alignment by default") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("X")
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      // Vertically centered: (5-1)/2 = 2
      val centerLine = lines(2)
      assert(centerLine.contains("X"))
      // Horizontally centered: (20-1)/2 = 9
      assertEquals(centerLine.indexOf("X"), 9)
    }
  }

  test("TextSlideBuilder.alignment overrides default alignment") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("AB")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      assert(lines(0).startsWith("AB"))
    }
  }

  test("TextSlideBuilder.alignment with Down/Right") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Z")
        .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      val lastLine = lines(4)
      assert(lastLine.endsWith("Z"))
    }
  }

  test("TextSlideBuilder.transition sets the out transition") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      transition = makeTransition()
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .transition(transition)
        .build()
    } yield {
      assertEquals(spec.out, Some(transition))
    }
  }

  test("TextSlideBuilder without transition has out = None") {
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

  test("TextSlideBuilder.transition with null sets out to None") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .transition(null)
        .build()
    } yield {
      assertEquals(spec.out, None)
    }
  }

  test("TextSlideBuilder handles multiline content") {
    for {
      console <- TestNConsole.make(screen = Screen(30, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Line 1\nLine 2\nLine 3")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      assert(lines(0).startsWith("Line 1"))
      assert(lines(1).startsWith("Line 2"))
      assert(lines(2).startsWith("Line 3"))
    }
  }

  test("TextSlideBuilder chaining - alignment after content") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Hi")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
        .build()
      content <- spec.slide.content
    } yield {
      val lines = content.content.split("\n", -1)
      // "Hi" right-aligned in 20 chars: starts at index 18
      assert(lines(0).endsWith("Hi"))
      assertEquals(lines(0).indexOf("Hi"), 18)
    }
  }

  test("TextSlideBuilder chaining - transition after alignment") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      transition = makeTransition()
      spec = TextSlideBuilder[IO]()
        .content("Hi")
        .alignment(
          Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
        )
        .transition(transition)
        .build()
      content <- spec.slide.content
    } yield {
      assert(content.content.contains("Hi"))
      assertEquals(spec.out, Some(transition))
    }
  }
}
