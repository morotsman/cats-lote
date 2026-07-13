package com.github.morotsman.lote.api.builders

import cats.effect.IO
import com.github.morotsman.lote.api.{Alignment, AnimationSettings, Character, HorizontalAlignment, Screen, UserInput, VerticalAlignment}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.internal.builders.TextSlideBuilder
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

class TextSlideBuilderSpec extends CatsEffectSuite {

  private def makeTransition(): Transition[IO] = new Transition[IO] {
    override def transition(from: Slide[IO], to: Slide[IO]): IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("TextSlideBuilder.content sets slide content") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .build()
    } yield {
      assertEquals(spec.out, None)
    }
  }

  test("TextSlideBuilder built-in transition helpers set transitions") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      implicit0(ticker: Ticker[IO]) = harness.ticker: Ticker[IO]
      implicit0(as: AnimationSettings) = harness.animationSettings
      morphSpec = TextSlideBuilder[IO]()
        .content("Morph")
        .morphTransition()
        .build()
      replaceSpec = TextSlideBuilder[IO]()
        .content("Replace")
        .replaceTransition('.')
        .build()
      fallingSpec = TextSlideBuilder[IO]()
        .content("Falling")
        .fallingCharactersTransition(gravity = 1.5, selectAccelerator = 1.3)
        .build()
      grabSpec = TextSlideBuilder[IO]()
        .content("Grab")
        .grabTransition(stepSize = 4)
        .build()
    } yield {
      assert(morphSpec.out.isDefined)
      assert(replaceSpec.out.isDefined)
      assert(fallingSpec.out.isDefined)
      assert(grabSpec.out.isDefined)
    }
  }

  test("TextSlideBuilder.title sets the slide title") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Test")
        .title("Intro")
        .build()
    } yield {
      assertEquals(spec.title, Some("Intro"))
    }
  }

  test("TextSlideBuilder handles multiline content") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(30, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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
      harness <- SlideTestHarness.make[IO](screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
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

  test("TextSlideBuilder.step builds a staged text slide with the default hint") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(30, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Agenda")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .step("First")
        .step("Second")
        .build()
      _ <- spec.slide.startShow
      _ <- spec.slide.userInput(Character('x'))
      written <- harness.writtenFrames
    } yield {
      assertEquals(written.length, 2)
      assert(written(1).contains("Agenda"))
      assert(written(1).contains("[press any key to continue]"))
      val lines = written.head.split("\n", -1)
      assert(lines(0).startsWith("Agenda"))
      assert(lines(1).startsWith("First"))
      assert(written.head.contains("[press any key to continue]"))
    }
  }

  test("TextSlideBuilder.separator controls how steps are joined") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(30, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Intro")
        .separator("\n\n")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .step("Details")
        .build()
      _ <- spec.slide.startShow
      _ <- spec.slide.userInput(Character('x'))
      written <- harness.writtenFrames
    } yield {
      val lines = written.head.split("\n", -1)
      assert(lines(0).startsWith("Intro"))
      assert(lines(1).trim.isEmpty)
      assert(lines(2).startsWith("Details"))
    }
  }

  test("TextSlideBuilder.hint overrides the default continue hint and disappears on the final step") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(30, 10))
      implicit0(nc: NConsole[IO]) = harness.console: NConsole[IO]
      spec = TextSlideBuilder[IO]()
        .content("Topic")
        .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        .hint("[next]")
        .step("Point")
        .build()
      _ <- spec.slide.startShow
      _ <- spec.slide.userInput(Character('x'))
      written <- harness.writtenFrames
    } yield {
      assert(written(1).contains("[next]"))
      assert(!written.head.contains("[next]"))
      val lines = written.head.split("\n", -1)
      assert(lines(0).startsWith("Topic"))
      assert(lines(1).startsWith("Point"))
    }
  }
}
