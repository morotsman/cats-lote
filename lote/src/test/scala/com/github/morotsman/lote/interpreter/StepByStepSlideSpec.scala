package com.github.morotsman.lote.interpreter

import cats.effect.IO
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class StepByStepSlideSpec extends CatsEffectSuite {

  test("StepByStepSlide.startShow writes the first stage with the any-key hint") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide <- StepByStepSlide.make[IO](Vector("First", "Second"))
      _ <- slide.startShow
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.length, 1)
      assert(written.head.contains("First"))
      assert(written.head.contains("[press any key to continue]"))
    }
  }

  test("StepByStepSlide advances on character input") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide <- StepByStepSlide.make[IO](Vector("First", "Second"))
      _ <- slide.startShow
      _ <- slide.userInput(Character('x'))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.length, 2)
      assert(written.head.contains("Second"))
      assert(!written.head.contains("[press any key to continue]"))
    }
  }

  test("StepByStepSlide advances on special key input") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide <- StepByStepSlide.make[IO](Vector("First", "Second"))
      _ <- slide.startShow
      _ <- slide.userInput(Key(SpecialKey.Enter))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.length, 2)
      assert(written.head.contains("Second"))
    }
  }

  test("StepByStepSlide ignores non-key input") {
    for {
      console <- TestNConsole.make(screen = Screen(40, 10))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide <- StepByStepSlide.make[IO](Vector("First", "Second"))
      _ <- slide.startShow
      _ <- slide.userInput(MouseClick(1, 1))
      _ <- slide.userInput(Key(SpecialKey.Timeout))
      written <- console.writtenRef.get
    } yield {
      assertEquals(written.length, 1)
      assert(written.head.contains("First"))
      assert(written.head.contains("[press any key to continue]"))
    }
  }
}

