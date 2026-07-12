package com.github.morotsman.lote.internal

import cats.effect.IO
import com.github.morotsman.lote.api.{Alignment, Character, HorizontalAlignment, Screen, VerticalAlignment}
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

class TextSlideSpec extends CatsEffectSuite {

  test("TextSlide.content returns aligned content") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = TextSlide[IO](
        "Hello",
        Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
      )
      content <- slide.content
    } yield {
      assert(content.content.contains("Hello"))
    }
  }

  test("TextSlide.startShow writes content to console") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = TextSlide[IO](
        "Hello",
        Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
      )
      _ <- slide.startShow
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
      assert(written.head.contains("Hello"))
    }
  }

  test("TextSlide.stopShow does nothing (unit)") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = TextSlide[IO](
        "Hello",
        Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
      )
      _ <- slide.stopShow
    } yield ()
  }

  test("TextSlide.userInput does nothing (unit)") {
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = TextSlide[IO](
        "Hello",
        Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
      )
      _ <- slide.userInput(Character('x'))
    } yield ()
  }

  test("ToTextSlide implicit converts string to Slide") {
    import com.github.morotsman.lote.internal.TextSlide.ToTextSlide
    for {
      console <- TestNConsole.make(screen = Screen(20, 5))
      implicit0(nc: NConsole[IO]) = console: NConsole[IO]
      slide = "Test content".toSlide[IO]()
      content <- slide.content
    } yield {
      assert(content.content.contains("Test content"))
    }
  }
}
