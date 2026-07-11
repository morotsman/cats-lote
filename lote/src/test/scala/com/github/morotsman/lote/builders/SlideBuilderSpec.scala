package com.github.morotsman.lote.builders

import cats.effect.IO
import com.github.morotsman.lote.algebra.{Slide, Transition}
import com.github.morotsman.lote.model._
import munit.CatsEffectSuite

class SlideBuilderSpec extends CatsEffectSuite {

  private def makeSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  private def makeTransition(): Transition[IO] = new Transition[IO] {
    override def transition(from: Slide[IO], to: Slide[IO]): IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("SlideBuilder.addSlide sets the slide") {
    val slide = makeSlide("hello")
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .build()

    assertEquals(spec.slide, slide)
  }

  test("SlideBuilder without transition has out = None") {
    val slide = makeSlide("hello")
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .build()

    assertEquals(spec.out, None)
  }

  test("SlideBuilder.transition sets the out transition") {
    val slide = makeSlide("hello")
    val transition = makeTransition()
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .transition(transition)
      .build()

    assertEquals(spec.out, Some(transition))
  }

  test("SlideBuilder.title sets the slide title") {
    val slide = makeSlide("hello")
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .title("Intro")
      .build()

    assertEquals(spec.title, Some("Intro"))
  }

  test("SlideBuilder preserves slide content through build") {
    for {
      slide <- IO.pure(makeSlide("my content"))
      spec = SlideBuilder[IO]()
        .addSlide(slide)
        .build()
      content <- spec.slide.content
    } yield {
      assertEquals(content.content, "my content")
    }
  }

  test("SlideBuilder last transition call wins") {
    val slide = makeSlide("hello")
    val transition1 = makeTransition()
    val transition2 = makeTransition()
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .transition(transition1)
      .transition(transition2)
      .build()

    assertEquals(spec.out, Some(transition2))
  }

  test("SlideBuilder produces a valid SlideSpecification") {
    val slide = makeSlide("spec test")
    val transition = makeTransition()
    val spec = SlideBuilder[IO]()
      .addSlide(slide)
      .transition(transition)
      .build()

    assert(spec.isInstanceOf[SlideSpecification[IO]])
    assertEquals(spec.slide, slide)
    assertEquals(spec.out, Some(transition))
  }
}
