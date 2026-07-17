package com.github.morotsman.lote.internal.interpreter.transition

import cats.effect.IO
import com.github.morotsman.lote.api.{Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite

import scala.concurrent.duration._

class GrabTransitionSpec extends CatsEffectSuite {

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[Option[ScreenAdjusted]] = IO.pure(Some(ScreenAdjusted(text)))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  // Create content that fits a small screen with enough rows for the snake (9 rows)
  private def smallScreenContent(width: Int): String = {
    val line = "Hello World" + " " * (width - 11)
    (0 until 12)
      .map { i =>
        if (i == 0) line.take(width)
        else " " * width
      }
      .mkString("\n")
  }

  test("GrabTransition completes and shows target slide") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      fromContent = smallScreenContent(40)
      toContent = "Goodbye" + " " * 33 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 200)
      written <- harness.writtenFrames
    } yield {
      assert(written.nonEmpty)
      assert(
        written.head.contains("Goodbye"),
        s"Expected final content to contain 'Goodbye', got: '${written.head.take(40)}'"
      )
    }
  }

  test("GrabTransition writes multiple frames during animation") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 200)
      written <- harness.writtenFrames
    } yield {
      // Should have many frames: crawl in + bite + drag out + final
      assert(
        written.length >= 3,
        s"Expected at least 3 writes, got ${written.length}"
      )
    }
  }

  test("GrabTransition clears screen during transition") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 10, harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 200)
      cleared <- harness.clearCount
    } yield {
      assert(cleared >= 2, s"Expected at least 2 clears, got $cleared")
    }
  }

  test("GrabTransition userInput is a no-op") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      transition = GrabTransition.create[IO](stepSize = 10, harness.console, harness.ticker, harness.animationSettings)
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }

  test("GrabTransition snake art appears in intermediate frames") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(60, 12), tickStep = 5.millis)
      fromContent = smallScreenContent(60)
      toContent = " " * 60 + ("\n" + " " * 60) * 11
      from = fixedSlide(fromContent)
      to = fixedSlide(toContent)
      transition = GrabTransition.create[IO](stepSize = 5, harness.console, harness.ticker, harness.animationSettings)
      _ <- harness.runWithTicking(transition.transition(from, to), ticks = 300)
      written <- harness.writtenFrames
    } yield {
      // Snake art contains characters like /^\/^\ and _|__|
      val allFrames = written.mkString("")
      assert(
        allFrames.contains("/^"),
        s"Expected snake art characters in frames"
      )
    }
  }

  test("GrabTransition with larger stepSize completes with fewer frames") {
    for {
      harness1 <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      fromContent = smallScreenContent(40)
      toContent = " " * 40 + ("\n" + " " * 40) * 11
      from1 = fixedSlide(fromContent)
      to1 = fixedSlide(toContent)
      transition1 = GrabTransition
        .create[IO](stepSize = 20, harness1.console, harness1.ticker, harness1.animationSettings)
      _ <- harness1.runWithTicking(transition1.transition(from1, to1), ticks = 500)
      written1 <- harness1.writtenFrames

      harness2 <- SlideTestHarness.make[IO](screen = Screen(40, 12), tickStep = 5.millis)
      from2 = fixedSlide(fromContent)
      to2 = fixedSlide(toContent)
      transition2 = GrabTransition
        .create[IO](stepSize = 2, harness2.console, harness2.ticker, harness2.animationSettings)
      _ <- harness2.runWithTicking(transition2.transition(from2, to2), ticks = 500)
      written2 <- harness2.writtenFrames
    } yield {
      assert(
        written1.length <= written2.length,
        s"Expected larger step (${written1.length} frames) <= smaller step (${written2.length} frames)"
      )
    }
  }
}
