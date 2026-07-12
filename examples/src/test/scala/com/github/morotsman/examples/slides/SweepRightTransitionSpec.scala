package com.github.morotsman.examples.slides

import cats.implicits._
import cats.effect.{IO, Ref}
import com.github.morotsman.lote.api.{AnimationSettings, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.{Slide, Ticker, TickerSubscription}
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._

class SweepRightTransitionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 10.seconds

  private def makeTicker(interval: FiniteDuration): IO[Ticker[IO]] =
    Ref[IO].of((Vector.empty[(Long, IO[Unit])], 0L, false)).map { stateRef =>
      new Ticker[IO] {
        private def tickLoop(): IO[Unit] =
          IO.sleep(interval) *> stateRef.get.flatMap { case (subscribers, _, running) =>
            subscribers.toList.traverse_(_._2) *> (if (running) tickLoop() else IO.unit)
          }

        override def subscribe(callback: IO[Unit]): IO[TickerSubscription[IO]] =
          stateRef.modify { case (subscribers, nextId, running) =>
            ((subscribers :+ (nextId -> callback), nextId + 1, running), nextId)
          }.map { id =>
            new TickerSubscription[IO] {
              override def cancel: IO[Unit] =
                stateRef.update { case (subscribers, nextId, running) =>
                  (subscribers.filterNot(_._1 == id), nextId, running)
                }
            }
          }

        override def start: IO[Unit] =
          stateRef.get.flatMap { case (_, _, running) =>
            if (running) IO.unit
            else stateRef.update { case (subscribers, nextId, _) =>
              (subscribers, nextId, true)
            } *> tickLoop().start.void
          }

        override def stop: IO[Unit] =
          stateRef.update { case (subscribers, nextId, _) =>
            (subscribers, nextId, false)
          }
      }
    }

  private def fixedSlide(text: String): Slide[IO] = new Slide[IO] {
    override def content: IO[ScreenAdjusted] = IO.pure(ScreenAdjusted(text))
    override def startShow: IO[Unit] = IO.unit
    override def stopShow: IO[Unit] = IO.unit
    override def userInput(input: UserInput): IO[Unit] = IO.unit
  }

  test("SweepRightTransition completes and shows target slide") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      ticker <- makeTicker(5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition.create[IO](columnsPerStep = 2, console, ticker, animationSettings)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      assert(written.nonEmpty)
      assertEquals(written.head, "BBBB")
    }
  }

  test("SweepRightTransition renders intermediate sweep frames") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      ticker <- makeTicker(5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition.create[IO](columnsPerStep = 2, console, ticker, animationSettings)
      _ <- transition.transition(from, to)
      written <- console.writtenRef.get
    } yield {
      val intermediateFrames = written.drop(1)
      assert(
        intermediateFrames.contains("BBAA"),
        s"Expected an intermediate sweep frame 'BBAA', got: $intermediateFrames"
      )
    }
  }

  test("SweepRightTransition clears the screen while animating") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      ticker <- makeTicker(5.millis)
      animationSettings = AnimationSettings(5.millis)
      from = fixedSlide("AAAA")
      to = fixedSlide("BBBB")
      transition = SweepRightTransition.create[IO](columnsPerStep = 2, console, ticker, animationSettings)
      _ <- transition.transition(from, to)
      cleared <- console.clearedRef.get
    } yield {
      assert(cleared >= 2, s"Expected at least two clear() calls, got $cleared")
    }
  }

  test("SweepRightTransition userInput is a no-op") {
    for {
      console <- TestNConsole.make(screen = Screen(4, 1))
      ticker <- makeTicker(5.millis)
      animationSettings = AnimationSettings(5.millis)
      transition = SweepRightTransition.create[IO](columnsPerStep = 2, console, ticker, animationSettings)
      _ <- transition.userInput(Key(SpecialKey.Right))
    } yield ()
  }
}


