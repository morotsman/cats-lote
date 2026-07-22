package com.github.morotsman.lote.internal.interpreter.ticker

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TickerInterpreterSpec extends CatsEffectSuite {

  private def withTimeControl[A](io: IO[A]): IO[A] =
    TestControl.executeEmbed(io)

  test("subscribe and start invokes callback periodically") {
    withTimeControl {
      for {
        counter <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.subscribe(counter.update(_ + 1))
        _ <- ticker.start
        _ <- IO.sleep(120.millis)
        _ <- ticker.stop
        count <- counter.get
      } yield {
        assert(count >= 3, s"Expected at least 3 ticks, got $count")
      }
    }
  }

  test("stop cancels the tick loop") {
    withTimeControl {
      for {
        counter <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.subscribe(counter.update(_ + 1))
        _ <- ticker.start
        _ <- IO.sleep(80.millis)
        _ <- ticker.stop
        countAtStop <- counter.get
        _ <- IO.sleep(80.millis)
        countAfterStop <- counter.get
      } yield {
        assertEquals(countAtStop, countAfterStop)
      }
    }
  }

  test("multiple subscribers all receive ticks") {
    withTimeControl {
      for {
        counter1 <- Ref[IO].of(0)
        counter2 <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.subscribe(counter1.update(_ + 1))
        _ <- ticker.subscribe(counter2.update(_ + 1))
        _ <- ticker.start
        _ <- IO.sleep(100.millis)
        _ <- ticker.stop
        c1 <- counter1.get
        c2 <- counter2.get
      } yield {
        assert(c1 >= 2, s"Subscriber 1 expected >= 2 ticks, got $c1")
        assert(c2 >= 2, s"Subscriber 2 expected >= 2 ticks, got $c2")
        // Subscribers may differ by at most 1 tick due to cancellation during parallel dispatch
        assert(Math.abs(c1 - c2) <= 1, s"Expected subscriber counts within 1, got c1=$c1 c2=$c2")
      }
    }
  }

  test("cancel subscription stops ticks for that subscriber") {
    withTimeControl {
      for {
        counter1 <- Ref[IO].of(0)
        counter2 <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        sub1 <- ticker.subscribe(counter1.update(_ + 1))
        _ <- ticker.subscribe(counter2.update(_ + 1))
        _ <- ticker.start
        _ <- IO.sleep(60.millis)
        _ <- sub1.cancel
        c1AtCancel <- counter1.get
        _ <- IO.sleep(60.millis)
        _ <- ticker.stop
        c1Final <- counter1.get
        c2Final <- counter2.get
      } yield {
        // After cancel, counter1 should not increase (allow +1 for a tick that was in-flight during cancel)
        assert(
          c1Final - c1AtCancel <= 1,
          s"Expected counter1 to stop after cancel, got before=$c1AtCancel after=$c1Final"
        )
        assert(
          c2Final > c1Final,
          s"Expected counter2 ($c2Final) > counter1 ($c1Final)"
        )
      }
    }
  }

  test("start is idempotent - calling multiple times has no extra effect") {
    withTimeControl {
      for {
        counter <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.subscribe(counter.update(_ + 1))
        _ <- ticker.start
        _ <- ticker.start
        _ <- ticker.start
        _ <- IO.sleep(100.millis)
        _ <- ticker.stop
        count <- counter.get
      } yield {
        assert(
          count <= 8,
          s"Expected at most ~8 ticks (no double-ticking), got $count"
        )
        assert(count >= 2, s"Expected at least 2 ticks, got $count")
      }
    }
  }

  test("ticker with no subscribers does not fail") {
    withTimeControl {
      for {
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.start
        _ <- IO.sleep(60.millis)
        _ <- ticker.stop
      } yield ()
    }
  }

  test("stop without start does not fail") {
    withTimeControl {
      for {
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.stop
      } yield ()
    }
  }

  test("a failing subscriber does not stall other subscribers") {
    withTimeControl {
      for {
        counter <- Ref[IO].of(0)
        ticker <- TickerInterpreter.make[IO](interval = 20.millis)
        _ <- ticker.subscribe(IO.raiseError(new RuntimeException("boom")))
        _ <- ticker.subscribe(counter.update(_ + 1))
        _ <- ticker.start
        _ <- IO.sleep(100.millis)
        _ <- ticker.stop
        count <- counter.get
      } yield {
        assert(count >= 2, s"Expected at least 2 ticks despite failing subscriber, got $count")
      }
    }
  }
}
