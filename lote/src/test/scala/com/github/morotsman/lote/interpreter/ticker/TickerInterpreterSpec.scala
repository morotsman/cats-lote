package com.github.morotsman.lote.interpreter.ticker

import cats.effect.IO
import cats.effect.kernel.Ref
import munit.CatsEffectSuite

import scala.concurrent.duration._

class TickerInterpreterSpec extends CatsEffectSuite {

  test("subscribe and start invokes callback periodically") {
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

  test("stop cancels the tick loop") {
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

  test("multiple subscribers all receive ticks") {
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
      assertEquals(c1, c2)
    }
  }

  test("cancel subscription stops ticks for that subscriber") {
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
      assertEquals(c1AtCancel, c1Final)
      assert(
        c2Final > c1Final,
        s"Expected counter2 ($c2Final) > counter1 ($c1Final)"
      )
    }
  }

  test("start is idempotent - calling multiple times has no extra effect") {
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

  test("ticker with no subscribers does not fail") {
    for {
      ticker <- TickerInterpreter.make[IO](interval = 20.millis)
      _ <- ticker.start
      _ <- IO.sleep(60.millis)
      _ <- ticker.stop
    } yield ()
  }

  test("stop without start does not fail") {
    for {
      ticker <- TickerInterpreter.make[IO](interval = 20.millis)
      _ <- ticker.stop
    } yield ()
  }
}
