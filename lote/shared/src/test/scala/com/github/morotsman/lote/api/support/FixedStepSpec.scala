package com.github.morotsman.lote.api.support

import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.testkit.SimulatedClock
import munit.CatsEffectSuite

import scala.concurrent.duration._

class FixedStepSpec extends CatsEffectSuite {

  private def mkEnv(startTime: FiniteDuration = 0.nanos): IO[(SimulatedClock[IO], Ref[IO, FixedStepState])] =
    for {
      clock <- SimulatedClock.make[IO](startTime)
      ref <- FixedStep.makeRef[IO](implicitly, implicitly, clock)
    } yield (clock, ref)

  // ── Basic step consumption ──

  test("consumeSteps — zero elapsed yields zero steps and zero progress") {
    for {
      (clock, ref) <- mkEnv()
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 0)
      assertEqualsDouble(result._2, 0.0, 1e-9)
    }
  }

  test("consumeSteps — exactly one step duration yields 1 step") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(40.millis)
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 1)
      assertEqualsDouble(result._2, 0.0, 1e-9)
    }
  }

  test("consumeSteps — two step durations yield 2 steps") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(80.millis)
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 2)
      assertEqualsDouble(result._2, 0.0, 1e-9)
    }
  }

  test("consumeSteps — fractional progress between steps") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(60.millis) // 1.5 steps at 40ms step
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 1)
      assertEqualsDouble(result._2, 0.5, 1e-9)
    }
  }

  test("consumeSteps — remainder accumulates across calls") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(30.millis) // 0.75 steps
      result1 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
      _ <- clock.advance(30.millis) // 30ms + 30ms remainder = 60ms total accumulated
      result2 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result1._1, 0) // not enough for a step
      assertEquals(result2._1, 1) // remainder from first call + 30ms = 60ms = 1 step + 20ms
      assertEqualsDouble(result2._2, 0.5, 1e-9) // 20ms / 40ms = 0.5
    }
  }

  // ── Burst-fire scenario (tab backgrounded) ──

  test("consumeSteps — large elapsed time produces many steps (burst-fire)") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(1.second) // 1000ms / 40ms = 25 steps
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 25)
      assertEqualsDouble(result._2, 0.0, 1e-9)
    }
  }

  test("consumeSteps — very large elapsed time (10 seconds)") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(10.seconds)
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 250) // 10000ms / 40ms
    }
  }

  // ── Edge cases ──

  test("consumeSteps — sub-millisecond elapsed time yields zero steps with small progress") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(1.milli) // much less than 40ms step
      result <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(result._1, 0)
      assertEqualsDouble(result._2, 0.025, 1e-9) // 1/40
    }
  }

  test("consumeSteps — progress is always in [0, 1)") {
    for {
      (clock, ref) <- mkEnv()
      results <- (1 to 100).toList.traverse { _ =>
        clock.advance(7.millis) *>
          FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
      }
    } yield {
      results.foreach { case (_, progress) =>
        assert(progress >= 0.0, s"progress $progress < 0")
        assert(progress < 1.0 + 1e-9, s"progress $progress >= 1")
      }
    }
  }

  // ── Reset ──

  test("reset — clears accumulated time") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(30.millis) // accumulate some time
      r1 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
      _ <- FixedStep.reset(ref)(implicitly, clock)
      r2 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(r1._1, 0)
      assert(r1._2 > 0.0) // had some progress before reset
      assertEquals(r2._1, 0) // reset cleared it
      assertEqualsDouble(r2._2, 0.0, 1e-9)
    }
  }

  // ── Multiple sequential consumptions ──

  test("consumeSteps — sequential calls correctly track state") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(100.millis) // 2 steps + 20ms remainder
      r1 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
      _ <- clock.advance(50.millis) // 20ms + 50ms = 70ms = 1 step + 30ms
      r2 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
      _ <- clock.advance(10.millis) // 30ms + 10ms = 40ms = 1 step
      r3 <- FixedStep.consumeSteps(ref, 40.millis)(implicitly, clock)
    } yield {
      assertEquals(r1._1, 2)
      assertEqualsDouble(r1._2, 0.5, 1e-9) // 20ms / 40ms
      assertEquals(r2._1, 1)
      assertEqualsDouble(r2._2, 0.75, 1e-9) // 30ms / 40ms
      assertEquals(r3._1, 1)
      assertEqualsDouble(r3._2, 0.0, 1e-9)
    }
  }

  // ── AnimationSettings overload ──

  test("consumeSteps with AnimationSettings — uses configured step") {
    for {
      (clock, ref) <- mkEnv()
      _ <- clock.advance(50.millis) // 50ms / 20ms = 2 steps + 10ms
      result <- {
        implicit val settings: com.github.morotsman.lote.api.AnimationSettings =
          com.github.morotsman.lote.api.AnimationSettings(20.millis)
        implicit val clk: AnimationClock[IO] = clock
        FixedStep.consumeSteps(ref)
      }
    } yield {
      assertEquals(result._1, 2)
      assertEqualsDouble(result._2, 0.5, 1e-9) // 10ms / 20ms
    }
  }
}
