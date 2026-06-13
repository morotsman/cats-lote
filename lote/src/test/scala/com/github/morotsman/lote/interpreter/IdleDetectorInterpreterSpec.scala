package com.github.morotsman.lote.interpreter

import cats.effect.IO
import com.github.morotsman.lote.model._
import munit.CatsEffectSuite

import scala.concurrent.duration._

class IdleDetectorInterpreterSpec extends CatsEffectSuite {

  test("is not idle before timeout") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 10.minutes))
      result <- detector.isIdle
    } yield assert(!result)
  }

  test("is idle after timeout elapses") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      result <- detector.isIdle
    } yield assert(result)
  }

  test("notifyActivity resets idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.notifyActivity()
      result <- detector.isIdle
    } yield assert(!result)
  }

  test("onKeyPress resets idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.onKeyPress(Key(SpecialKey.Right))
      result <- detector.isIdle
    } yield assert(!result)
  }

  test("onContentChange with different content resets idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- detector.onContentChange("first content")
      _ <- IO.sleep(100.millis)
      _ <- detector.onContentChange("different content")
      result <- detector.isIdle
    } yield assert(!result)
  }

  test("onContentChange with same content does not reset idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- detector.onContentChange("same content")
      _ <- IO.sleep(30.millis)
      _ <- detector.onContentChange("same content")
      _ <- IO.sleep(30.millis)
      // Total > 50ms, same content should not have reset
      result <- detector.isIdle
    } yield assert(result)
  }

  test("idleStartTime returns None when not idle") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 10.minutes))
      result <- detector.idleStartTime
    } yield assertEquals(result, None)
  }

  test("idleStartTime returns Some when idle") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.isIdle // triggers the idle state transition
      result <- detector.idleStartTime
    } yield assert(result.isDefined)
  }

  test("idleStartTime resets after notifyActivity") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.isIdle
      startBefore <- detector.idleStartTime
      _ <- detector.notifyActivity()
      startAfter <- detector.idleStartTime
    } yield {
      assert(startBefore.isDefined)
      assertEquals(startAfter, None)
    }
  }

  test("becomes idle again after activity followed by another timeout") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      idle1 <- detector.isIdle
      _ <- detector.notifyActivity()
      idle2 <- detector.isIdle
      _ <- IO.sleep(100.millis)
      idle3 <- detector.isIdle
    } yield {
      assert(idle1)
      assert(!idle2)
      assert(idle3)
    }
  }

  test("onMouseClick resets idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      _ <- detector.onMouseClick(10, 20)
      idleAfter <- detector.isIdle
    } yield {
      assert(idleBefore)
      assert(!idleAfter)
    }
  }

  test("onMouseMove resets idle state") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      idleBefore <- detector.isIdle
      _ <- detector.onMouseMove(5, 15)
      idleAfter <- detector.isIdle
    } yield {
      assert(idleBefore)
      assert(!idleAfter)
    }
  }

  test("onMouseClick resets idleStartTime") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.isIdle
      startBefore <- detector.idleStartTime
      _ <- detector.onMouseClick(1, 1)
      startAfter <- detector.idleStartTime
    } yield {
      assert(startBefore.isDefined)
      assertEquals(startAfter, None)
    }
  }

  test("onMouseMove resets idleStartTime") {
    for {
      detector <- IdleDetectorInterpreter.make[IO](IdleDetectorConfig(idleTimeout = 50.millis))
      _ <- IO.sleep(100.millis)
      _ <- detector.isIdle
      startBefore <- detector.idleStartTime
      _ <- detector.onMouseMove(1, 1)
      startAfter <- detector.idleStartTime
    } yield {
      assert(startBefore.isDefined)
      assertEquals(startAfter, None)
    }
  }
}

