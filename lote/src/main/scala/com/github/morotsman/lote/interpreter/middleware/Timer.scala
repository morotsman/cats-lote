package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.{Clock, Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Overlay}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Timer {

  def make[F[_] : Monad : Clock : Temporal : Ref.Make : NConsole](
                                                                    allocatedTime: FiniteDuration,
                                                                    startTime: Long = System.currentTimeMillis()
                                                                  ): F[Overlay[F]] = Monad[F].pure {
    new Overlay[F] {


      override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = for {
        time <- Clock[F].realTime
        timeLeft = allocatedTime.minus(FiniteDuration(time.toMillis - startTime, TimeUnit.MILLISECONDS))
        seconds = timeLeft.toSeconds % (timeLeft.toMinutes * 60)
        output = s"${timeLeft.toMinutes}:${if (seconds < 10) "0" + seconds else seconds}"
        result = screenAdjusted.copy(content = output + screenAdjusted.content.drop(output.length))
      } yield result

    }
  }

}
