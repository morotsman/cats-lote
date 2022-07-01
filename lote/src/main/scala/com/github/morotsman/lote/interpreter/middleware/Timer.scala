package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.Clock
import cats.implicits._
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.Context

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, TimeUnit}

object Timer {

  def make[F[_] : Monad : Clock](
                                  allocatedTime: FiniteDuration,
                                  startTime: Long = System.currentTimeMillis()
                                ): Overlay[F] = new Overlay[F] {

    // TODO refactor
    override def applyOverlay(context: Context, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] =  for {
      time <- Clock[F].realTime
      timeLeft = allocatedTime.minus(FiniteDuration(time.toMillis - startTime, TimeUnit.MILLISECONDS))
      seconds = timeLeft.toSeconds % (timeLeft.toMinutes * 60)
      output = s"${timeLeft.toMinutes}:${if (seconds < 10) "0" + seconds else seconds}"
      result = ScreenAdjusted(output + screenAdjusted.content.drop(output.length))
    } yield result

  }

}
