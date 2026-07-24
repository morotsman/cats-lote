package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.{Clock, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted}
import com.github.morotsman.lote.api.spi.Overlay

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

private[lote] object Timer {

  def make[F[_]: Temporal](
      allocatedTime: FiniteDuration,
      startTime: Long = System.currentTimeMillis()
  ): F[Overlay[F]] = Temporal[F].pure {
    new Overlay[F] {

      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): F[ScreenAdjusted] = for {
        time <- Clock[F].realTime
        timeLeft = allocatedTime.minus(
          FiniteDuration(time.toMillis - startTime, TimeUnit.MILLISECONDS)
        )
        seconds = if (timeLeft.toMinutes > 0) timeLeft.toSeconds % (timeLeft.toMinutes * 60) else timeLeft.toSeconds
        output = if (timeLeft.toMinutes > 0 || timeLeft.toSeconds > 0)
          s"${timeLeft.toMinutes}:${if (seconds < 10) "0" + seconds else seconds}"
        else
          "0:00"
        result = screenAdjusted.copy(content = output + screenAdjusted.content.drop(output.length))
      } yield result

    }
  }

}
