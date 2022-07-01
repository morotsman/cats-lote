package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.Ref
import cats.effect.kernel.{Fiber, Spawn}
import cats.effect.{Clock, Temporal}
import cats.implicits._
import cats.effect.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Overlay}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.Context

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class TimerState[F[_]](
                ongoing: Option[Fiber[F, Throwable, ScreenAdjusted]] = None
                )

object Timer {

  def make[F[_] : Monad : Clock : Temporal : Ref.Make: Spawn](
                                    allocatedTime: FiniteDuration,
                                    console: NConsole[F],
                                    startTime: Long = System.currentTimeMillis()
                                  ): F[Overlay[F]] = Ref[F].of(TimerState[F]()).map { state =>
    new Overlay[F] {

      // TODO refactor
      override def applyOverlay(context: Context, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = {
        def addTimeLeft() = for {
          time <- Clock[F].realTime
          timeLeft = allocatedTime.minus(FiniteDuration(time.toMillis - startTime, TimeUnit.MILLISECONDS))
          seconds = timeLeft.toSeconds % (timeLeft.toMinutes * 60)
          output = s"${timeLeft.toMinutes}:${if (seconds < 10) "0" + seconds else seconds}"
          result = ScreenAdjusted(output + screenAdjusted.content.drop(output.length))
        } yield result


        def loop(): F[ScreenAdjusted] = for {
          _ <- Temporal[F].sleep(1.second)
          r <- addTimeLeft()
          _ <- console.writeString(r)
          _ <- loop()
        } yield r


        for {
          s <- state.get
          _ <- s.ongoing.traverse(_.cancel)
          r <- addTimeLeft()
          f <- loop().start
          _ <- state.set(TimerState(Option(f)))
        } yield r

      }

    }
  }

}
