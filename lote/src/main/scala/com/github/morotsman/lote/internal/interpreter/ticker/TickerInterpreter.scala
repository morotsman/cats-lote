package com.github.morotsman.lote.internal.interpreter.ticker

import cats.effect.{Ref, Temporal}
import cats.effect.implicits._
import cats.effect.kernel.Fiber
import cats.implicits._
import com.github.morotsman.lote.api.spi.{Ticker, TickerSubscription}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

private[lote] case class TickerEntry[F[_]](id: Long, callback: F[Unit])

private[lote] case class TickerState[F[_]](
    subscribers: List[TickerEntry[F]] = List.empty,
    nextId: Long = 0,
    fiber: Option[Fiber[F, Throwable, Unit]] = None,
    running: Boolean = false
)

private[lote] object TickerInterpreter {

  def make[F[_]: Temporal: Ref.Make](
      interval: FiniteDuration = 40.millis
  ): F[Ticker[F]] =
    Ref[F].of(TickerState[F]()).map { state =>
      new Ticker[F] {

        private def tickLoop(): F[Unit] = for {
          _ <- Temporal[F].sleep(interval)
          s <- state.get
          _ <- s.subscribers.traverse_(_.callback)
          running <- state.get.map(_.running)
          _ <- if (running) tickLoop() else Temporal[F].unit
        } yield ()

        override def subscribe(callback: F[Unit]): F[TickerSubscription[F]] =
          for {
            id <- state.modify(s => (s.copy(nextId = s.nextId + 1), s.nextId))
            _ <- state.update(s => s.copy(subscribers = s.subscribers :+ TickerEntry(id, callback)))
          } yield new TickerSubscription[F] {
            override def cancel: F[Unit] =
              state.update(s => s.copy(subscribers = s.subscribers.filterNot(_.id == id)))
          }

        override def start: F[Unit] = for {
          s <- state.get
          _ <-
            if (s.running) Temporal[F].unit
            else
              for {
                _ <- state.update(_.copy(running = true))
                f <- tickLoop().start
                _ <- state.update(_.copy(fiber = Some(f)))
              } yield ()
        } yield ()

        override def stop: F[Unit] = for {
          s <- state.get
          _ <- s.fiber.traverse_(_.cancel)
          _ <- state.set(TickerState[F]())
        } yield ()
      }
    }
}
