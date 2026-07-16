package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.{Clock, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.UserInput
import com.github.morotsman.lote.internal.algebra.IdleDetector

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, DurationInt}

private[lote] case class IdleDetectorConfig(
    idleTimeout: FiniteDuration = 2.minutes
)

private case class IdleDetectorState(
    lastSlideChangeTime: Long,
    lastContentChangeTime: Long,
    lastRawContent: Option[String] = None,
    isIdle: Boolean = false,
    idleStartTime: Long = 0
)

private[lote] object IdleDetectorInterpreter {

  def make[F[_]: Monad: Clock: Ref.Make](
      config: IdleDetectorConfig = IdleDetectorConfig()
  ): F[IdleDetector[F]] = for {
    now <- Clock[F].realTime.map(_.toMillis)
    state <- Ref[F].of(
      IdleDetectorState(lastSlideChangeTime = now, lastContentChangeTime = now)
    )
  } yield new IdleDetector[F] {

    override def notifyActivity(): F[Unit] = for {
      now <- Clock[F].realTime.map(_.toMillis)
      _ <- state.update(
        _.copy(
          lastSlideChangeTime = now,
          lastContentChangeTime = now,
          isIdle = false
        )
      )
    } yield ()

    override def onKeyPress(input: UserInput): F[Unit] = notifyActivity()

    override def onMouseClick(x: Int, y: Int): F[Unit] = notifyActivity()

    override def onMouseMove(x: Int, y: Int): F[Unit] = notifyActivity()

    override def onContentChange(content: String): F[Unit] = for {
      s <- state.get
      now <- Clock[F].realTime.map(_.toMillis)
      contentChanged = s.lastRawContent.exists(_ != content)
      _ <-
        if (contentChanged) {
          state.update(
            _.copy(
              lastContentChangeTime = now,
              lastRawContent = Some(content),
              isIdle = false
            )
          )
        } else {
          state.update(_.copy(lastRawContent = Some(content)))
        }
    } yield ()

    override def isIdle: F[Boolean] = for {
      s <- state.get
      now <- Clock[F].realTime.map(_.toMillis)
      lastActivity = Math.max(s.lastSlideChangeTime, s.lastContentChangeTime)
      elapsed = FiniteDuration(now - lastActivity, TimeUnit.MILLISECONDS)
      idle = elapsed >= config.idleTimeout
      _ <-
        if (idle && !s.isIdle) {
          state.update(_.copy(isIdle = true, idleStartTime = now))
        } else {
          Monad[F].unit
        }
    } yield idle

    override def idleStartTime: F[Option[Long]] = for {
      s <- state.get
    } yield if (s.isIdle) Some(s.idleStartTime) else None
  }
}
