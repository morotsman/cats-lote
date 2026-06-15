package com.github.morotsman.lote.algebra

import cats.Applicative
import com.github.morotsman.lote.model.UserInput

trait IdleDetector[F[_]] {

  /** Signal that user activity occurred (e.g. slide navigation) */
  def notifyActivity(): F[Unit]

  /** Signal a key press event as activity */
  def onKeyPress(input: UserInput): F[Unit]

  /** Signal a mouse click event as activity */
  def onMouseClick(x: Int, y: Int): F[Unit]

  /** Signal mouse movement as activity */
  def onMouseMove(x: Int, y: Int): F[Unit]

  /** Signal content change; only resets idle if content actually differs */
  def onContentChange(content: String): F[Unit]

  /** Check whether the idle timeout has been reached */
  def isIdle: F[Boolean]

  /** Returns the timestamp (millis) when idle state began, if currently idle */
  def idleStartTime: F[Option[Long]]
}

object IdleDetector {
  @inline def apply[F[_]](implicit instance: IdleDetector[F]): IdleDetector[F] =
    instance

  /** A no-op IdleDetector that never reports idle. Useful as a default. */
  def noop[F[_]: Applicative]: IdleDetector[F] = new IdleDetector[F] {
    override def notifyActivity(): F[Unit] = Applicative[F].unit
    override def onKeyPress(input: UserInput): F[Unit] = Applicative[F].unit
    override def onMouseClick(x: Int, y: Int): F[Unit] = Applicative[F].unit
    override def onMouseMove(x: Int, y: Int): F[Unit] = Applicative[F].unit
    override def onContentChange(content: String): F[Unit] = Applicative[F].unit
    override def isIdle: F[Boolean] = Applicative[F].pure(false)
    override def idleStartTime: F[Option[Long]] = Applicative[F].pure(None)
  }
}
