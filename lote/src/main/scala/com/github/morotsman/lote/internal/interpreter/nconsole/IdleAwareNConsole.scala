package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.Monad
import cats.implicits._
import com.github.morotsman.lote.api.{Alignment, MouseClick, MouseMove, Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.internal.algebra.IdleDetector
import com.github.morotsman.lote.api.spi.NConsole

/** Wraps an NConsole to notify an IdleDetector on user input (read) and content changes (write). readInterruptible is
  * intentionally NOT notifying the detector, preserving existing semantics.
  */
private[lote] object IdleAwareNConsole {

  def wrap[F[_]: Monad](
      underlying: NConsole[F],
      idleDetector: IdleDetector[F]
  ): NConsole[F] =
    new NConsole[F] {

      private def notifyInput(input: UserInput): F[Unit] = input match {
        case MouseClick(x, y) => idleDetector.onMouseClick(x, y)
        case MouseMove(x, y)  => idleDetector.onMouseMove(x, y)
        case _                => idleDetector.onKeyPress(input)
      }

      override def read(timeoutInMillis: Long): F[UserInput] = for {
        input <- underlying.read(timeoutInMillis)
        _ <- notifyInput(input)
      } yield input

      override def read(): F[UserInput] = for {
        input <- underlying.read()
        _ <- notifyInput(input)
      } yield input

      // Intentionally does NOT notify idle detector
      override def readInterruptible(): F[UserInput] =
        underlying.readInterruptible()

      override def alignText(
          s: String,
          alignment: Alignment
      ): F[ScreenAdjusted] =
        underlying.alignText(s, alignment)

      override def writeString(s: ScreenAdjusted): F[Unit] =
        underlying.writeString(s)

      override def clear(): F[Unit] = underlying.clear()

      override def close(): F[Unit] = underlying.close()

      override def context: F[Screen] = underlying.context
    }
}

