package com.github.morotsman.lote.interpreter.nconsole

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._

object NConsoleInterpreter {

  /**
   * Creates an NConsole backed by the given Terminal abstraction.
   */
  def make[F[_] : Sync](terminal: Terminal): NConsole[F] = {
    val width = terminal.width
    val height = terminal.height

    new NConsole[F] {
      override def read(timeoutInMillis: Long): F[UserInput] = Sync[F].blocking {
        val input = terminal.read(timeoutInMillis).toChar

        if (input == 27) {
          val input = terminal.read(0).toChar
          if (input == '[') {
            val input = terminal.read(0).toChar
            input match {
              case 'A' => Key(SpecialKey.Up)
              case 'D' => Key(SpecialKey.Left)
              case 'C' => Key(SpecialKey.Right)
              case 'B' => Key(SpecialKey.Down)
              case _ => Key(SpecialKey.Unknown)
            }
          } else {
            Key(SpecialKey.Esc)
          }
        } else if (input.toInt == 65534) {
          Key(SpecialKey.Timeout)
        } else {
          Character(input)
        }
      }

      override def readInterruptible(): F[UserInput] = {
        Monad[F].tailRecM(()) { _ =>
          for {
            input <- read(5)
            result <- input match {
              case Key(k) if k == SpecialKey.Timeout =>
                Monad[F].pure(Either.left(()))
              case _ =>
                Monad[F].pure(Either.right(input))
            }
          } yield result
        }
      }

      override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] = Sync[F].blocking {
        val cutOverflow = s.split("\n").take(height - 1).map(_.take(width)).mkString("\n")
        ScreenAdjusted(Aligner.alignText(cutOverflow, alignment, width = width, height = height))
      }

      override def writeString(screenAdjusted: ScreenAdjusted): F[Unit] = Sync[F].blocking {
        terminal.write(screenAdjusted.content)
      }

      override def clear(): F[Unit] = Sync[F].blocking {
        terminal.flush()
      }

      override def context: F[Screen] = Sync[F].delay(Screen(
        screenWidth = width,
        screenHeight = height
      ))

      override def read(): F[UserInput] = read(0L)
    }
  }

  def make[F[_] : Sync](): NConsole[F] = make(JLineTerminal.make())
}
