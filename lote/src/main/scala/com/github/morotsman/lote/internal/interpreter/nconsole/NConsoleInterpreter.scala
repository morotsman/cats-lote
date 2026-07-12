package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.Monad
import cats.effect.{Resource, Sync}
import cats.implicits._
import com.github.morotsman.lote.api.{Alignment, Character, Key, MouseClick, MouseMove, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.NConsole

private[lote] object NConsoleInterpreter {

  /** Creates an NConsole backed by the given Terminal abstraction. Package-private: use `resource` methods for safe
    * lifecycle management.
    */
  private[lote] def make[F[_]: Sync](terminal: Terminal): NConsole[F] = {
    val width = terminal.width
    val height = terminal.height

    new NConsole[F] {
      override def read(timeoutInMillis: Long): F[UserInput] =
        Sync[F].blocking {
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
                case '<' =>
                  // SGR extended mouse mode: ESC [ < button ; x ; y M/m
                  val sb = new StringBuilder
                  var ch = terminal.read(0).toChar
                  while (ch != 'M' && ch != 'm') {
                    sb.append(ch)
                    ch = terminal.read(0).toChar
                  }
                  val parts = sb.toString.split(';')
                  if (parts.length == 3) {
                    val button = parts(0).toInt
                    val x = parts(1).toInt
                    val y = parts(2).toInt
                    val isRelease = ch == 'm'
                    if (button >= 32 && button <= 35) {
                      // Motion events (bit 5 set = 32 added to button value)
                      MouseMove(x, y)
                    } else if (!isRelease) {
                      MouseClick(x, y)
                    } else {
                      Key(SpecialKey.Unknown)
                    }
                  } else {
                    Key(SpecialKey.Unknown)
                  }
                case _ => Key(SpecialKey.Unknown)
              }
            } else {
              Key(SpecialKey.Esc)
            }
          } else if (input.toInt == 65534) {
            Key(SpecialKey.Timeout)
          } else if (input == ' ') {
            Key(SpecialKey.Space)
          } else if (input == '\r' || input == '\n') {
            Key(SpecialKey.Enter)
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

      override def alignText(
          s: String,
          alignment: Alignment
      ): F[ScreenAdjusted] = Sync[F].blocking {
        val cutOverflow =
          s.split("\n").take(height - 1).map(_.take(width)).mkString("\n")
        ScreenAdjusted(
          Aligner.alignText(
            cutOverflow,
            alignment,
            width = width,
            height = height
          )
        )
      }

      override def writeString(screenAdjusted: ScreenAdjusted): F[Unit] =
        Sync[F].blocking {
          terminal.write(screenAdjusted.content)
        }

      override def clear(): F[Unit] = Sync[F].blocking {
        terminal.flush()
      }

      override def context: F[Screen] = Sync[F].delay(
        Screen(
          screenWidth = width,
          screenHeight = height
        )
      )

      override def close(): F[Unit] = Sync[F].blocking {
        terminal.close()
      }

      override def read(): F[UserInput] = read(0L)
    }
  }

  private def make[F[_]: Sync](): NConsole[F] = make(JLineTerminal.make())

  /** Creates an NConsole as a Resource that automatically handles terminal cleanup (disabling mouse tracking, restoring
    * terminal state) when the resource is released.
    */
  private[lote] def resource[F[_]: Sync](): Resource[F, NConsole[F]] =
    Resource.make(Sync[F].delay(make[F]()))(_.close())

  /** Creates an NConsole as a Resource backed by the given Terminal.
    */
  private[lote] def resource[F[_]: Sync](terminal: Terminal): Resource[F, NConsole[F]] =
    Resource.make(Sync[F].delay(make[F](terminal)))(_.close())
}

