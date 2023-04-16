package com.github.morotsman.lote.interpreter.nconsole

import cats.Monad
import cats.effect.{IO, Sync}
import cats.implicits._
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

object NConsole {
  private val terminal = TerminalBuilder.terminal()
  private val reader = terminal.reader()
  private val width = terminal.getWidth
  terminal.enterRawMode()
  terminal.puts(Capability.clear_screen)
  private val height = terminal.getHeight

  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] = instance

  def make[F[_] : Sync](): F[NConsole[F]] = {
    Sync[F].delay(
      new NConsole[F] {
        override def read(timeoutInMillis: Long): F[UserInput] = Sync[F].blocking {
          val input = reader.read(timeoutInMillis).toChar

          if (input == 27) {
            val input = reader.read().toChar
            if (input == '[') {
              val input = reader.read().toChar
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
          ScreenAdjusted(Aligner.alignText(cutOverflow, alignment, width = width, height = height), width, height)
        }

        override def writeString(screenAdjusted: ScreenAdjusted): F[Unit] = Sync[F].blocking {
          println(screenAdjusted.content)
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

    )
  }

  case class ScreenAdjusted(content: String, width: Int, height: Int)
}

object NConsoleInstances {
  implicit val IONConsole: NConsole[IO] = new NConsole[IO] {
    private val console: IO[NConsole[IO]] = NConsole.make[IO]()

    override def read(): IO[UserInput] = console.flatMap(_.read())

    override def read(timeoutInMillis: Long): IO[UserInput] =
      console.flatMap(_.read(timeoutInMillis))

    override def readInterruptible(): IO[UserInput] =
      console.flatMap(_.readInterruptible())

    override def alignText(s: String, alignment: Alignment): IO[NConsole.ScreenAdjusted] =
      console.flatMap(_.alignText(s, alignment))

    override def writeString(s: NConsole.ScreenAdjusted): IO[Unit] =
      console.flatMap(_.writeString(s))

    override def clear(): IO[Unit] =
      console.flatMap(_.clear())

    override def context: IO[Screen] =
      console.flatMap(_.context)
  }
}
