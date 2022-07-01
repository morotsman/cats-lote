package com.github.morotsman.lote.interpreter.nconsole

import cats.effect.Sync
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._
import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

object NConsole {
  case class ScreenAdjusted(content: String)


  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] = instance

  private val terminal = TerminalBuilder.terminal()
  terminal.enterRawMode()
  terminal.puts(Capability.clear_screen)
  private val reader = terminal.reader()

  private val width = terminal.getWidth
  private val height = terminal.getHeight

  def make[F[_] : Sync](): F[NConsole[F]] = {
    Sync[F].delay(
      new NConsole[F] {
        override def read(): F[UserInput] = Sync[F].blocking {
          val input = reader.read().toChar
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
          } else {
            Character(input)
          }
        }

        override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] = Sync[F].blocking {
          ScreenAdjusted(Aligner.alignText(s, alignment, width = width, height = height))
        }

        override def writeString(screenAdjusted: ScreenAdjusted): F[Unit] = Sync[F].blocking {
          println(screenAdjusted.content)
        }

        override def clear(): F[Unit] = Sync[F].blocking {
          terminal.flush()
        }

        override def context: F[Context] = Sync[F].delay(Context(
          screenWidth = width,
          screenHeight = height
        ))
      }

    )
  }
}