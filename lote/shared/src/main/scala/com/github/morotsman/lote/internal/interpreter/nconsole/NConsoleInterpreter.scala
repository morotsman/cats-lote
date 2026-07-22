package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.Monad
import cats.effect.{Resource, Sync}
import cats.implicits._
import com.github.morotsman.lote.api.{
  Alignment,
  Character,
  Key,
  MouseClick,
  MouseMove,
  PlatformCapability,
  RenderEffect,
  Screen,
  ScreenAdjusted,
  SpecialKey,
  UserInput
}
import com.github.morotsman.lote.api.spi.{EffectfulTerminal, NConsole, Terminal => TerminalAlgebra}

private[lote] object NConsoleInterpreter {

  /** Creates an NConsole backed by the given effectful Terminal algebra.
    */
  private[lote] def make[F[_]: Sync](terminal: TerminalAlgebra[F]): F[NConsole[F]] = {
    terminal.size.map { screen =>
      val width = screen.screenWidth
      val height = screen.screenHeight

      new NConsole[F] {
        override def read(timeoutInMillis: Long): F[UserInput] =
          terminal.read(timeoutInMillis).flatMap { rawInput =>
            val input = rawInput.toChar
            if (input == 27) {
              terminal.read(0).flatMap { rawInput2 =>
                val input2 = rawInput2.toChar
                if (input2 == '[') {
                  terminal.read(0).flatMap { rawInput3 =>
                    val input3 = rawInput3.toChar
                    input3 match {
                      case 'A' => Monad[F].pure(Key(SpecialKey.Up))
                      case 'D' => Monad[F].pure(Key(SpecialKey.Left))
                      case 'C' => Monad[F].pure(Key(SpecialKey.Right))
                      case 'B' => Monad[F].pure(Key(SpecialKey.Down))
                      case '<' =>
                        // SGR extended mouse mode: ESC [ < button ; x ; y M/m
                        parseSgrMouse(terminal)
                      case _ => Monad[F].pure(Key(SpecialKey.Unknown))
                    }
                  }
                } else {
                  Monad[F].pure(Key(SpecialKey.Esc))
                }
              }
            } else if (input.toInt == 65534) {
              Monad[F].pure(Key(SpecialKey.Timeout))
            } else if (input == ' ') {
              Monad[F].pure(Key(SpecialKey.Space))
            } else if (input == '\r' || input == '\n') {
              Monad[F].pure(Key(SpecialKey.Enter))
            } else {
              Monad[F].pure(Character(input))
            }
          }

        override def readInterruptible(): F[UserInput] = {
          Monad[F].tailRecM(()) { _ =>
            for {
              input <- read(5)
              result <- input match {
                case Key(k) if k == SpecialKey.Timeout =>
                  Monad[F].pure(Either.left(()))
                case _: MouseMove =>
                  Monad[F].pure(Either.left(()))
                case _: MouseClick =>
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
          terminal.write(screenAdjusted.content)

        override def clear(): F[Unit] =
          terminal.flush()

        override def context: F[Screen] =
          terminal.size

        override def close(): F[Unit] =
          terminal.close()

        override def read(): F[UserInput] = read(0L)

        override def capabilities: Set[PlatformCapability] = terminal.capabilities

        override def scene3DRef: Option[Any] = terminal.scene3DRef

        override def applyEffect(effect: RenderEffect): F[Unit] =
          terminal match {
            case et: EffectfulTerminal[F @unchecked] => et.applyEffect(effect)
            case _ =>
              effect match {
                case RenderEffect.RenderFloatingChars(chars) if chars.nonEmpty =>
                  // Fallback for terminal backends: render floating chars at
                  // the nearest discrete grid position using ANSI cursor movement.
                  val commands = chars.flatMap { fc =>
                    val col = math.round(fc.cellX).toInt
                    val row = math.round(fc.cellY).toInt
                    if (col >= 0 && col < width && row >= 0 && row < height)
                      Some(s"\u001b[${row + 1};${col + 1}H${fc.char}")
                    else
                      None
                  }
                  if (commands.nonEmpty)
                    terminal.writeRaw(commands.mkString)
                  else
                    Sync[F].unit
                case _ => Sync[F].unit
              }
          }
      }
    }
  }

  /** Parse SGR extended mouse sequence: button ; x ; y M/m */
  private def parseSgrMouse[F[_]: Sync](terminal: TerminalAlgebra[F]): F[UserInput] = {
    Monad[F].tailRecM(new StringBuilder) { sb =>
      terminal.read(0).map { rawCh =>
        val ch = rawCh.toChar
        if (ch == 'M' || ch == 'm') {
          val parts = sb.toString.split(';')
          val result = if (parts.length == 3) {
            val button = parts(0).toInt
            val x = parts(1).toInt
            val y = parts(2).toInt
            val isRelease = ch == 'm'
            if (button >= 32 && button <= 35) {
              MouseMove(x, y)
            } else if (!isRelease) {
              MouseClick(x, y)
            } else {
              Key(SpecialKey.Unknown)
            }
          } else {
            Key(SpecialKey.Unknown)
          }
          Either.right(result)
        } else {
          sb.append(ch)
          Either.left(sb)
        }
      }
    }
  }

  /** Creates an NConsole as a Resource backed by the given Terminal. Terminal cleanup (close) is called on release. */
  private[lote] def resource[F[_]: Sync](terminal: TerminalAlgebra[F]): Resource[F, NConsole[F]] =
    Resource.make(make[F](terminal))(_.close())
}
