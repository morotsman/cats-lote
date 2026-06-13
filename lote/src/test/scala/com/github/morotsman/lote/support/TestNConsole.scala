package com.github.morotsman.lote.support

import cats.effect.IO
import cats.effect.Ref
import com.github.morotsman.lote.algebra.NConsole
import com.github.morotsman.lote.model._

/**
 * A test implementation of NConsole that records interactions and allows
 * injecting user inputs for testing purposes.
 */
case class TestNConsole(
                         screen: Screen,
                         inputsRef: Ref[IO, List[UserInput]],
                         writtenRef: Ref[IO, List[String]],
                         clearedRef: Ref[IO, Int]
                       ) extends NConsole[IO] {

  override def read(timeoutInMillis: Long): IO[UserInput] =
    inputsRef.modify {
      case head :: tail => (tail, head)
      case Nil          => (Nil, Key(SpecialKey.Timeout))
    }

  override def read(): IO[UserInput] =
    inputsRef.modify {
      case head :: tail => (tail, head)
      case Nil          => (Nil, Key(SpecialKey.Timeout))
    }

  override def readInterruptible(): IO[UserInput] = read()

  override def alignText(s: String, alignment: Alignment): IO[ScreenAdjusted] = {
    val lines = s.split("\n", -1)
    lines.map(_.length).maxOption.getOrElse(0)

    val horizontallyAligned = alignment.horizontalAlignment match {
      case HorizontalAlignment.Left => lines.map(l => l + " " * (screen.screenWidth - l.length))
      case HorizontalAlignment.Center => lines.map { l =>
        val padding = (screen.screenWidth - l.length) / 2
        " " * padding + l + " " * (screen.screenWidth - l.length - padding)
      }
      case HorizontalAlignment.Right => lines.map(l => " " * (screen.screenWidth - l.length) + l)
    }

    val contentHeight = horizontallyAligned.length
    val verticallyAligned = alignment.verticalAlignment match {
      case VerticalAlignment.Up =>
        horizontallyAligned ++ Array.fill(screen.screenHeight - contentHeight)(" " * screen.screenWidth)
      case VerticalAlignment.Center =>
        val topPadding = (screen.screenHeight - contentHeight) / 2
        Array.fill(topPadding)(" " * screen.screenWidth) ++ horizontallyAligned ++
          Array.fill(screen.screenHeight - contentHeight - topPadding)(" " * screen.screenWidth)
      case VerticalAlignment.Down =>
        Array.fill(screen.screenHeight - contentHeight)(" " * screen.screenWidth) ++ horizontallyAligned
    }

    IO.pure(ScreenAdjusted(verticallyAligned.mkString("\n")))
  }

  override def writeString(s: ScreenAdjusted): IO[Unit] =
    writtenRef.update(s.content :: _)

  override def clear(): IO[Unit] =
    clearedRef.update(_ + 1)

  override def context: IO[Screen] = IO.pure(screen)
}

object TestNConsole {
  def make(
            screen: Screen = Screen(80, 24),
            inputs: List[UserInput] = Nil
          ): IO[TestNConsole] = for {
    inputsRef <- Ref.of[IO, List[UserInput]](inputs)
    writtenRef <- Ref.of[IO, List[String]](Nil)
    clearedRef <- Ref.of[IO, Int](0)
  } yield TestNConsole(screen, inputsRef, writtenRef, clearedRef)
}

