package com.github.morotsman.lote.testkit

import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Key, Screen, ScreenAdjusted, SpecialKey, UserInput, VerticalAlignment}
import com.github.morotsman.lote.api.spi.NConsole

import scala.concurrent.duration.{Duration, FiniteDuration}

/** A test implementation of `NConsole[F]` that records all interactions and allows injecting user inputs.
  *
  * Use this to unit-test custom slides, transitions, and overlays without a real terminal.
  *
  * @param readDelay
  *   optional delay before each `read()` call returns, giving background fibers time to run.
  *   In production, `read()` blocks on keyboard input; this parameter simulates that pause.
  *   Defaults to `Duration.Zero` (no delay). Set to e.g. `1.millis` for integration tests
  *   that use `SessionBuilder.runWith`, where slide fibers need time to write content.
  *
  * Example:
  * {{{
  * for {
  *   console <- TestConsole.make[IO](screen = Screen(80, 24), inputs = List(Character('a')))
  *   slide   <- MyCustomSlide.make[IO](console)
  *   _       <- slide.startShow
  *   _       <- slide.userInput(Character('a'))
  *   written <- console.writtenFrames
  * } yield assert(written.nonEmpty)
  * }}}
  */
final class TestConsole[F[_]: Temporal] private (
    val screen: Screen,
    private val inputsRef: Ref[F, List[UserInput]],
    private val writtenRef: Ref[F, List[String]],
    private val clearedRef: Ref[F, Int],
    private val readDelay: FiniteDuration
) extends NConsole[F] {

  override def read(timeoutInMillis: Long): F[UserInput] = {
    val delay =
      if (readDelay > Duration.Zero)
        Temporal[F].cede *> Temporal[F].sleep(readDelay) *> Temporal[F].cede
      else Temporal[F].cede
    delay *>
      inputsRef.modify {
        case head :: tail => (tail, head)
        case Nil          => (Nil, Key(SpecialKey.Timeout))
      }
  }

  override def read(): F[UserInput] = read(0L)

  override def readInterruptible(): F[UserInput] = read()

  override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] = {
    val lines = s.split("\n", -1)

    val horizontallyAligned = alignment.horizontalAlignment match {
      case HorizontalAlignment.Left =>
        lines.map(l => l + " " * math.max(0, screen.screenWidth - l.length))
      case HorizontalAlignment.Center =>
        lines.map { l =>
          val padding = math.max(0, (screen.screenWidth - l.length) / 2)
          " " * padding + l + " " * math.max(0, screen.screenWidth - l.length - padding)
        }
      case HorizontalAlignment.Right =>
        lines.map(l => " " * math.max(0, screen.screenWidth - l.length) + l)
    }

    val contentHeight = horizontallyAligned.length
    val verticallyAligned = alignment.verticalAlignment match {
      case VerticalAlignment.Up =>
        horizontallyAligned ++ Array.fill(math.max(0, screen.screenHeight - contentHeight))(" " * screen.screenWidth)
      case VerticalAlignment.Center =>
        val topPadding = math.max(0, (screen.screenHeight - contentHeight) / 2)
        Array.fill(topPadding)(" " * screen.screenWidth) ++
          horizontallyAligned ++
          Array.fill(math.max(0, screen.screenHeight - contentHeight - topPadding))(" " * screen.screenWidth)
      case VerticalAlignment.Down =>
        Array.fill(math.max(0, screen.screenHeight - contentHeight))(" " * screen.screenWidth) ++ horizontallyAligned
    }

    Temporal[F].pure(ScreenAdjusted(verticallyAligned.mkString("\n")))
  }

  override def writeString(s: ScreenAdjusted): F[Unit] =
    writtenRef.update(s.content :: _)

  override def clear(): F[Unit] =
    clearedRef.update(_ + 1)

  override def close(): F[Unit] = Temporal[F].unit

  override def context: F[Screen] = Temporal[F].pure(screen)

  // --- Test inspection methods ---

  /** Returns all written frames in reverse chronological order (most recent first). */
  def writtenFrames: F[List[String]] = writtenRef.get

  /** Returns written frames in chronological order (oldest first). */
  def writtenFramesInOrder: F[List[String]] = writtenRef.get.map(_.reverse)

  /** Returns the most recently written frame, or None if nothing was written. */
  def lastWrittenFrame: F[Option[String]] = writtenRef.get.map(_.headOption)

  /** Returns how many times `clear()` was called. */
  def clearCount: F[Int] = clearedRef.get

  /** Enqueue additional user inputs to be consumed by subsequent `read()` calls. */
  def enqueueInputs(inputs: List[UserInput]): F[Unit] =
    inputsRef.update(_ ++ inputs)

  /** Reset all recorded state (written frames, clear count). */
  def reset: F[Unit] =
    writtenRef.set(Nil) *> clearedRef.set(0)
}

object TestConsole {

  /** Creates a new `TestConsole` with the given screen dimensions and pre-loaded user inputs.
    *
    * @param readDelay
    *   optional delay before each `read()` returns (default `Duration.Zero`).
    *   Set to e.g. `1.millis` for integration tests where slide fibers need time to write content.
    */
  def make[F[_]: Temporal: Ref.Make](
      screen: Screen = Screen(80, 24),
      inputs: List[UserInput] = Nil,
      readDelay: FiniteDuration = Duration.Zero
  ): F[TestConsole[F]] =
    for {
      inputsRef <- Ref[F].of(inputs)
      writtenRef <- Ref[F].of(List.empty[String])
      clearedRef <- Ref[F].of(0)
    } yield new TestConsole[F](screen, inputsRef, writtenRef, clearedRef, readDelay)
}
