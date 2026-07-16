package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.{Fiber, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{Key, SpecialKey, UserInput}
import com.github.morotsman.lote.internal.algebra.PresentationExecutor
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.internal.model.Presentation

private[lote] object PresentationExecutorInterpreter {

  private case class ExecutorState(
      currentIndex: Int,
      switchSlide: Boolean
  )

  private sealed trait Command
  private case object NavigateRight extends Command
  private case object NavigateLeft extends Command
  private case object Exit extends Command
  private case class DelegateInput(input: UserInput) extends Command

  private type LoopResult[F[_]] = Either[Fiber[F, Throwable, Unit], (Int, Boolean, Fiber[F, Throwable, Unit])]

  def make[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      onSlideChange: Int => F[Unit] = null
  ): F[PresentationExecutor[F]] = {
    val notifyOnSlideChange: Int => F[Unit] =
      if (onSlideChange != null) onSlideChange else (_: Int) => Monad[F].unit

    Ref[F].of(ExecutorState(currentIndex = 0, switchSlide = true)).map { stateRef =>
      new PresentationExecutor[F] {
        override def start(): F[Unit] = for {
          _ <- NConsole[F].clear()
          _ <- executionLoop()
        } yield ()

        override def setSlide(index: Int): F[Unit] = {
          val clampedIndex = Math.max(0, Math.min(index, presentation.slideSpecifications.length - 1))
          stateRef.set(ExecutorState(currentIndex = clampedIndex, switchSlide = true))
        }

        private def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
          Monad[F].unit.start.flatMap { initialWork =>
            Monad[F].tailRecM((initialWork: Fiber[F, Throwable, Unit])) { work =>
              for {
                state <- stateRef.get
                currentIndex = state.currentIndex
                current = presentation.slideSpecifications(currentIndex)
                currentWork <-
                  if (state.switchSlide) {
                    notifyOnSlideChange(currentIndex) >>
                      stateRef.set(ExecutorState(currentIndex, switchSlide = false)) >>
                      current.slide.startShow.start
                  } else {
                    Monad[F].pure(work)
                  }
                userInput <- NConsole[F].read()
                command = resolveCommand(userInput)
                result <- dispatch(command, presentation, stateRef, currentIndex, currentWork)
              } yield result
            }
          }
        }
      }
    }
  }

  private def resolveCommand(input: UserInput): Command = input match {
    case Key(k) if k == SpecialKey.Right => NavigateRight
    case Key(k) if k == SpecialKey.Left  => NavigateLeft
    case Key(k) if k == SpecialKey.Esc   => Exit
    case other                           => DelegateInput(other)
  }

  private def dispatch[F[_]: Temporal: NConsole](
      command: Command,
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = command match {
    case NavigateRight =>
      handleNavigateRight(presentation, stateRef, currentIndex, currentWork)
    case NavigateLeft =>
      handleNavigateLeft(presentation, stateRef, currentIndex, currentWork)
    case Exit =>
      handleExit(presentation, currentIndex, currentWork)
    case DelegateInput(input) =>
      handleDelegateInput(presentation, stateRef, currentIndex, currentWork, input)
  }

  private def handleNavigateRight[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    if (currentIndex < presentation.slideSpecifications.length - 1) {
      val nextSlide = presentation.slideSpecifications(currentIndex + 1).slide
      current.slide.stopShow >>
        currentWork.cancel >>
        NConsole[F].clear() >>
        current.out
          .fold(Monad[F].unit) { t =>
            Temporal[F]
              .race(
                t.transition(current.slide, nextSlide),
                NConsole[F].readInterruptible()
              )
              .as(Monad[F].unit)
          } >>
        stateRef.set(ExecutorState(currentIndex + 1, switchSlide = true)) >>
        Monad[F].pure(Either.left(currentWork))
    } else {
      switchToSlide(current.slide, currentWork, stateRef, currentIndex) >>
        Monad[F].pure(Either.left(currentWork))
    }
  }

  private def handleNavigateLeft[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    val targetIndex = if (currentIndex > 0) currentIndex - 1 else currentIndex
    switchToSlide(current.slide, currentWork, stateRef, targetIndex) >>
      Monad[F].pure(Either.left(currentWork))
  }

  private def handleExit[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    current.slide.stopShow >>
      currentWork.cancel >>
      NConsole[F].clear() >>
      Monad[F].pure(Either.right((currentIndex, true, currentWork)))
  }

  private def handleDelegateInput[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit],
      input: UserInput
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    current.slide.userInput(input) >>
      stateRef.get.flatMap { latestState =>
        if (latestState.switchSlide) {
          // External navigation (e.g. QuickNavigation) has requested a slide change
          switchToSlide(current.slide, currentWork, stateRef, latestState.currentIndex) >>
            Monad[F].pure(Either.left(currentWork))
        } else {
          stateRef.set(ExecutorState(currentIndex, switchSlide = false)) >>
            Monad[F].pure(Either.left(currentWork))
        }
      }
  }

  // --- Low-level utility ---

  private def switchToSlide[F[_]: Temporal: NConsole](
      currentSlide: com.github.morotsman.lote.api.spi.Slide[F],
      currentWork: Fiber[F, Throwable, Unit],
      stateRef: Ref[F, ExecutorState],
      targetIndex: Int
  ): F[Unit] =
    currentSlide.stopShow >>
      currentWork.cancel >>
      NConsole[F].clear() >>
      stateRef.set(ExecutorState(targetIndex, switchSlide = true))
}
