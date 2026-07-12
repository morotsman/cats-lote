package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.{Fiber, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{Key, SpecialKey}
import com.github.morotsman.lote.internal.algebra.PresentationExecutor
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.internal.model.Presentation

private[lote] object PresentationExecutorInterpreter {

  private case class ExecutorState(
      currentIndex: Int,
      switchSlide: Boolean
  )

  def make[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      onSlideChange: Int => F[Unit] = null
  ): F[PresentationExecutor[F]] = {
    val slideChange: Int => F[Unit] =
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

        def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
          Monad[F].unit.start.flatMap { initialWork =>
            Monad[F].tailRecM((initialWork: Fiber[F, Throwable, Unit])) { work =>
              for {
                state <- stateRef.get
                currentIndex = state.currentIndex
                switchSlide = state.switchSlide
                current = presentation.slideSpecifications(currentIndex)
                _ <-
                  if (switchSlide) slideChange(currentIndex) else Monad[F].unit
                currentWork <-
                  if (switchSlide) {
                    current.slide.startShow.start
                  } else {
                    Monad[F].pure(work)
                  }
                _ <- if (switchSlide) stateRef.set(ExecutorState(currentIndex, switchSlide = false)) else Monad[F].unit
                userInput <- NConsole[F].read()
                result <- userInput match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentIndex < presentation.slideSpecifications.length - 1) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear() >>
                        current.out
                          .fold(Monad[F].unit) { t =>
                            Temporal[F]
                              .race(
                                t.transition(
                                  current.slide,
                                  presentation
                                    .slideSpecifications(currentIndex + 1)
                                    .slide
                                ),
                                NConsole[F].readInterruptible()
                              )
                              .as(Monad[F].unit)
                          } >>
                        stateRef.set(ExecutorState(currentIndex + 1, switchSlide = true)) >>
                        Monad[F].pure(Either.left(currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear() >>
                        stateRef.set(ExecutorState(currentIndex, switchSlide = true)) >>
                        Monad[F].pure(Either.left(currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentIndex > 0) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear() >>
                        stateRef.set(ExecutorState(currentIndex - 1, switchSlide = true)) >>
                        Monad[F].pure(Either.left(currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear() >>
                        stateRef.set(ExecutorState(currentIndex, switchSlide = true)) >>
                        Monad[F].pure(Either.left(currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    current.slide.stopShow >>
                      currentWork.cancel >>
                      NConsole[F].clear() >>
                      Monad[F].pure(Either.right((currentIndex, true, currentWork)))
                  case _ =>
                    val cur = presentation.slideSpecifications(currentIndex)
                    cur.slide.userInput(userInput) >>
                      stateRef.get.flatMap { latestState =>
                        if (latestState.switchSlide) {
                          // External navigation (e.g. QuickNavigation) has requested a slide change
                          cur.slide.stopShow >>
                            currentWork.cancel >>
                            NConsole[F].clear() >>
                            Monad[F].pure(Either.left(currentWork))
                        } else {
                          stateRef.set(ExecutorState(currentIndex, switchSlide = false)) >>
                            Monad[F].pure(Either.left(currentWork))
                        }
                      }
                }
              } yield result
            }
          }
        }
      }
    }
  }
}

