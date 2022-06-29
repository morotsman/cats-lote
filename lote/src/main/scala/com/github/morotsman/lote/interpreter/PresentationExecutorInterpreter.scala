package com.github.morotsman
package lote.interpreter

import cats.Monad
import cats.effect.implicits._
import cats.effect.{Fiber, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, PresentationExecutor}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.model.{Key, Presentation, SpecialKey}

object PresentationExecutorInterpreter {
  def make[F[_] : Temporal : NConsole](presentation: Presentation[F]): F[PresentationExecutor[F]] = Monad[F].pure(
    new PresentationExecutor[F] {
      override def start(): F[Unit] = for {
        _ <- NConsole[F].clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Fiber[F, Throwable, Unit])] =
        presentation.slideSpecifications.head.slide.startShow().start >>=
          (Monad[F].tailRecM(0, _) { case (currentIndex, currentWork) =>
            def shiftSlide(toIndex: Int): F[Fiber[F, Throwable, Unit]] = {
              val current = presentation.slideSpecifications(currentIndex)
              for {
                _ <- current.slide.stopShow()
                _ <- currentWork.cancel
                _ <- NConsole[F].clear()
                next = presentation.slideSpecifications(toIndex)
                work <- {
                  (for {
                    _ <- current.out.fold(Monad[F].unit) {
                      _.transition(current.slide, next.slide)
                    }
                    _ <- NConsole[F].clear()
                    _ <- next.in.fold(Monad[F].unit) {
                      _.transition(current.slide, next.slide)
                    }
                    _ <- NConsole[F].clear()
                    _ <- next.slide.startShow().start
                  } yield ()).start
                }
              } yield work
            }

            for {
              input <- NConsole[F].read()
              result <- {
                input match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentIndex < presentation.slideSpecifications.length - 1) {
                      val nextIndex = currentIndex + 1
                      shiftSlide(nextIndex).map(Either.left(nextIndex, _))
                    } else {
                      Monad[F].pure(Either.left(currentIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentIndex > 0) {
                      val nextIndex = currentIndex - 1
                      shiftSlide(nextIndex).map(Either.left(nextIndex, _))
                    } else {
                      Monad[F].pure(Either.left(currentIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    val current = presentation.slideSpecifications(currentIndex)
                    current.slide.stopShow() >>
                      NConsole[F].clear() >>
                      presentation.exitSlide.fold(Monad[F].unit)(_.startShow()) >>
                      NConsole[F].clear().as(Either.right(currentIndex, currentWork))
                  case _ =>
                    val current = presentation.slideSpecifications(currentIndex)
                    current.slide.userInput(input) >>
                      Monad[F].pure(Either.left(currentIndex, currentWork))
                }
              }
            } yield result
          })

    }
  )
}
