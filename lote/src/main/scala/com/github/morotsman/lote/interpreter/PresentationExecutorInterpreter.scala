package com.github.morotsman
package lote.interpreter

import cats.Monad
import cats.effect.implicits._
import cats.effect.{Fiber, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, PresentationExecutor}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.transition.Nothing
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
          (Monad[F].tailRecM(0, _) { case (currentSlideIndex, currentWork) =>
            def shiftSlide(toIndex: Int) = {
              val current = presentation.slideSpecifications(currentSlideIndex)
              for {
                _ <- current.slide.stopShow()
                _ <- currentWork.cancel
                _ <- NConsole[F].clear()
                next = presentation.slideSpecifications(toIndex)
                work <- {
                  (for {
                    _ <- current.out.fold(Nothing().transition(current.slide, next.slide)) {
                      _.transition(current.slide, next.slide)
                    }
                    _ <- NConsole[F].clear()
                    _ <- next.in.fold(Nothing().transition(current.slide, next.slide)) {
                      _.transition(current.slide, next.slide)
                    }
                    _ <- NConsole[F].clear()
                    _ <- next.slide.startShow().start
                  } yield ()).start
                }
              } yield Either.left(toIndex, work)
            }

            for {
              input <- NConsole[F].read()
              result <- {
                input match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentSlideIndex < presentation.slideSpecifications.length - 1) {
                      shiftSlide(currentSlideIndex + 1)
                    } else {
                      Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentSlideIndex > 0) {
                      shiftSlide(currentSlideIndex - 1)
                    } else {
                      Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    val current = presentation.slideSpecifications(currentSlideIndex)
                    current.slide.stopShow() >>
                      NConsole[F].clear() >>
                      presentation.exitSlide.fold(Monad[F].unit)(_.startShow()).as(Either.right(currentSlideIndex, currentWork))
                  case _ =>
                    val current = presentation.slideSpecifications(currentSlideIndex)
                    current.slide.userInput(input) >>
                      Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                }
              }
            } yield result
          })

    }
  )
}
