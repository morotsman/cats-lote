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
  def make[F[_] : Temporal](console: NConsole[F], presentation: Presentation[F]): F[PresentationExecutor[F]] = Monad[F].pure(
    new PresentationExecutor[F] {
      override def start(): F[Unit] = for {
        _ <- console.clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Fiber[F, Throwable, Unit])] =
        presentation.slideSpecifications.head.slide.startShow(console).start >>=
          (Monad[F].tailRecM(0, _) { case (currentIndex, currentWork) =>

            def shiftSlide(toIndex: Int, forward: Boolean): F[Fiber[F, Throwable, Unit]] = {
              val current = presentation.slideSpecifications(currentIndex)
              for {
                _ <- current.slide.stopShow
                _ <- currentWork.cancel
                _ <- console.clear()
                next = presentation.slideSpecifications(toIndex)
                work <- {
                  if (forward) {
                    (for {
                      _ <- current.out.fold(Monad[F].unit) {
                        _.transition(current.slide, next.slide)(console)
                      }
                      _ <- console.clear()
                      _ <- next.slide.startShow(console).start
                    } yield ()).start
                  } else {
                    next.slide.startShow(console).start
                  }
                }
              } yield work
            }

            for {
              input <- console.read()
              result <- {
                input match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentIndex < presentation.slideSpecifications.length - 1) {
                      val nextIndex = currentIndex + 1
                      shiftSlide(nextIndex, forward = true).map(Either.left(nextIndex, _))
                    } else {
                      Monad[F].pure(Either.left(currentIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentIndex > 0) {
                      val nextIndex = currentIndex - 1
                      shiftSlide(nextIndex, forward = false).map(Either.left(nextIndex, _))
                    } else {
                      Monad[F].pure(Either.left(currentIndex, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    val current = presentation.slideSpecifications(currentIndex)
                    current.slide.stopShow >>
                      console.clear() >>
                      presentation.exitSlide.fold(Monad[F].unit)(_.startShow(console)) >>
                      console.clear().as(Either.right(currentIndex, currentWork))
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
