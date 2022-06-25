package com.github.morotsman
package lote.interpreter

import cats.effect.implicits._
import cats.implicits._
import lote.algebra.{NConsole, PresentationExecutor}
import lote.model.{Key, Presentation, SpecialKey}
import lote.slides.Bye
import lote.interpreter.transition.Nothing

import cats.Monad
import cats.effect.{Fiber, Temporal}

import scala.concurrent.duration.DurationInt

object PresentationExecutorInterpreter {
  def make[F[_] : Temporal : NConsole]
  (presentation: Presentation[F]): F[PresentationExecutor[F]] = Monad[F].pure(
    new PresentationExecutor[F] {
      override def start(): F[Unit] = for {
        _ <- NConsole[F].clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Fiber[F, Throwable, Unit])] =
        presentation.slideSpecifications.head.slide.startShow().start >>= (Monad[F].tailRecM(0, _) { case (currentSlideIndex, currentWork) =>
          for {
            input <- NConsole[F].read()
            result <- {
              val currentSat = presentation.slideSpecifications(currentSlideIndex)
              input match {
                case Key(k) if k == SpecialKey.Right =>
                  if (currentSlideIndex < presentation.slideSpecifications.length - 1) {
                    for {
                      _ <- currentSat.slide.stopShow()
                      _ <- currentWork.cancel
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex + 1
                      nextSat = presentation.slideSpecifications(index)
                      work <- {
                        (for {
                          _ <- currentSat.right.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                            right.transition(currentSat.slide, nextSat.slide)
                          }
                          _ <- NConsole[F].clear()
                          _ <- nextSat.left.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                            right.transition(currentSat.slide, nextSat.slide)
                          }
                          _ <- NConsole[F].clear()
                          _ <- nextSat.slide.startShow().start
                        } yield ()).start
                      }
                    } yield Either.left(index, work)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                  }
                case Key(k) if k == SpecialKey.Left =>
                  if (currentSlideIndex > 0) {
                    for {
                      _ <- currentSat.slide.stopShow()
                      _ <- currentWork.cancel
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex - 1
                      nextSat = presentation.slideSpecifications(index)
                      work <- (for {
                        _ <- currentSat.left.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                          right.transition(currentSat.slide, nextSat.slide)
                        }
                        _ <- NConsole[F].clear()
                        _ <- nextSat.right.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                          right.transition(currentSat.slide, nextSat.slide)
                        }
                        _ <- NConsole[F].clear()
                        _ <- nextSat.slide.startShow().start
                      } yield ()).start

                    } yield Either.left(index, work)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                  }
                case Key(k) if k == SpecialKey.Esc =>
                  currentSat.slide.stopShow() >>
                    NConsole[F].clear() >>
                    Bye[F].startShow() >>
                    Temporal[F].sleep(500.milli) >>
                    NConsole[F].clear().as(Either.right(currentSlideIndex, currentWork))
                case _ =>
                  currentSat.slide.userInput(input) >>
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
              }
            }
          } yield result
        })

    }
  )
}
