package com.github.morotsman.lote.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.Fiber
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, PresentationExecutor}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.model._


object PresentationExecutorInterpreter {

  def make[F[_] : Temporal: NConsole](presentation: Presentation[F]): F[PresentationExecutor[F]] = Monad[F].pure(
    new PresentationExecutor[F] {
      override def start(): F[Unit] = for {
        _ <- NConsole[F].clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
        Monad[F].unit.start.flatMap { initialWork =>
          Monad[F].tailRecM((0, true, initialWork)) {
            case (currentIndex, switchSlide, work) =>
              val current = presentation.slideSpecifications(currentIndex)

              for {
                currentWork <- if (switchSlide) {
                  current.slide.startShow.start
                } else {
                  Monad[F].pure(work)
                }
                userInput <- NConsole[F].read()
                result <- userInput match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentIndex < presentation.slideSpecifications.length - 1) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear() >>
                        current.out.fold(Monad[F].unit) { t =>
                          Temporal[F].race(
                            t.transition(current.slide, presentation.slideSpecifications(currentIndex + 1).slide),
                            NConsole[F].readInterruptible()
                          ).as(Monad[F].unit)
                        }.as(Either.left(currentIndex + 1, true, currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear().as(Either.left(currentIndex, true, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentIndex > 0) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear().as(Either.left(currentIndex - 1, true, currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        NConsole[F].clear().as(Either.left(currentIndex, true, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    current.slide.stopShow >>
                      currentWork.cancel >>
                      NConsole[F].clear() >>
                      presentation.exitSlide.fold(Monad[F].unit)(_.startShow) >>
                      NConsole[F].clear().as(Either.right(currentIndex, true, currentWork))
                  case _ =>
                    val current = presentation.slideSpecifications(currentIndex)
                    current.slide.userInput(userInput) >>
                      Monad[F].pure(Either.left(currentIndex, false, currentWork))
                }
              }
              yield result
          }

        }

      }

    })
}
