package com.github.morotsman.lote.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.Fiber
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, PresentationExecutor}
import com.github.morotsman.lote.model._


object PresentationExecutorInterpreter2 {

  def make[F[_] : Temporal](console: NConsole[F], presentation: Presentation[F]): F[PresentationExecutor[F]] = Monad[F].pure(
    new PresentationExecutor[F] {
      override def start(): F[Unit] = for {
        _ <- console.clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
        Monad[F].unit.start.flatMap { initialWork =>
          Monad[F].tailRecM((0, true, initialWork)) {
            case (currentIndex, switchSlide, work) =>
              val current = presentation.slideSpecifications(currentIndex)

              for {
                currentWork <- if (switchSlide) {
                  current.slide.startShow(console).start
                } else {
                  Monad[F].pure(work)
                }
                userInput <- console.read()
                result <- userInput match {
                  case Key(k) if k == SpecialKey.Right =>
                    if (currentIndex < presentation.slideSpecifications.length - 1) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        console.clear() >>
                        current.out.fold(Monad[F].unit) { t =>
                          Temporal[F].race(
                            t.transition(current.slide, presentation.slideSpecifications(currentIndex + 1).slide)(console),
                            console.readInterruptible()
                          ).as(Monad[F].unit)
                        }.as(Either.left(currentIndex + 1, true, currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        console.clear().as(Either.left(currentIndex, true, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Left =>
                    if (currentIndex > 0) {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        console.clear().as(Either.left(currentIndex - 1, true, currentWork))
                    } else {
                      current.slide.stopShow >>
                        currentWork.cancel >>
                        console.clear().as(Either.left(currentIndex, true, currentWork))
                    }
                  case Key(k) if k == SpecialKey.Esc =>
                    current.slide.stopShow >>
                      currentWork.cancel >>
                      console.clear() >>
                      presentation.exitSlide.fold(Monad[F].unit)(_.startShow(console)) >>
                      console.clear().as(Either.right(currentIndex, true, currentWork))
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
