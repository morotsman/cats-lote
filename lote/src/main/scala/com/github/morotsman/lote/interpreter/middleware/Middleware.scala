package com.github.morotsman.lote.interpreter.middleware

import cats._
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.algebra.{Middleware, NConsole, Overlay}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.model.{Alignment, Screen, ScreenAdjusted, UserInput}

case class MiddlewareState[F[_]](
                                  overlays: List[Overlay[F]]
                                )

object Middleware {
  def make[F[_] : Monad : Ref.Make : NConsole](): F[Middleware[F]] =
    Ref[F].of(MiddlewareState[F](List.empty)).map { state =>
      new Middleware[F] {

        override def addOverlays(overlays: List[Overlay[F]]): F[Unit] = {
          state.set(MiddlewareState(overlays))
        }

        private def applyMiddleware(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = for {
          middleWare <- state.get
          c <- context
          result <- {
            middleWare.overlays.foldLeft(Monad[F].pure(screenAdjusted)) { case (fsa, o) =>
              fsa.flatMap(sa => o.applyOverlay(c, sa))
            }
          }
        } yield result

        override def read(timeoutInMillis: Long): F[UserInput] =
          NConsole[F].read()

        override def read(): F[UserInput] =
          NConsole[F].read(0L)

        override def readInterruptible(): F[UserInput] =
          NConsole[F].readInterruptible()

        override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] =
          NConsole[F].alignText(s: String, alignment: Alignment)

        override def writeString(s: ScreenAdjusted): F[Unit] = for {
          withOverlay <- applyMiddleware(s)
          _ <- NConsole[F].writeString(withOverlay)
        } yield ()

        override def clear(): F[Unit] =
          NConsole[F].clear()

        override def context: F[Screen] = NConsole[F].context
      }
    }


}


