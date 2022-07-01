package com.github.morotsman.lote.interpreter.middleware

import cats._
import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.algebra.{Middleware, NConsole, Overlay}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, UserInput}

case class MiddlewareState[F[_]](
                                  overlays: List[Overlay[F]]
                                )

object Middleware {
  def make[F[_] : Monad](state: Ref[F, MiddlewareState[F]], console: NConsole[F]): Middleware[F] = new Middleware[F] {

    override def addOverlays(overlays: List[Overlay[F]]): F[Unit] = {
      state.set(MiddlewareState(overlays))
    }

    override def applyMiddleware(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = for {
      middleWare <- state.get
      result <- {
        middleWare.overlays.foldLeft(Monad[F].pure(screenAdjusted)) { case (fsa, o) =>
          fsa.flatMap(sa => o.applyOverlay(sa))
        }
      }
    } yield result

    override def read(): F[UserInput] =
      console.read()

    override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] =
      console.alignText(s: String, alignment: Alignment)

    override def writeString(s: String, alignment: Alignment): F[Unit] =
      console.writeString(s, alignment)

    override def writeString(s: ScreenAdjusted): F[Unit] = for {
      withOverlay <- applyMiddleware(s)
      _ <- console.writeString(withOverlay)
    } yield ()

    override def clear(): F[Unit] =
      console.clear()
  }
}


