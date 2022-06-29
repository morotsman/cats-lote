package com.github.morotsman.lote.interpreter.middleware

import cats._
import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.algebra.{Middleware, Overlay}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

case class MiddlewareState[F[_]](
                                  overlays: List[Overlay[F]]
                                )

object Middleware {
  def make[F[_] : Monad](state: Ref[F, MiddlewareState[F]]): Middleware[F] = new Middleware[F] {

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
  }
}


