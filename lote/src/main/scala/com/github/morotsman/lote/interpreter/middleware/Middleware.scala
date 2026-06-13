package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.algebra.{IdleDetector, Middleware, NConsole, Overlay, Ticker, TickerSubscription}
import com.github.morotsman.lote.model.{Alignment, Screen, ScreenAdjusted, UserInput}

case class MiddlewareState[F[_]](
                                  overlays: List[Overlay[F]],
                                  lastContent: Option[ScreenAdjusted] = None,
                                  lastRendered: Option[String] = None,
                                  subscription: Option[TickerSubscription[F]] = None
                                )

object Middleware {
  def make[F[_] : Monad : Ref.Make](
                                      console: NConsole[F],
                                      ticker: Ticker[F],
                                      idleDetector: IdleDetector[F]
                                    ): F[Middleware[F]] =
    Ref[F].of(MiddlewareState[F](List.empty)).map { state =>
      new Middleware[F] {

        override def addOverlays(overlays: List[Overlay[F]]): F[Unit] = {
          state.update(_.copy(overlays = overlays))
        }

        private def notifyContentChange(content: ScreenAdjusted): F[Unit] =
          idleDetector.onContentChange(content.content)

        private def notifyKeyPress(input: UserInput): F[Unit] =
          idleDetector.onKeyPress(input)

        private def applyMiddleware(screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = for {
          middleWare <- state.get
          c <- context
          _ <- notifyContentChange(screenAdjusted)
          result <- {
            middleWare.overlays.foldLeft(Monad[F].pure(screenAdjusted)) { case (fsa, o) =>
              fsa.flatMap(sa => o.applyOverlay(c, sa))
            }
          }
        } yield result

        private val onTick: F[Unit] = for {
          s <- state.get
          _ <- s.lastContent.traverse_ { content =>
            for {
              withOverlay <- applyMiddleware(content)
              currentState <- state.get
              contentStillCurrent = currentState.lastContent.contains(content)
              _ <- if (contentStillCurrent && !currentState.lastRendered.contains(withOverlay.content)) {
                console.writeString(withOverlay) *>
                  state.update(_.copy(lastRendered = Some(withOverlay.content)))
              } else Monad[F].unit
            } yield ()
          }
        } yield ()

        private def ensureSubscribed(): F[Unit] = for {
          s <- state.get
          _ <- if (s.subscription.isEmpty) {
            for {
              sub <- ticker.subscribe(onTick)
              _ <- state.update(_.copy(subscription = Some(sub)))
              _ <- ticker.start
            } yield ()
          } else Monad[F].unit
        } yield ()

        override def read(timeoutInMillis: Long): F[UserInput] = for {
          input <- console.read()
          _ <- notifyKeyPress(input)
        } yield input

        override def read(): F[UserInput] = for {
          input <- console.read(0L)
          _ <- notifyKeyPress(input)
        } yield input

        override def readInterruptible(): F[UserInput] =
          console.readInterruptible()

        override def alignText(s: String, alignment: Alignment): F[ScreenAdjusted] =
          console.alignText(s: String, alignment: Alignment)

        override def writeString(s: ScreenAdjusted): F[Unit] = for {
          _ <- state.update(_.copy(lastContent = Some(s)))
          _ <- ensureSubscribed()
          withOverlay <- applyMiddleware(s)
          _ <- console.writeString(withOverlay)
          _ <- state.update(_.copy(lastRendered = Some(withOverlay.content)))
        } yield ()

        override def clear(): F[Unit] =
          console.clear()

        override def context: F[Screen] = console.context
      }
    }

}
