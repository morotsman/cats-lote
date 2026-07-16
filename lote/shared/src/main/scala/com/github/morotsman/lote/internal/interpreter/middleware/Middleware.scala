package com.github.morotsman.lote.internal.interpreter.middleware

import cats.Monad
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.api.{Alignment, PlatformCapability, RenderEffect, Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.internal.algebra.Middleware
import com.github.morotsman.lote.api.spi.{NConsole, Overlay, Ticker, TickerSubscription}

case class MiddlewareState[F[_]](
    overlays: List[Overlay[F]],
    lastContent: Option[ScreenAdjusted] = None,
    lastRendered: Option[String] = None,
    subscription: Option[TickerSubscription[F]] = None
)

private[lote] object Middleware {
  def make[F[_]: Monad: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F]
  ): F[Middleware[F]] =
    Ref[F].of(MiddlewareState[F](List.empty)).map { state =>
      new Middleware[F] {

        override def addOverlays(overlays: List[Overlay[F]]): F[Unit] = {
          state.update(_.copy(overlays = overlays))
        }

        private def notifyKeyPress(input: UserInput): F[Unit] = for {
          middleWare <- state.get
          _ <- middleWare.overlays.traverse_(_.onUserInput(input))
        } yield ()

        private def applyMiddleware(
            screenAdjusted: ScreenAdjusted
        ): F[ScreenAdjusted] = for {
          middleWare <- state.get
          c <- context
          result <- {
            middleWare.overlays.foldLeft(Monad[F].pure(screenAdjusted)) { case (fsa, o) =>
              fsa.flatMap(sa => o.applyOverlay(c, sa, screenAdjusted))
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
              _ <-
                if (
                  contentStillCurrent && !currentState.lastRendered.contains(
                    withOverlay.content
                  )
                ) {
                  console.writeString(withOverlay) *>
                    state.update(
                      _.copy(lastRendered = Some(withOverlay.content))
                    )
                } else Monad[F].unit
            } yield ()
          }
        } yield ()

        private def ensureSubscribed(): F[Unit] = for {
          s <- state.get
          _ <-
            if (s.subscription.isEmpty) {
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

        override def alignText(
            s: String,
            alignment: Alignment
        ): F[ScreenAdjusted] =
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

        override def close(): F[Unit] =
          console.close()

        override def context: F[Screen] = console.context

        override def capabilities: Set[PlatformCapability] = console.capabilities

        override def applyEffect(effect: RenderEffect): F[Unit] = console.applyEffect(effect)
      }
    }

}
