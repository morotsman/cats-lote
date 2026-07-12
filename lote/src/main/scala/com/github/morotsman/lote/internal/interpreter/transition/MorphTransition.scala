package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, Screen, UserInput}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}

private[lote] object MorphTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker]()(implicit animationSettings: AnimationSettings): Transition[F] =
    create(NConsole[F], Ticker[F], animationSettings)

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Transition[F] =
    new Transition[F] {

      def setupPosition(
          fromCharacter: Char,
          toCharacter: Char
      ): List[CharacterPosition] = List(
        CharacterPosition(
          fromCharacter,
          inTransition = false,
          canTransform = true
        ),
        CharacterPosition(
          toCharacter,
          inTransition = false,
          canTransform = false
        )
      )

      def getNewIndex(
          screen: Screen,
          currentIndex: Int,
          cp: CharacterPosition
      ): Option[Int] = {
        val _ = (screen, currentIndex, cp)
        None
      }

      override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
        CharactersTransition.create(
          selectAccelerator = 1.5,
          setupPosition = setupPosition,
          getNewIndex = getNewIndex,
          console = console,
          ticker = ticker,
          animationSettings = animationSettings
        ).transition(from, to)

      }

      override def userInput(input: UserInput): F[Unit] = Monad[F].unit

    }
}

