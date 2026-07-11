package com.github.morotsman.lote.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.model.{Screen, UserInput}

object MorphTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker]()(implicit animationSettings: AnimationSettings): Transition[F] =
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
      ): Option[Int] =
        None

      override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
        CharactersTransition(
          selectAccelerator = 1.5,
          setupPosition = setupPosition,
          getNewIndex = getNewIndex
        ).transition(from, to)

      }

      override def userInput(input: UserInput): F[Unit] = Monad[F].unit

    }
}
