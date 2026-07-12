package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, Screen, UserInput}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}

private[lote] object ReplaceTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      replace: Char
  )(implicit animationSettings: AnimationSettings): Transition[F] =
    create(replace, NConsole[F], Ticker[F], animationSettings)

  def create[F[_]: Temporal: Ref.Make](
      replace: Char,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Transition[F] = new Transition[F] {

    def setupPosition(
        fromCharacter: Char,
        toCharacter: Char
    ): List[CharacterPosition] = {
      val _ = toCharacter
      List(
        CharacterPosition(
          fromCharacter,
          inTransition = false,
          canTransform = true
        ),
        CharacterPosition(replace, inTransition = false, canTransform = false)
      )
    }

    def getNewIndex(
        _screen: Screen,
        _currentIndex: Int,
        _cp: CharacterPosition
    ): Option[Int] = {
      val _ = (_screen, _currentIndex, _cp)
      None
    }

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      CharactersTransition.create(
        selectAccelerator = 1.3,
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

