package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{AnimationSettings, Screen, UserInput}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, Transition}

private[lote] object FallingCharactersTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2
  )(implicit animationSettings: AnimationSettings): Transition[F] =
    create(
      gravity = gravity,
      selectAccelerator = selectAccelerator,
      console = NConsole[F],
      ticker = Ticker[F],
      animationSettings = animationSettings
    )

  def create[F[_]: Temporal: Ref.Make](
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2,
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
        CharacterPosition(' ', inTransition = false, canTransform = false)
      )
    }

    def getNewIndex(
        screen: Screen,
        currentIndex: Int,
        cp: CharacterPosition
    ): Option[Int] =
      if (cp.character == ' ') {
        None
      } else {
        val acceleration = cp.tick * gravity
        Some(currentIndex + (screen.screenWidth + 1) * acceleration.toInt)
      }

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      CharactersTransition.create(
        selectAccelerator = selectAccelerator,
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

