package com.github.morotsman.lote.interpreter.transition

import cats.Monad
import cats.effect.{Concurrent, Ref}
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.model.{Screen, UserInput}

object FallingCharactersTransition {

  def apply[F[_]: Concurrent: Ref.Make: NConsole: Ticker](
      gravity: Double = 1.2,
      selectAccelerator: Double = 1.2
  ): Transition[F] = new Transition[F] {

    def setupPosition(
        fromCharacter: Char,
        toCharacter: Char
    ): List[CharacterPosition] = List(
      CharacterPosition(
        fromCharacter,
        inTransition = false,
        canTransform = true
      ),
      CharacterPosition(' ', inTransition = false, canTransform = false)
    )

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
      CharactersTransition(
        selectAccelerator = selectAccelerator,
        setupPosition = setupPosition,
        getNewIndex = getNewIndex
      ).transition(from, to)

    }

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }
}
