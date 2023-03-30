package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model.Screen


object ReplaceTransition2 {

  def apply[F[_] : Temporal](replace: Char): Transition[F] = new Transition[F] {

    def setupPosition(fromCharacter: Char, toCharacter: Char): List[CharacterPosition] = List(
      CharacterPosition(fromCharacter, inTransition = false, canTransform = true),
      CharacterPosition(replace, inTransition = false, canTransform = false)
    )

    def getNewIndex(screen: Screen, currentIndex: Int, cp: CharacterPosition): Option[Int] =
      None

    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {
      CharactersTransition(
        selectAccelerator = 1.3,
        setupPosition = setupPosition,
        getNewIndex = getNewIndex
      ).transition(from, to)(console)

    }

  }
}
