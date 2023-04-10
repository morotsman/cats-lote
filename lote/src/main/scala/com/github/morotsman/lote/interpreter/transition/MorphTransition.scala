package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model.Screen

import scala.concurrent.duration.{DurationInt, FiniteDuration}


object MorphTransition {

  def apply[F[_] : Temporal: NConsole](): Transition[F] = new Transition[F] {

    def setupPosition(fromCharacter: Char, toCharacter: Char): List[CharacterPosition] = List(
      CharacterPosition(fromCharacter, inTransition = false, canTransform = true),
      CharacterPosition(toCharacter, inTransition = false, canTransform = false)
    )

    def getNewIndex(screen: Screen, currentIndex: Int, cp: CharacterPosition): Option[Int] =
      None

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      CharactersTransition(
        selectAccelerator = 1.5,
        setupPosition = setupPosition,
        getNewIndex = getNewIndex
      ).transition(from, to)

    }

  }
}
