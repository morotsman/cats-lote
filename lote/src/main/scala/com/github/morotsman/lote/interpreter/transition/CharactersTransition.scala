package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.{ScreenAdjusted, make}
import com.github.morotsman.lote.model.Screen

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random


case class CharacterPosition(character: Char, inTransition: Boolean, canTransform: Boolean, tick: Int = 0)

case class Position(characters: List[CharacterPosition])

object CharactersTransition {

  def apply[F[_] : Temporal](
                              gravity: Double = 1.2,
                              selectAccelerator: Double = 1.1,
                              timeBetweenTicks: FiniteDuration = 40.milli,
                              setupPosition: (Char, Char) => List[CharacterPosition],
                              getNewIndex: (Screen, Int, CharacterPosition) => Int
                            ): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      def setupPositions(from: ScreenAdjusted, to: ScreenAdjusted): List[Position] =
        from.content.zip(to.content).map { case (from, to) => if (from == '\n') {
          Position(List(
            CharacterPosition(from, inTransition = false, canTransform = false)
          ))
        } else {
          Position(setupPosition(from, to))
        }
        }.toList

      def transformPositions(
                              screen: Screen,
                              currentCharacterPositions: List[Position],
                              positionsToUpdate: List[Int]
                            ): List[Position] = {
        val toUpdate = currentCharacterPositions.toArray

        // mark positions to transform
        if (positionsToUpdate.nonEmpty) {
          positionsToUpdate.foreach { randomPosition =>
            currentCharacterPositions.get(randomPosition).foreach { position =>
              val markedAsMoving = position.copy(characters = position.characters.map(cp => if (cp.canTransform) {
                cp.copy(inTransition = true)
              } else {
                cp
              }
              ))
              toUpdate(randomPosition) = markedAsMoving
            }
          }
        }

        // transform positions
        currentCharacterPositions.zipWithIndex
          .filter { case (position, _) =>
            position.characters.exists(_.inTransition)
          }
          .foreach { case (position, index) =>
            // take them away from the old position
            toUpdate(index) = toUpdate(index).copy(characters = toUpdate(index).characters.filter(!_.inTransition))

            // move to new position
            val toMove = position.characters.filter(_.inTransition)
            toMove.foreach { cp =>
              val newIndex = getNewIndex(screen, index, cp);
              val updatedPosition = cp.copy(tick = cp.tick + 1)
              if (
                newIndex < toUpdate.length &&
                  newIndex >=  0 &&
                  !toUpdate(newIndex).characters.exists(_.character == '\n')) {
                toUpdate(newIndex) = toUpdate(newIndex).copy(characters = updatedPosition :: toUpdate(newIndex).characters)
              }
            }
          }

        toUpdate.toList
      }

      def transformSlides(
                           screen: Screen,
                           positions: List[Position],
                           randomPositions: List[Int],
                           nrUnderTransformation: Double = 1.0
                         ): F[Unit] = {
        if (positions.forall(_.characters.forall(_.canTransform == false))) {
          console.clear()
        } else {
          val positionsToUpdate = randomPositions.take(nrUnderTransformation.toInt);
          val newRandomPositions = randomPositions.drop(nrUnderTransformation.toInt)
          val updatedPositions = transformPositions(screen, positions, positionsToUpdate)
          console.clear() >>
            console.writeString(
              ScreenAdjusted(
                updatedPositions.map(_.characters.headOption.map(_.character).getOrElse("")).mkString(""),
                screen.screenWidth,
                screen.screenHeight
              )
            ) >>
            Temporal[F].sleep(timeBetweenTicks) >>
            transformSlides(screen, updatedPositions, newRandomPositions, nrUnderTransformation * selectAccelerator)
        }
      }

      for {
        slide1 <- from.content(console)
        slide2 <- to.content(console)
        positions = setupPositions(slide1, slide2)
        randomPositions = Random.shuffle(positions.indices.toList)
        _ <- console.writeString(slide1) >>
          transformSlides(Screen(slide1.width, slide1.height), positions, randomPositions) >>
          console.writeString(slide2)
      } yield ()
    }
  }

}