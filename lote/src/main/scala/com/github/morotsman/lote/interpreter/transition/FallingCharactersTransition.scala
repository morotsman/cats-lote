package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.{ScreenAdjusted, make}

import scala.concurrent.duration.DurationInt
import scala.util.Random


case class CharacterPosition(character: Char, inTransition: Boolean, canTransform: Boolean, tick: Int)

case class Position(characters: List[CharacterPosition])

object FallingCharactersTransition {

  def apply[F[_] : Temporal](gravity: Double = 1.2, selectAccelerator: Double = 1.1): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      // TODO should be provided
      def setupPositions(from: ScreenAdjusted, to: ScreenAdjusted): List[Position] =
        from.content.zip(to.content).map { case (from, to) => if (from == '\n') {
          Position(List(
            CharacterPosition(from, inTransition = false, canTransform = false, tick = 0)
          ))
        } else {
          Position(List(
            CharacterPosition(from, inTransition = false, canTransform = true, tick = 0),
            CharacterPosition(' ', inTransition = false, canTransform = false, tick = 0)
          ))
        }
        }.toList

      // TODO should be provided
      def getNewIndex(screenWidth: Int, screenHeight: Int, currentIndex: Int, cp: CharacterPosition): Int = {
        val acceleration = cp.tick * gravity
        currentIndex + (screenWidth + 1) * acceleration.toInt
      }


      def transformPositions(
                              screenWidth: Int,
                              screenHeight: Int,
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
              val newIndex = getNewIndex(screenWidth, screenHeight, index, cp);
              val updatedPosition = cp.copy(tick = cp.tick + 1)
              if (newIndex < toUpdate.length) {
                toUpdate(newIndex) = toUpdate(newIndex).copy(characters = updatedPosition :: toUpdate(newIndex).characters)
              }
            }
          }

        toUpdate.toList
      }

      def transformSlides(
                           screenHeight: Int,
                           screenWidth: Int,
                           positions: List[Position],
                           randomPositions: List[Int],
                           nrUnderTransformation: Double = 1.0
                         ): F[Unit] = {
        if (positions.forall(_.characters.forall(_.canTransform == false))) {
          console.clear()
        } else {
          val positionsToUpdate = randomPositions.take(nrUnderTransformation.toInt);
          val newRandomPositions = randomPositions.drop(nrUnderTransformation.toInt)
          val updatedPositions = transformPositions(screenWidth, screenHeight, positions, positionsToUpdate)
          console.clear() >>
            console.writeString(
              ScreenAdjusted(updatedPositions.map(_.characters.headOption.map(_.character).getOrElse("")).mkString(""),
                screenWidth,
                screenHeight
              )
            ) >>
            Temporal[F].sleep(40.milli) >>
            transformSlides(screenHeight, screenWidth, updatedPositions, newRandomPositions, nrUnderTransformation * selectAccelerator)
        }
      }

      for {
        slide1 <- from.content(console)
        slide2 <- to.content(console)
        positions = setupPositions(slide1, slide2)
        randomPositions = Random.shuffle(positions.indices.toList)
        _ <- console.writeString(slide1) >>
          transformSlides(slide1.height, slide1.width, positions, randomPositions) >>
          console.writeString(slide2)
      } yield ()
    }
  }

}
