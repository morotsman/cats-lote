package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.{ScreenAdjusted, make}

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Falling(accelerator: Double)

case class CharacterPosition[A](character: Char, moving: Boolean, movable: Boolean, meta: A)

case class Position(characters: List[CharacterPosition[Falling]])

object FallingCharactersTransition {

  def apply[F[_] : Temporal](gravity: Double = 1.2, selectAccelerator: Double = 1.1): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      // TODO should be provided
      def setupPositions(from: ScreenAdjusted, to: ScreenAdjusted): List[Position] =
        from.content.zip(to.content).map { case (from, to) => if (from == '\n') {
          Position(List(
            CharacterPosition(from, moving = false, movable = false, Falling(accelerator = 1.0))
          ))
        } else {
          Position(List(
            CharacterPosition(from, moving = false, movable = true, Falling(accelerator = 1.0)),
            CharacterPosition(' ', moving = false, movable = false, Falling(accelerator = 1.0))
          ))
        }
        }.toList

      // TODO should be provided
      def updateCharacterPosition(screenWidth: Int, currentIndex: Int, cp: CharacterPosition[Falling]): (Int, CharacterPosition[Falling]) = {
        val acceleration = cp.meta.accelerator * gravity
        val newIndex = currentIndex + (screenWidth + 1) * acceleration.toInt
        (newIndex, cp.copy(meta = cp.meta.copy(accelerator = acceleration)))
      }


      def transformPositions(
                              screenWidth: Int,
                              currentCharacterPositions: List[Position],
                              positionsToUpdate: List[Int]
                            ): List[Position] = {
        val toUpdate = currentCharacterPositions.toArray

        // mark positions to transform
        if (positionsToUpdate.nonEmpty) {
          positionsToUpdate.foreach { randomPosition =>
            currentCharacterPositions.get(randomPosition).foreach { position =>
              val markedAsMoving = position.copy(characters = position.characters.map(cp => if (cp.movable) {
                cp.copy(moving = true)
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
            position.characters.exists(_.moving)
          }
          .foreach { case (position, index) =>
            // take them away from the old position
            toUpdate(index) = toUpdate(index).copy(characters = toUpdate(index).characters.filter(!_.moving))

            // move to new position
            val toMove = position.characters.filter(_.moving)
            toMove.foreach { cp =>
              val (newIndex, updatedPosition) = updateCharacterPosition(screenWidth, index, cp);
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
        if (positions.forall(_.characters.forall(_.movable == false))) {
          console.clear()
        } else {
          val positionsToUpdate = randomPositions.take(nrUnderTransformation.toInt);
          val newRandomPositions = randomPositions.drop(nrUnderTransformation.toInt)
          val updatedPositions = transformPositions(screenWidth, positions, positionsToUpdate)
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
