package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.{ScreenAdjusted, make}

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Falling(accelerator: Double, moving: Boolean, movable: Boolean)

case class CharacterPosition[A](character: Char, meta: A)

case class Position(characters: List[CharacterPosition[Falling]])

object FallingCharactersTransition {

  def apply[F[_] : Temporal](gravity: Double = 1.2, selectAccelerator: Double = 1.1): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      def getCharacterPositions(from: ScreenAdjusted): List[Position] =
        from.content.map(c => if (c == '\n') {
          Position(List(
            CharacterPosition(c, Falling(accelerator = 1.0, moving = false, movable = false))
          ))
        } else {
          Position(List(
            CharacterPosition(c, Falling(accelerator = 1.0, moving = false, movable = true)),
            CharacterPosition(' ', Falling(accelerator = 1.0, moving = false, movable = false))
          ))
        }).toList

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
              val tmp: Position = position.copy(characters = position.characters.map(cp => if (cp.meta.movable) {
                cp.copy(meta = cp.meta.copy(moving = true))
              } else {
                cp
              }
              ))
              toUpdate(randomPosition) = tmp
            }
          }
        }

        // transform positions
        currentCharacterPositions.zipWithIndex.filter(pi => pi._1.characters.exists(_.meta.moving)).foreach { pi =>
          val toMove = pi._1.characters.filter(_.meta.moving).map {
            cp => cp.copy(meta = cp.meta.copy(accelerator = cp.meta.accelerator * gravity))
          }
          // take them away from the old position
          toUpdate(pi._2) = toUpdate(pi._2).copy(characters = toUpdate(pi._2).characters.filter(!_.meta.moving))
          // move to new position
          toMove.foreach { cp =>
            val newIndex = pi._2 + (screenWidth + 1) * cp.meta.accelerator.toInt
            if (newIndex < toUpdate.length) {
              toUpdate(newIndex) = toUpdate(newIndex).copy(characters = cp :: toUpdate(newIndex).characters)
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
        if (positions.forall(_.characters.forall(c => c.character == ' ' || c.character == '\n'))) {
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
        characterPositions = getCharacterPositions(slide1)
        randomPositions = Random.shuffle(characterPositions.indices.toList)
        _ <- console.writeString(slide1) >>
          transformSlides(slide1.height, slide1.width, characterPositions, randomPositions) >>
          console.writeString(slide2)
      } yield ()
    }
  }

}
