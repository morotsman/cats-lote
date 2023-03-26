package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class CharacterPosition(character: Char, accelerator: Double, moving: Boolean, movable: Boolean)

case class Position(characters: List[CharacterPosition])

object FallingCharactersTransition {
  def apply[F[_] : Temporal](gravity: Double = 1.2, selectAccelerator: Double = 1.1): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      def getCharacterPositions(from: ScreenAdjusted): List[Position] =
        from.content.map(c => if (c == '\n' || c == ' ') {
          Position(List(
            CharacterPosition(c, accelerator = 1.0, moving = false, movable = false)
          ))
        } else {
          Position(List(
            CharacterPosition(c, accelerator = 1.0, moving = false, movable = true),
            CharacterPosition(' ', accelerator = 1.0, moving = false, movable = false)
          ))
        }).toList

      def falling(characterPositions: List[Position], screenWidth: Int, numberOfFalling: Int, notFalling: List[Int]): (List[Position], List[Int]) = {
        val toChange = characterPositions.toArray

        // select characters to fall
        if (notFalling.nonEmpty) {
          val randomPositions = notFalling.take(numberOfFalling)
          randomPositions.foreach { randomPosition =>
            characterPositions.get(randomPosition).foreach { position =>
              val tmp: Position = position.copy(characters = position.characters.map(cp => if (cp.movable) cp.copy(moving = true) else cp))
              toChange(randomPosition) = tmp
            }
          }
        }

        // Move all falling characters
        val falling: Array[(Position, Int)] = toChange.zipWithIndex.filter(pi => pi._1.characters.exists(_.moving))
        falling.foreach { pi =>
          val toMove = pi._1.characters.filter(_.moving).map(cp => cp.copy(accelerator = cp.accelerator * gravity)) // TODO get from outside?
          // take them away from the old position
          toChange(pi._2) = toChange(pi._2).copy(characters = toChange(pi._2).characters.filter(!_.moving))
          // move to new position
          toMove.foreach { cp =>
            val newIndex = pi._2 + (screenWidth + 1) * cp.accelerator.toInt
            if (newIndex < toChange.length) {
              toChange(newIndex) = toChange(newIndex).copy(characters = cp :: toChange(newIndex).characters)
            }
          }
        }

        (toChange.toList, notFalling.drop(numberOfFalling))
      }

      def fall(screenHeight: Int, screenWidth: Int, positions: List[Position], numberOfFalling: Double, notFalling: List[Int]): F[Unit] = {
        if (positions.forall(_.characters.forall(c => c.character == ' ' || c.character == '\n'))) {
          console.clear()
        } else {
          val (newPositions, newNotFalling) = falling(positions, screenWidth, numberOfFalling.toInt, notFalling)
          console.clear() >>
            //TODO snygga till .head
            console.writeString(ScreenAdjusted(newPositions.map(_.characters.head.character).mkString(""), screenWidth, screenHeight)) >>
            Temporal[F].sleep(40.milli) >>
            fall(screenHeight, screenWidth, newPositions, numberOfFalling * selectAccelerator, newNotFalling)
        }
      }

      for {
        slide1 <- from.content(console)
        slide2 <- to.content(console)
        characterPositions = getCharacterPositions(slide1)
        notFalling = Random.shuffle(characterPositions.indices.toList)
        _ <- console.writeString(slide1) >> fall(slide1.height, slide1.width, characterPositions, 1, notFalling) >> console.writeString(slide2)
      } yield ()

    }
  }


}
