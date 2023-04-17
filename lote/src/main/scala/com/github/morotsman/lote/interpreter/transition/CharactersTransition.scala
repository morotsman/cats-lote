package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted, UserInput}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random


case class CharacterPosition(character: Char, inTransition: Boolean, canTransform: Boolean, tick: Int = 0)

case class Position(characters: List[CharacterPosition])

object CharactersTransition {

  def apply[F[_] : Temporal: NConsole](
                              selectAccelerator: Double = 1.1,
                              timeBetweenTicks: FiniteDuration = 40.milli,
                              setupPosition: (Char, Char) => List[CharacterPosition],
                              getNewIndex: (Screen, Int, CharacterPosition) => Option[Int]
                            ): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

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
              newIndex.foreach { index =>
                val updatedPosition = cp.copy(tick = cp.tick + 1)
                if (
                  index < toUpdate.length &&
                    index >= 0 &&
                    !toUpdate(index).characters.exists(_.character == '\n')) {
                  toUpdate(index) = toUpdate(index).copy(characters = updatedPosition :: toUpdate(index).characters)
                }
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
          NConsole[F].clear()
        } else {
          val positionsToUpdate = randomPositions.take(nrUnderTransformation.toInt);
          val newRandomPositions = randomPositions.drop(nrUnderTransformation.toInt)
          val updatedPositions = transformPositions(screen, positions, positionsToUpdate)
          NConsole[F].clear() >>
            NConsole[F].writeString(
              ScreenAdjusted(
                updatedPositions.map(_.characters.headOption.map(_.character).getOrElse("")).mkString("")
              )
            ) >>
            Temporal[F].sleep(timeBetweenTicks) >>
            transformSlides(screen, updatedPositions, newRandomPositions, nrUnderTransformation * selectAccelerator)
        }
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        screen <- NConsole[F].context
        positions = setupPositions(slide1, slide2)
        randomPositions = Random.shuffle(positions.indices.toList)
        _ <- NConsole[F].writeString(slide1) >>
          transformSlides(Screen(screen.screenWidth, screen.screenHeight), positions, randomPositions) >>
          Temporal[F].sleep(200.milli) >>
          NConsole[F].writeString(slide2)
      } yield ()
    }

    override def userInput(input: UserInput): F[Unit] = Temporal[F].unit
  }

}
