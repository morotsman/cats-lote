package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted, UserInput}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random


case class CharacterPosition(character: Char, inTransition: Boolean, canTransform: Boolean, tick: Int = 0)

case class ScreenPosition(index: Int, characterPositions: List[CharacterPosition])

object CharactersTransition {

  def apply[F[_] : Temporal : NConsole](
                                         selectAccelerator: Double = 1.1,
                                         timeBetweenTicks: FiniteDuration = 40.milli,
                                         setupPosition: (Char, Char) => List[CharacterPosition],
                                         getNewIndex: (Screen, Int, CharacterPosition) => Option[Int]
                                       ): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def setupPositions(from: ScreenAdjusted, to: ScreenAdjusted): List[ScreenPosition] =
        from.content.zip(to.content).zipWithIndex.map { case ((from, to), index) => if (from == '\n') {
          ScreenPosition(index, List(
            CharacterPosition(from, inTransition = false, canTransform = false)
          ))
        } else {
          ScreenPosition(index, setupPosition(from, to))
        }
        }.toList

      def transformPositions(
                              screenPositions: List[ScreenPosition],
                              positionsToUpdate: Set[Int]
                            ): F[List[ScreenPosition]] = {


        // mark positions to transform
        val positions : List[ScreenPosition] = screenPositions.map { screenPosition =>
          if (positionsToUpdate.contains(screenPosition.index)) {
            screenPosition.copy(characterPositions = screenPosition.characterPositions.map(cp => if (cp.canTransform) {
              cp.copy(inTransition = true)
            } else {
              cp
            }))
          } else {
            screenPosition
          }
        }

        val toUpdate = positions.toArray
        NConsole[F].context.map { screen =>
          // transform positions
          screenPositions
            .filter { screenPosition =>
              screenPosition.characterPositions.exists(_.inTransition)
            }
            .foreach { case ScreenPosition(index, characterPositions) =>
              // take them away from the old position
              toUpdate(index) = toUpdate(index).copy(
                characterPositions = toUpdate(index).characterPositions.filter(!_.inTransition)
              )

              // move to new position
              val toMove = characterPositions.filter(_.inTransition)
              toMove.foreach { cp =>
                val newIndex = getNewIndex(screen, index, cp);
                newIndex.foreach { index =>
                  val updatedPosition = cp.copy(tick = cp.tick + 1)
                  if (
                    index < toUpdate.length &&
                      index >= 0 &&
                      !toUpdate(index).characterPositions.exists(_.character == '\n')) {
                    toUpdate(index) = toUpdate(index).copy(characterPositions = updatedPosition :: toUpdate(index).characterPositions)
                  }
                }

              }
            }
          toUpdate.toList
        }
      }

      def transformSlides(
                           positions: List[ScreenPosition],
                           randomIndexes: Set[Int],
                           nrUnderTransformation: Double = 1.0
                         ): F[Unit] = {
        if (positions.forall(_.characterPositions.forall(_.canTransform == false))) {
          NConsole[F].clear()
        } else {
          for {
            _ <- NConsole[F].clear()
            positionsToUpdate = randomIndexes.take(nrUnderTransformation.toInt)
            updatedPositions <- transformPositions(positions, positionsToUpdate)
            _ <- NConsole[F].writeString(
              ScreenAdjusted(
                updatedPositions.map(_.characterPositions.headOption.map(_.character).getOrElse("")).mkString("")
              )
            )
            _ <- Temporal[F].sleep(timeBetweenTicks)
            newRandomIndexes = randomIndexes.drop(nrUnderTransformation.toInt)
            _ <- transformSlides(updatedPositions, newRandomIndexes, nrUnderTransformation * selectAccelerator)
          } yield ()
        }
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        positions = setupPositions(slide1, slide2)
        randomIndexes = Random.shuffle(positions.indices.toList)
        _ <- NConsole[F].writeString(slide1) >>
          transformSlides(positions, randomIndexes.toSet) >>
          Temporal[F].sleep(200.milli) >>
          NConsole[F].writeString(slide2)
      } yield ()
    }

    override def userInput(input: UserInput): F[Unit] = Temporal[F].unit
  }

}
