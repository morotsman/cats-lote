package com.github.morotsman.lote.interpreter.transition

import cats.Monad
import cats.effect.{Concurrent, Deferred, Ref}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted, UserInput}

import scala.util.Random

case class CharacterPosition(
    character: Char,
    inTransition: Boolean,
    canTransform: Boolean,
    tick: Int = 0
)

case class ScreenPosition(
    index: Int,
    characterPositions: List[CharacterPosition]
)

case class TransitionState(
    positions: List[ScreenPosition],
    randomIndexes: Set[Int],
    nrUnderTransformation: Double
)

object CharactersTransition {

  def apply[F[_]: Concurrent: Ref.Make: NConsole: Ticker](
      selectAccelerator: Double = 1.1,
      setupPosition: (Char, Char) => List[CharacterPosition],
      getNewIndex: (Screen, Int, CharacterPosition) => Option[Int]
  ): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def setupPositions(
          from: ScreenAdjusted,
          to: ScreenAdjusted
      ): List[ScreenPosition] =
        from.content
          .zip(to.content)
          .zipWithIndex
          .map { case ((from, to), index) =>
            if (from == '\n') {
              ScreenPosition(
                index,
                List(
                  CharacterPosition(
                    from,
                    inTransition = false,
                    canTransform = false
                  )
                )
              )
            } else {
              ScreenPosition(index, setupPosition(from, to))
            }
          }
          .toList

      def transformPositions(
          screenPositions: List[ScreenPosition],
          positionsToUpdate: Set[Int]
      ): F[List[ScreenPosition]] = {

        NConsole[F].context.map { screen =>
          val updatedCharacterPositions = screenPositions
            .flatMap(screenPosition =>
              screenPosition.characterPositions.flatMap { characterPosition =>
                if (
                  characterPosition.inTransition || (positionsToUpdate.contains(
                    screenPosition.index
                  ) && characterPosition.canTransform)
                ) {
                  val maybeNewIndex = getNewIndex(
                    screen,
                    screenPosition.index,
                    characterPosition
                  );
                  maybeNewIndex
                    .filter(newIndex =>
                      newIndex < screenPositions.length && newIndex >= 0 && !screenPositions(
                        newIndex
                      ).characterPositions.exists(_.character == '\n')
                    )
                    .map { newIndex =>
                      (
                        newIndex,
                        characterPosition.copy(
                          tick = characterPosition.tick + 1,
                          inTransition = true
                        )
                      )
                    }
                } else {
                  Option((screenPosition.index, characterPosition))
                }
              }
            )
            .groupBy(_._1)

          screenPositions.map { screenPosition =>
            screenPosition.copy(
              characterPositions = updatedCharacterPositions(
                screenPosition.index
              ).map(_._2).sortBy(!_.canTransform)
            )
          }
        }
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        positions = setupPositions(slide1, slide2)
        randomIndexes = Random.shuffle(positions.indices.toList).toSet
        stateRef <- Ref[F].of(TransitionState(positions, randomIndexes, 1.0))
        done <- Deferred[F, Unit]
        onTick = for {
          s <- stateRef.get
          _ <-
            if (
              s.positions.forall(
                _.characterPositions.forall(_.canTransform == false)
              )
            ) {
              NConsole[F].clear() *> done.complete(()).void
            } else {
              for {
                _ <- NConsole[F].clear()
                positionsToUpdate = s.randomIndexes.take(
                  s.nrUnderTransformation.toInt
                )
                updatedPositions <- transformPositions(
                  s.positions,
                  positionsToUpdate
                )
                _ <- NConsole[F].writeString(
                  ScreenAdjusted(
                    updatedPositions
                      .map(
                        _.characterPositions.headOption
                          .map(_.character)
                          .getOrElse("")
                      )
                      .mkString("")
                  )
                )
                newRandomIndexes = s.randomIndexes.drop(
                  s.nrUnderTransformation.toInt
                )
                _ <- stateRef.set(
                  TransitionState(
                    updatedPositions,
                    newRandomIndexes,
                    s.nrUnderTransformation * selectAccelerator
                  )
                )
              } yield ()
            }
        } yield ()
        _ <- NConsole[F].writeString(slide1)
        sub <- Ticker[F].subscribe(onTick)
        _ <- Ticker[F].start
        _ <- done.get.guarantee(sub.cancel)
        _ <- NConsole[F].writeString(slide2)
      } yield ()
    }

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }

}
