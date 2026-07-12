package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.api.support.FixedStep
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, TickerSubscription, Transition}

import scala.util.Random

private[lote] case class CharacterPosition(
    character: Char,
    inTransition: Boolean,
    canTransform: Boolean,
    tick: Double = 0.0
)

private[lote] case class ScreenPosition(
    index: Int,
    characterPositions: List[CharacterPosition]
)

private[lote] case class TransitionState(
    positions: List[ScreenPosition],
    randomIndexes: Set[Int],
    nrUnderTransformation: Double
)

private[lote] object CharactersTransition {

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      selectAccelerator: Double = 1.1,
      setupPosition: (Char, Char) => List[CharacterPosition],
      getNewIndex: (Screen, Int, CharacterPosition) => Option[Int]
  )(implicit animationSettings: AnimationSettings): Transition[F] =
    create(
      selectAccelerator = selectAccelerator,
      setupPosition = setupPosition,
      getNewIndex = getNewIndex,
      console = NConsole[F],
      ticker = Ticker[F],
      animationSettings = animationSettings
    )

  def create[F[_]: Temporal: Ref.Make](
      selectAccelerator: Double = 1.1,
      setupPosition: (Char, Char) => List[CharacterPosition],
      getNewIndex: (Screen, Int, CharacterPosition) => Option[Int],
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  ): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def isFinished(state: TransitionState): Boolean =
        state.positions.forall(_.characterPositions.forall(_.canTransform == false))

      def render(state: TransitionState): ScreenAdjusted =
        ScreenAdjusted(
          state.positions
            .map(
              _.characterPositions.headOption
                .map(_.character)
                .getOrElse("")
            )
            .mkString("")
        )

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
          screen: Screen,
          screenPositions: List[ScreenPosition],
          positionsToUpdate: Set[Int]
      ): List[ScreenPosition] = {
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
                )
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
                        tick = characterPosition.tick + 1.0,
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

      def advanceState(
          screen: Screen,
          state: TransitionState
      ): TransitionState = {
        val positionsToUpdate = state.randomIndexes.take(
          state.nrUnderTransformation.toInt
        )
        val updatedPositions = transformPositions(
          screen,
          state.positions,
          positionsToUpdate
        )
        val newRandomIndexes = state.randomIndexes.drop(
          state.nrUnderTransformation.toInt
        )

        TransitionState(
          updatedPositions,
          newRandomIndexes,
          state.nrUnderTransformation * selectAccelerator
        )
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        positions = setupPositions(slide1, slide2)
        randomIndexes = Random.shuffle(positions.indices.toList).toSet
        initialState = TransitionState(positions, randomIndexes, 1.0)
        stateRef <- Ref[F].of(initialState)
        stepperRef <- FixedStep.makeRef[F]
        done <- Deferred[F, Unit]
        _ <- console.writeString(slide1)
        _ <- if (isFinished(initialState)) done.complete(()).void else Monad[F].unit
        onTick = { (nrOfSteps: Int) =>
          if (nrOfSteps <= 0) {
            Monad[F].unit
          } else {
            for {
              screen <- console.context
              currentState <- stateRef.get
              updatedState =
                (0 until nrOfSteps).foldLeft(currentState) { case (state, _) =>
                  if (isFinished(state)) state else advanceState(screen, state)
                }
              _ <- stateRef.set(updatedState)
              _ <-
                if (isFinished(updatedState)) {
                  console.clear() *> done.complete(()).attempt.void
                } else {
                  console.clear() *> console.writeString(
                    render(updatedState)
                  )
                }
            } yield ()
          }
        }
        tickerCallback = FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap(onTick)
        maybeSub <-
          if (isFinished(initialState)) Monad[F].pure(Option.empty[TickerSubscription[F]])
          else ticker.subscribe(tickerCallback).map(Option(_))
        _ <- maybeSub.traverse_(_ => ticker.start)
        _ <- done.get.guarantee(maybeSub.traverse_(_.cancel))
        _ <- console.writeString(slide2)
      } yield ()
    }

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }

}

