package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Screen, ScreenAdjusted}
import com.github.morotsman.lote.api.support.{AnimationClock, GlideLayer, SmoothChar, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

import scala.util.Random

private[lote] case class CharacterPosition(
    character: Char,
    inTransition: Boolean,
    canTransform: Boolean,
    tick: Double = 0.0,
    originalIndex: Int = -1
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
  )(implicit clock: AnimationClock[F]): Transition[F] = {

    def isFinished(state: TransitionState): Boolean =
      state.positions.forall(_.characterPositions.forall(_.canTransform == false))

    def renderBackground(state: TransitionState): ScreenAdjusted =
      ScreenAdjusted(
        state.positions
          .map { sp =>
            sp.characterPositions
              .find(!_.inTransition)
              .map(_.character)
              .getOrElse(' ')
          }
          .mkString("")
      )

    def inTransitionSmoothChars(state: TransitionState, screenWidth: Int): Vector[SmoothChar] = {
      val stride = screenWidth + 1
      state.positions
        .flatMap { sp =>
          sp.characterPositions.collect {
            case cp if cp.inTransition =>
              (
                cp.originalIndex,
                SmoothChar(cp.character, sp.index % stride, sp.index / stride, key = cp.originalIndex)
              )
          }
        }
        .sortBy(_._1)
        .map(_._2)
        .toVector
    }

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
                  canTransform = false,
                  originalIndex = index
                )
              )
            )
          } else {
            ScreenPosition(index, setupPosition(from, to).map(_.copy(originalIndex = index)))
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

    TickedTransition(console, ticker, animationSettings)
      .buildWithSetup { (slide1, slide2, complete) =>
        val positions = setupPositions(slide1, slide2)
        val randomIndexes = Random.shuffle(positions.indices.toList).toSet
        val initialState = TransitionState(positions, randomIndexes, 1.0)

        for {
          gridLayer <- GlideLayer.make[F](console, animationSettings.step, wrapThreshold = 1000)
          stateRef  <- Ref[F].of(initialState)
          _         <- console.writeString(slide1)
          _         <- if (isFinished(initialState)) complete else Monad[F].unit
        } yield TickedTransition.TickHandler(
          onTick = (nrOfSteps: Int, _progress: Double) => {
            if (nrOfSteps <= 0) {
              stateRef.get.flatMap { currentState =>
                if (!isFinished(currentState)) {
                  console.context.flatMap { screen =>
                    val bg = renderBackground(currentState)
                    val smoothChars = inTransitionSmoothChars(currentState, screen.screenWidth)
                    gridLayer.renderOnto(bg, smoothChars).flatMap { _ => Monad[F].unit }
                  }
                } else Monad[F].unit
              }
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
                    console.clear() *> gridLayer.clear() *> complete
                  } else {
                    val bg = renderBackground(updatedState)
                    val smoothChars = inTransitionSmoothChars(updatedState, screen.screenWidth)
                    gridLayer.renderOnto(bg, smoothChars).flatMap { composited =>
                      console.clear() *> console.writeString(composited)
                    }
                  }
              } yield ()
            }
          },
          cleanup = gridLayer.clear()
        )
      }
  }

}

