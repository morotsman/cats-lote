package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.implicits._
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Slide, Ticker, TickerSubscription}
import com.github.morotsman.lote.interpreter.animation.FixedStep
import com.github.morotsman.lote.model._

import scala.util.Random

object ExampleInteractiveSlide {

  /** Example custom `Slide[F]` backed by a separate animator component.
    *
    * The slide itself stays small because it mostly maps the `Slide[F]` lifecycle onto the animator:
    *   - `content` provides the initial blank screen
    *   - `startShow` starts the animation loop
    *   - `stopShow` cleans up ticker subscriptions and state
    *   - `userInput` translates keys into domain-specific commands
    */

  def make[F[_]: Monad: NConsole](
      animator: Animator[F]
  ): Slide[F] = {

    new Slide[F] {
      override def content: F[ScreenAdjusted] =
        NConsole[F].alignText(
          "",
          Alignment(
            VerticalAlignment.Center,
            HorizontalAlignment.Center
          )
        )

      override def startShow: F[Unit] =
        animator.animate()

      override def stopShow: F[Unit] =
        animator.stop()

      override def userInput(input: UserInput): F[Unit] = input match {
        case Character(c) if c == 'a' =>
          animator.changeDirection(DirectionLeft())
        case Character(c) if c == 'w' =>
          animator.changeDirection(DirectionUp())
        case Character(c) if c == 's' =>
          animator.changeDirection(DirectionDown())
        case Character(c) if c == 'd' =>
          animator.changeDirection(DirectionRight())
        case _ =>
          Monad[F].unit
      }
    }
  }
}

trait Animator[F[_]] {
  def animate(): F[Unit]

  def stop(): F[Unit]

  def changeDirection(input: Direction): F[Unit]
}

case class WormSegment(index: Int, direction: Direction, symbol: Char)

case class Worm(segments: Vector[WormSegment])

case class AnimatorState(
    emptyScreen: Vector[Char],
    worm: Worm,
    heartIndexes: Set[Int],
    screenWidth: Int,
    running: Boolean
)

object Animator {

  def make[F[_]: Temporal: Ref.Make: NConsole: Ticker]()(implicit animationSettings: AnimationSettings): F[Animator[F]] = {

    for {
      queue <- Queue.unbounded[F, Direction]
      stateRef <- Ref[F].of(Option.empty[AnimatorState])
      subscriptionRef <- Ref[F].of(Option.empty[TickerSubscription[F]])
      stepperRef <- FixedStep.makeRef[F]
    } yield new Animator[F] {

      // The animator owns the mutable game state, the queued input, and the ticker subscription.
      // This is a useful pattern when you want a Slide[F] to stay small and push ongoing behavior into a
      // dedicated runtime component.

      private def growWorm(
          worm: Worm,
          collisions: Set[Int],
          screenWidth: Int
      ): Worm =
        if (collisions.nonEmpty) {
          val last = worm.segments.last
          Worm(segments =
            worm.segments :+ last.copy(
              index = last.direction match {
                case DirectionLeft()  => last.index + 1
                case DirectionRight() => last.index - 1
                case DirectionUp()    => last.index + screenWidth
                case DirectionDown()  => last.index - screenWidth
                case NoDirection()    => last.index
              },
              symbol = '?'
            )
          )
        } else {
          worm
        }

      private def moveWorm(
          worm: Worm,
          newHeadDirection: Direction,
          screenWidth: Int,
          screenLength: Int
      ): Worm =
        worm.copy(segments =
          worm.segments
            .foldLeft((newHeadDirection, Vector[WormSegment]())) { case ((newDirection, acc), oldSegment) =>
              val newSegment = oldSegment.copy(
                direction = newDirection,
                index = oldSegment.direction match {
                  case DirectionLeft() =>
                    if (oldSegment.index % screenWidth == 0) {
                      oldSegment.index + screenWidth - 1
                    } else {
                      oldSegment.index - 1
                    }
                  case DirectionRight() =>
                    if (oldSegment.index % screenWidth == screenWidth - 1) {
                      oldSegment.index - screenWidth + 1
                    } else {
                      oldSegment.index + 1
                    }
                  case DirectionUp() =>
                    if (oldSegment.index - screenWidth > -1) {
                      oldSegment.index - screenWidth
                    } else {
                      screenLength + (oldSegment.index - screenWidth)
                    }
                  case DirectionDown() =>
                    if (oldSegment.index + screenWidth < screenLength) {
                      oldSegment.index + screenWidth
                    } else {
                      screenWidth + oldSegment.index - screenLength
                    }
                  case NoDirection() =>
                    oldSegment.index
                }
              )
              (oldSegment.direction, newSegment +: acc)
            }
            ._2
            .reverse
        )

      private def updateWormState(
          s: AnimatorState,
          maybeUserInput: Option[Direction],
          screenWidth: Int
      ): AnimatorState = {
        // Each update does three things in order:
        //   1. detect collected hearts
        //   2. grow the worm if a heart was collected
        //   3. apply the newest queued direction and move the worm forward
        val collisions =
          s.worm.segments.map(_.index).toSet.intersect(s.heartIndexes)
        val updatedHeartIndexes = s.heartIndexes.removedAll(collisions)
        val wormWithHearts = growWorm(s.worm, collisions, screenWidth)
        val headDirection = maybeUserInput.orElse(
          wormWithHearts.segments.headOption.map(_.direction)
        )
        val updatedWorm = headDirection.fold(wormWithHearts) { dir =>
          moveWorm(wormWithHearts, dir, screenWidth, s.emptyScreen.length)
        }
        s.copy(worm = updatedWorm, heartIndexes = updatedHeartIndexes)
      }

      private def hasSelfCollision(worm: Worm): Boolean =
        worm.segments.map(_.index).size != worm.segments.map(_.index).toSet.size

      private def renderScreen(
          s: AnimatorState,
          screenWidth: Int
      ): ScreenAdjusted = {
        val screenWithWorm = s.worm.segments.foldRight(s.emptyScreen) { case (WormSegment(index, _, sym), screen) =>
          screen.updated(index, sym)
        }
        val finalScreen = s.heartIndexes.foldRight(screenWithWorm) { case (index, screen) =>
          screen.updated(index, '?')
        }
        ScreenAdjusted(
          finalScreen.grouped(screenWidth).map(_.mkString).mkString("\n")
        )
      }

      // User input is consumed here (via tryTake) rather than applied immediately in changeDirection.
      // This ensures direction changes are synchronized with the game loop, preventing race conditions
      // with state and limiting direction changes to one per frame (avoiding 180° turns that would
      // cause instant self-collision). The trade-off is up to ~40ms input latency, which is imperceptible.
      private def onTick(nrOfSteps: Int): F[Unit] =
        if (nrOfSteps <= 0) Monad[F].unit
        else
          for {
            maybeState <- stateRef.get
            _ <- maybeState.traverse_ { s =>
              if (!s.running) Monad[F].unit
              else if (hasSelfCollision(s.worm)) {
                stateRef.update(_.map(_.copy(running = false)))
              } else {
                for {
                  screen <- NConsole[F].context
                  updatedState <- (0 until nrOfSteps).toList.foldLeftM(s) {
                    case (currentState, _) if !currentState.running =>
                      Monad[F].pure(currentState)
                    case (currentState, _) if hasSelfCollision(currentState.worm) =>
                      Monad[F].pure(currentState.copy(running = false))
                    case (currentState, _) =>
                      queue.tryTake.map { maybeUserInput =>
                        updateWormState(
                          currentState,
                          maybeUserInput,
                          screen.screenWidth
                        )
                      }
                  }
                  _ <- if (updatedState.running)
                    NConsole[F].writeString(
                      renderScreen(updatedState, screen.screenWidth)
                    )
                  else Monad[F].unit
                  _ <- stateRef.set(Some(updatedState))
                } yield ()
              }
            }
          } yield ()

      // The ticker callback is the bridge between the shared session clock and the game loop.
      // `FixedStep.consumeSteps(...)` turns wall-clock time into zero or more simulation steps,
      // and `onTick` applies those steps to the current game state.
      private val tickerCallback: F[Unit] =
        FixedStep.consumeSteps(stepperRef).flatMap(onTick)

      override def animate(): F[Unit] = for {
        screen <- NConsole[F].context
        screenSize = screen.screenWidth * (screen.screenHeight - 1)
        emptyScreen = Vector.fill(screenSize)(' ')
        heartIndexes = List.fill(500)(Random.nextInt(screenSize)).toSet
        message = "WASD is your friend"
        initialWorm = Worm(message.zipWithIndex.map { case (c, index) =>
          WormSegment(screenSize / 2 + index, DirectionLeft(), c)
        }.toVector)
        _ <- stateRef.set(
          Some(
            AnimatorState(
              emptyScreen,
              initialWorm,
              heartIndexes,
              screen.screenWidth,
              running = true
            )
          )
        )
        // Reset the fixed-step accumulator so the game starts with a clean timing baseline each time the
        // slide becomes active.
        _ <- FixedStep.reset(stepperRef)
        // Subscribing here means the game starts advancing only while this slide is active.
        sub <- Ticker[F].subscribe(tickerCallback)
        _ <- subscriptionRef.set(Some(sub))
        _ <- Ticker[F].start
      } yield ()

      override def stop(): F[Unit] = for {
        maybeSub <- subscriptionRef.get
        // Leaving the slide cancels future updates and drops the in-memory state so the next visit
        // starts from a clean slate.
        _ <- maybeSub.traverse_(_.cancel)
        _ <- subscriptionRef.set(None)
        _ <- stateRef.set(None)
      } yield ()

      override def changeDirection(input: Direction): F[Unit] =
        queue.offer(input)
    }
  }
}
