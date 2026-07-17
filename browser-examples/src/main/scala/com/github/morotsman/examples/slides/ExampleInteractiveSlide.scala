package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.implicits._
import com.github.morotsman.lote.api.{
  AnimationSettings,
  Character,
  ScreenAdjusted,
  UserInput
}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{Clock, FixedStep}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, TickerSubscription}

import scala.util.Random

object ExampleInteractiveSlide {

  def contextual[F[_]: Temporal: Ref.Make]()(implicit monad: Monad[F]): ContextualF[F, Slide[F]] =
    ContextualF { ctx =>
      def buildSlide(animator: Animator[F]): Slide[F] =
        fromAnimator[F](animator, ctx.console)(monad)

      Animator.create[F](ctx.console, ctx.ticker, ctx.animationSettings).map(buildSlide)
    }

  def fromAnimator[F[_]](
      animator: Animator[F],
      console: NConsole[F]
  )(implicit monad: Monad[F]): Slide[F] = {

    new Slide[F] {
      override def content: F[Option[ScreenAdjusted]] =
        monad.pure(None)

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
          monad.unit
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

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: Clock[F]): F[Animator[F]] = {

    for {
      queue <- Queue.unbounded[F, Direction]
      stateRef <- Ref[F].of(Option.empty[AnimatorState])
      subscriptionRef <- Ref[F].of(Option.empty[TickerSubscription[F]])
      stepperRef <- FixedStep.makeRef[F]
    } yield new Animator[F] {

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

      private val youDiedArt: String =
        """
          |
          |
          |▓██   ██▓ ▒█████   █    ██    ▓█████▄  ██▓▓█████ ▓█████▄
          | ▒██  ██▒▒██▒  ██▒ ██  ▓██▒   ▒██▀ ██▌▓██▒▓█   ▀ ▒██▀ ██▌
          |  ▒██ ██░▒██░  ██▒▓██  ▒██░   ░██   █▌▒██▒▒███   ░██   █▌
          |  ░ ▐██▓░▒██   ██░▓▓█  ░██░   ░▓█▄   ▌░██░▒▓█  ▄ ░▓█▄   ▌
          |  ░ ██▒▓░░ ████▓▒░▒▒█████▓    ░▒████▓ ░██░░▒████▒ ░▒████▓
          |   ██▒▒▒ ░ ▒░▒░▒░ ░▒▓▒ ▒ ▒    ▒▒▓  ▒ ░▓  ░░ ▒░ ░  ▒▒▓  ▒
          | ▓██ ░▒░   ░ ▒ ▒░ ░░▒░ ░ ░    ░ ▒  ▒  ▒ ░ ░ ░  ░  ░ ▒  ▒
          | ▒ ▒ ░░  ░ ░ ░ ▒   ░░░ ░ ░    ░ ░  ░  ▒ ░   ░     ░ ░  ░
          | ░ ░       ░ ░       ░            ░  ░       ░  ░      ░
          | ░ ░                            ░                    ░
          |
          |""".stripMargin

      private def renderDeathScreen(
          screenWidth: Int,
          screenHeight: Int
      ): ScreenAdjusted = {
        val artLines = youDiedArt.split("\n", -1).toVector
        val artHeight = artLines.length
        val artWidth = artLines.map(_.length).maxOption.getOrElse(0)

        val topPad = math.max(0, (screenHeight - artHeight) / 2)
        val leftPad = math.max(0, (screenWidth - artWidth) / 2)

        def fitLine(line: String): String = {
          val padded = (" " * leftPad) + line
          val truncated = padded.take(screenWidth)
          truncated + (" " * math.max(0, screenWidth - truncated.length))
        }

        val emptyLine = " " * screenWidth
        val lines = Vector.fill(topPad)(emptyLine) ++
          artLines.map(fitLine) ++
          Vector.fill(math.max(0, screenHeight - topPad - artHeight))(emptyLine)

        ScreenAdjusted(lines.take(screenHeight).mkString("\n"))
      }

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

      private def onTick(nrOfSteps: Int): F[Unit] =
        if (nrOfSteps <= 0) Monad[F].unit
        else
          for {
            maybeState <- stateRef.get
            _ <- maybeState.traverse_ { s =>
              if (!s.running) Monad[F].unit
              else if (hasSelfCollision(s.worm)) {
                for {
                  screen <- console.context
                  _ <- console.writeString(renderDeathScreen(screen.screenWidth, screen.screenHeight))
                  _ <- stateRef.update(_.map(_.copy(running = false)))
                } yield ()
              } else {
                for {
                  screen <- console.context
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
                  _ <-
                    if (updatedState.running)
                      console.writeString(
                        renderScreen(updatedState, screen.screenWidth)
                      )
                    else Monad[F].unit
                  _ <- stateRef.set(Some(updatedState))
                } yield ()
              }
            }
          } yield ()

      private val tickerCallback: F[Unit] =
        FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap(onTick)

      override def animate(): F[Unit] = for {
        screen <- console.context
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
        _ <- FixedStep.reset(stepperRef)
        sub <- ticker.subscribe(tickerCallback)
        _ <- subscriptionRef.set(Some(sub))
        _ <- ticker.start
      } yield ()

      override def stop(): F[Unit] = for {
        maybeSub <- subscriptionRef.get
        _ <- maybeSub.traverse_(_.cancel)
        _ <- subscriptionRef.set(None)
        _ <- stateRef.set(None)
      } yield ()

      override def changeDirection(input: Direction): F[Unit] =
        queue.offer(input)
    }
  }
}
