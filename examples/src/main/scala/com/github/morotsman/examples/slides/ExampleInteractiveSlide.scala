package com.github.morotsman.examples.slides

import cats.effect.Temporal
import cats.effect.std.Queue
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model._

import scala.concurrent.duration.DurationInt
import scala.util.Random


object ExampleInteractiveSlide {

  def make[F[_] : Temporal](console: NConsole[F], animator: Animator[F]): F[Slide[F]] = {

    Temporal[F].pure(new Slide[F] {
      override def content: F[NConsole.ScreenAdjusted] =
        console.alignText("", Alignment(
          VerticalAlignment.Center,
          HorizontalAlignment.Center
        ))

      override def startShow: F[Unit] =
        animator.animate()

      override def stopShow: F[Unit] =
        Temporal[F].unit

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
          Temporal[F].unit
      }
    })
  }
}

trait Animator[F[_]] {
  def animate(): F[Unit]

  def changeDirection(input: Direction): F[Unit]
}

case class WormSegment(index: Int, direction: Direction, symbol: Char)

case class Worm(segments: Vector[WormSegment])

object Animator {

  def make[F[_] : Temporal](console: NConsole[F]): F[Animator[F]] = {

    def createAnimator(queue: Queue[F, Direction]): Animator[F] = new Animator[F] {
      override def animate(): F[Unit] = {
        for {
          screen <- console.context
          screenSize = screen.screenWidth * screen.screenHeight
          emptyScreen = Vector.fill(screenSize)(' ')
          heartIndexes = List.fill(500)(Random.nextInt(screenSize))
          message = "WASD is your friend"
          _ <- loop(emptyScreen, Worm(message.zipWithIndex.map { case (c, index) =>
            WormSegment(screenSize / 2 + index, DirectionLeft(), c)
          }.toVector), heartIndexes.toSet)
        } yield ()
      }

      def loop(emptyScreen: Vector[Char], originalWorm: Worm, heartIndexes: Set[Int]): F[Unit] = {
        val collisions = originalWorm.segments.map(_.index).toSet.intersect(heartIndexes)
        val updatedHeartIndexes = heartIndexes.removedAll(collisions)

        if (originalWorm.segments.map(_.index).size == originalWorm.segments.map(_.index).toSet.size) {
          for {
            _ <- Temporal[F].sleep(100.milli)
            maybeUserInput <- queue.tryTake
            screen <- console.context
            wormWithHearts = Worm(segments = if (collisions.nonEmpty) {
              originalWorm.segments :+ originalWorm.segments.last.copy(
                index = originalWorm.segments.last.direction match {
                  case DirectionLeft() => originalWorm.segments.last.index + 1
                  case DirectionRight() => originalWorm.segments.last.index - 1
                  case DirectionUp() => originalWorm.segments.last.index + screen.screenWidth
                  case DirectionDown() => originalWorm.segments.last.index - screen.screenWidth
                },
                symbol = '?'
              )
            } else {
              originalWorm.segments
            })
            updatedWorm = {
              maybeUserInput.orElse(wormWithHearts.segments.headOption.map(_.direction)).fold(wormWithHearts) { headDirection =>
                wormWithHearts.copy(segments = wormWithHearts.segments.foldLeft((headDirection, Vector[WormSegment]())) {
                  case ((newDirection, acc), oldSegment) =>
                    val newSegment = oldSegment.copy(
                      direction = newDirection,
                      index = oldSegment.direction match {
                        case DirectionLeft() =>
                          oldSegment.index - 1
                        case DirectionRight() =>
                          oldSegment.index + 1
                        case DirectionUp() => if (oldSegment.index - screen.screenWidth > -1) {
                          oldSegment.index - screen.screenWidth
                        } else {
                          emptyScreen.length + (oldSegment.index - screen.screenWidth)
                        }
                        case DirectionDown() => if (oldSegment.index + screen.screenWidth < emptyScreen.length) {
                          oldSegment.index + screen.screenWidth
                        } else {
                          screen.screenWidth + oldSegment.index - emptyScreen.length
                        }
                      }
                    )
                    (oldSegment.direction, newSegment +: acc)
                }._2.reverse)
              }
            }
            screenWithWorm = {
              updatedWorm.segments.foldRight(emptyScreen) { case (WormSegment(index, _, s), updatedScreen) =>
                updatedScreen.updated(index, s)
              }
            }
            updatedScreen = {
              updatedHeartIndexes.foldRight(screenWithWorm) { case (index, updatedScreen) =>
                updatedScreen.updated(index, '?')

              }
            }
            _ <- console.writeString(ScreenAdjusted(
              updatedScreen.mkString,
              screen.screenWidth,
              screen.screenHeight
            ))
            _ <- loop(emptyScreen, updatedWorm, updatedHeartIndexes)
          } yield ()
        } else {
          Temporal[F].unit
        }
      }

      override def changeDirection(input: Direction): F[Unit] =
        queue.offer(input)
    }

    for {
      queue <- Queue.unbounded[F, Direction]
      animator = createAnimator(queue)
    } yield animator
  }
}