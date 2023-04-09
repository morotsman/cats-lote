package kubernetes.session1.slides

import cats.effect.Temporal
import cats.effect.std.Queue
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model._

import scala.concurrent.duration.DurationInt


object ExampleInteractiveSlide {

  def make[F[_] : Temporal](animator: Animator[F]): F[Slide[F]] = {

    Temporal[F].pure(new Slide[F] {
      override def content: NConsole[F] => F[NConsole.ScreenAdjusted] =
        console => console.alignText("WASD is your friend", Alignment(
          VerticalAlignment.Center,
          HorizontalAlignment.Center
        ))

      override def startShow: NConsole[F] => F[Unit] =
        _ => animator.animate()

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

  def make[F[_] : Temporal : NConsole](): F[Animator[F]] = {

    def createAnimator(queue: Queue[F, Direction]): Animator[F] = new Animator[F] {
      override def animate(): F[Unit] = {
        for {
          screen <- NConsole[F].context
          emptyScreen = Vector.fill(screen.screenWidth * screen.screenHeight)(' ')
          _ <- loop(emptyScreen, Worm(Vector(
            WormSegment(emptyScreen.length / 2, DirectionLeft(), '1'),
            WormSegment(emptyScreen.length / 2 + 1, DirectionLeft(), '2'),
            WormSegment(emptyScreen.length / 2 + 2, DirectionLeft(), '3'),
            WormSegment(emptyScreen.length / 2 + 3, DirectionLeft(), '4'),
            WormSegment(emptyScreen.length / 2 + 4, DirectionLeft(), '5')
          )))
        } yield ()
      }

      def loop(emptyScreen: Vector[Char], worm: Worm): F[Unit] = {
        for {
          _ <- Temporal[F].sleep(100.milli)
          maybeUserInput <- queue.tryTake
          screen <- NConsole[F].context
          updatedWorm = {
            maybeUserInput.orElse(worm.segments.headOption.map(_.direction)).fold(worm) { headDirection =>
              worm.copy(segments = worm.segments.foldLeft((headDirection, Vector[WormSegment]())) {
                case ((newDirection, acc), oldSegment) =>
                  val newSegment = oldSegment.copy(
                    direction = newDirection,
                    index = oldSegment.direction match {
                      case DirectionLeft() => oldSegment.index - 1
                      case DirectionRight() => oldSegment.index + 1
                      case DirectionUp() => oldSegment.index - screen.screenWidth
                      case DirectionDown() => oldSegment.index + screen.screenWidth
                    }
                  )
                  (oldSegment.direction, newSegment +: acc)
              }._2.reverse)
            }
          }
          updatedScreen = {
            updatedWorm.segments.foldRight(emptyScreen) { case (WormSegment(index, _, s), updatedScreen) =>
              updatedScreen.updated(index, s)
            }
          }
          _ <- NConsole[F].writeString(ScreenAdjusted(
            updatedScreen.mkString,
            screen.screenWidth,
            screen.screenHeight
          ))
          _ <- loop(emptyScreen, updatedWorm)
        } yield ()
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