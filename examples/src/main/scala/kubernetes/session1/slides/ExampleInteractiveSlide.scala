package kubernetes.session1.slides

import cats.effect.{Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, Character, HorizontalAlignment, UserInput, VerticalAlignment}
import cats.effect.std.Queue

import scala.concurrent.duration.{DurationInt}


object ExampleInteractiveSlide {

  def make[F[_] : Temporal](animator: Animator[F]): F[Slide[F]] = {

    Temporal[F].pure(new Slide[F] {
      override def content: NConsole[F] => F[NConsole.ScreenAdjusted] =
        console => console.alignText("AWSD is your friend", Alignment(
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
          animator.changeDirection(DirectionDown())
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

object Animator {
  def make[F[_] : Temporal : NConsole](): F[Animator[F]] = {

    def createAnimator(queue: Queue[F, Direction]): Animator[F] = new Animator[F] {
      override def animate(): F[Unit] = {
        for {
          screen <- NConsole[F].context
          emptyScreen = Vector.fill(screen.screenWidth*screen.screenHeight)(' ')
          _ <- loop(emptyScreen, Vector(
            emptyScreen.length / 2,
            emptyScreen.length / 2 + 1,
            emptyScreen.length / 2 + 2,
            emptyScreen.length / 2 + 3,
            emptyScreen.length / 2 + 4,
          ))
        } yield ()
      }

      def loop(emptyScreen: Vector[Char], currentIndexes: Vector[Int]): F[Unit] = {
        for {
          maybeUserInput <- queue.tryTake
          _ <- Temporal[F].sleep(100.milli)
          screen <- NConsole[F].context
          updatedScreen = {
            currentIndexes.foldRight(emptyScreen) { case (index, updatedScreen) =>
              updatedScreen.updated(index, '*')
            }
          }
          _ <- NConsole[F].writeString(ScreenAdjusted(
            updatedScreen.mkString,
            screen.screenWidth,
            screen.screenHeight
          ))
          _ <- loop(emptyScreen, currentIndexes.map(_ - 1))
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