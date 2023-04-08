package kubernetes.session1.slides

import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

object Direction extends Enumeration {
  type VerticalAlignment = Value
  val Up, Down, Left, Right, No = Value
}

object ExampleInteractiveSlide {

  def make[F[_] : Temporal : NConsole](animator: Animator[F]): F[Slide[F]] = {

    for {
      direction <- Ref[F].of(Direction.No)
      slide = new Slide[F] {
        override def content: NConsole[F] => F[NConsole.ScreenAdjusted] =
          console => console.alignText("AWSD is your friend", Alignment(
            VerticalAlignment.Center,
            HorizontalAlignment.Center
          ))

        override def startShow: NConsole[F] => F[Unit] =
          _ => animator.animate()

        override def stopShow: F[Unit] =
          Temporal[F].unit

        override def userInput(input: UserInput): F[Unit] = {
          animator.changeDirection(input)
        }
      }
    } yield slide
  }
}

trait Animator[F[_]] {
  def animate(): F[Unit]
  def changeDirection(input: UserInput): F[Unit]
}

object Animator {
  def make[F[_] : Temporal : NConsole](): F[Animator[F]] = {
    val animator = new Animator[F] {
      override def animate(): F[Unit] = {
        for {
          screen <- NConsole[F].context
          emptyScreen = List.fill(screen.screenWidth*screen.screenHeight)(' ')
          _ <- loop(emptyScreen)
        } yield ()
      }

      def loop(emptyScreen: List[Char]): F[Unit] = {
        for {
          _ <- Temporal[F].sleep(40.milli)
          screen <- NConsole[F].context
          _ <- NConsole[F].writeString(ScreenAdjusted(
            // Random.nextInt(100).toChar.toString,
            emptyScreen.mkString,
            screen.screenWidth,
            screen.screenHeight
          ))
          _ <- loop(emptyScreen)
        } yield ()
      }

      override def changeDirection(input: UserInput): F[Unit] = ???
    }
    Temporal[F].pure(animator)
  }
}