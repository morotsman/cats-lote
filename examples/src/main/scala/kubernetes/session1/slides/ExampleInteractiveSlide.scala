package kubernetes.session1.slides

import cats.effect.{Sync, Temporal}
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}
import cats.implicits._


object ExampleInteractiveSlide {

  def make[F[_] : Temporal: NConsole](): F[Slide[F]] = {

    val tmp: Slide[F] = new Slide[F] {
      override def content: NConsole[F] => F[NConsole.ScreenAdjusted] =
        console => console.alignText("*", Alignment(
          VerticalAlignment.Center,
          HorizontalAlignment.Center
        ))

      override def startShow: NConsole[F] => F[Unit] =
        console => content(console) >>= (c => console.writeString(c))

      override def stopShow: F[Unit] =
        Temporal[F].unit

      override def userInput(input: UserInput): F[Unit] = for {
        input <- NConsole[F].alignText(input.toString, Alignment(
          VerticalAlignment.Center,
          HorizontalAlignment.Center
        ))
        _ <- {
          NConsole[F].writeString(input)
        }
      } yield ()
        Temporal[F].unit
    }

    Temporal[F].pure(tmp)
  }


}