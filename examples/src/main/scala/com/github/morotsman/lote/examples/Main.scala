package com.github.morotsman.lote.examples

import cats.effect._
import com.github.morotsman.lote.PresentationBuilder
import com.github.morotsman.lote.examples.slides._
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.nconsole.NConsoleInstances.IONConsole
import com.github.morotsman.lote.interpreter.transition.{MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = {
    val presentation = PresentationBuilder[IO]()
      .addTextSlide {
        _.content(Start())
          .transition(right = ReplaceTransition(' '))
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(Agenda())
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(DistributedSystem())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Right))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content("kfhdfs ksdfhkjsdfh kdsfkjsdfh dfsjkdjshf")
          .transition(right = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Down, HorizontalAlignment.Right))
      }
      //.addExitSlide(Bye())
      .build()

    for {
      executor <- PresentationExecutorInterpreter.make[IO](presentation)
      _ <- executor.start()
    } yield ()
  }

}
