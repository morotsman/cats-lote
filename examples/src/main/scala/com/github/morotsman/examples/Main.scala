package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.slides.{Agenda, Animator, Bye, ExampleInteractiveSlide, Start}
import com.github.morotsman.lote.interpreter.StepByStepSlide
import com.github.morotsman.lote.builders.{SessionBuilder, SlideContext, TextSlideBuilder}
import com.github.morotsman.lote.interpreter.transition.{
  FallingCharactersTransition,
  MorphTransition,
  ReplaceTransition
}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

import scala.concurrent.duration.DurationInt

object Session1 extends IOApp.Simple {

  override def run(): IO[Unit] = {
    SessionBuilder[IO]()
      .withTimer(30.minutes)
      .withProgressBar()
      .withQuickNavigation()
      .withIdleAnimation(idleTimeout = 2.minutes)
      .addTextSlide { implicit ctx => import ctx._
        _.content(Start())
          .title("Start")
          .transition(ReplaceTransition(' '))
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      // Explicit types to clarify what `addTextSlide` expects:
      // a function from SlideContext to a TextSlideBuilder transformation.
      .addTextSlide { (ctx: SlideContext[IO]) =>
        import ctx._
        (builder: TextSlideBuilder[IO, TextSlideBuilder.WithoutContent]) =>
          builder
            .content(Agenda())
            .title("Agenda")
            .transition(FallingCharactersTransition(1.4, 1.3))
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide { implicit ctx => import ctx._
        _.content("Supports different alignments")
          .transition(MorphTransition())
          .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
      }
      .addSlideF { implicit ctx => import ctx._
        for {
          slide <- StepByStepSlide.make[IO](Vector(
            "Step 1: Introduction to the topic",
            "Step 1: Introduction to the topic\nStep 2: Dive deeper into details",
            "Step 1: Introduction to the topic\nStep 2: Dive deeper into details\nStep 3: Conclusion"
          ))
        } yield {
          _.addSlide(slide).title("Step by Step")
        }
      }
      .addSlideF { implicit ctx => import ctx._
        for {
          animator <- Animator.make[IO]()
          slide <- ExampleInteractiveSlide.make[IO](animator)
        } yield {
          _.addSlide(slide).title("Interactive")
        }
      }
      .addTextSlide { implicit ctx => import ctx._
        _.content(Bye())
          .title("Bye")
          .transition(FallingCharactersTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addExitSlide("Thanks for watching!")
      .run()
  }

}
