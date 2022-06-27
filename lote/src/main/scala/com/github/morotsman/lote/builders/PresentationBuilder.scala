package com.github.morotsman.lote.builders

import cats.effect.{Sync, Temporal}
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.builders.PresentationBuilder.{BuildState, Buildable, SlideAdded}
import com.github.morotsman.lote.builders.SlideBuilder.{WithContentSlide, WithoutSlide}
import com.github.morotsman.lote.builders.TextSlideBuilder.{WithContent, WithoutContent}
import com.github.morotsman.lote.interpreter.TextSlide.ToTextSlide
import com.github.morotsman.lote.model.{Presentation, SlideSpecification}

case class PresentationBuilder[F[_] : Temporal : NConsole : Sync, State <: BuildState](
                                                                                        slideSpecifications: List[SlideSpecification[F]],
                                                                                        exitSlide: Option[Slide[F]]
                                                                                      ) {
  def addSlide(
                slideBuilder: SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]
              ): PresentationBuilder[F, State with SlideAdded] = {
    val builder: SlideBuilder[F, WithoutSlide] = SlideBuilder()
    val slideSpecification = slideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  def addTextSlide(
                    textSlideBuilder: TextSlideBuilder[F, WithoutContent] => TextSlideBuilder[F, WithContent]
                  ): PresentationBuilder[F, State with SlideAdded] = {
    val builder = TextSlideBuilder()
    val slideSpecification = textSlideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  def addExitSlide(slide: Slide[F]): PresentationBuilder[F, State] =
    this.copy(exitSlide = Option(slide))

  def addExitSlide(s: String): PresentationBuilder[F, State] =
    this.copy(exitSlide = Option(s.toSlide))

  def build()(implicit ev: State =:= Buildable): Presentation[F] =
    Presentation(
      slideSpecifications = slideSpecifications.reverse,
      exitSlide = exitSlide
    )
}

object PresentationBuilder {
  sealed trait BuildState

  sealed trait Empty extends BuildState

  sealed trait SlideAdded extends BuildState

  type Buildable = Empty with SlideAdded

  def apply[F[_] : Temporal : NConsole : Sync](): PresentationBuilder[F, Empty] =
    PresentationBuilder[F, Empty](List.empty, None)
}

