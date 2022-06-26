package com.github.morotsman
package lote

import lote.PresentationBuilder._
import lote.interpreter.SimpleSlide.ToSimpleSlide
import cats.effect.{Sync, Temporal}
import lote.algebra.{NConsole, Slide, Transition}
import lote.model.{Presentation, SlideSpecification}

case class PresentationBuilder[F[_] : Temporal : NConsole : Sync, State <: BuildState](
                                                                                        slideSpecifications: List[SlideSpecification[F]],
                                                                                        exitSlide: Option[Slide[F]]
                                                                                      ) {
  def addSlide(slide: Slide[F], slideSpecificationBuilder: SlideSpecificationBuilder[F] => SlideSpecificationBuilder[F])
  : PresentationBuilder[F, State with SlideAdded] = {
    val builder = SlideSpecificationBuilder(slide = slide, left = None, right = None)
    val slideSpecification = slideSpecificationBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  def addSlide(s: String, slideSpecificationBuilder: SlideSpecificationBuilder[F] => SlideSpecificationBuilder[F])
  : PresentationBuilder[F, State with SlideAdded] =
    addSlide(s.toSlide, slideSpecificationBuilder)

  def addSlide(s: String): PresentationBuilder[F, State with SlideAdded] =
    addSlide(s.toSlide, identity)

  def addSlide(slide: Slide[F]): PresentationBuilder[F, State with SlideAdded] =
    addSlide(slide, identity)

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

