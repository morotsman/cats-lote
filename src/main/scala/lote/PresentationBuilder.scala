package com.github.morotsman
package lote

import lote.PresentationBuilder._
import lote.interpreter.SimpleSlide.ToSimpleSlide
import cats.effect.{Sync, Temporal}
import lote.algebra.{NConsole, Slide, Transition}
import lote.model.{Presentation, SlideSpecification}

case class PresentationBuilder[F[_] : Temporal : NConsole: Sync, State <: BuildState](
                                                                                       ongoing: Option[SlideSpecification[F]],
                                                                                       sat: List[SlideSpecification[F]]
                                                                               ) {
  def addSlide(slide: Slide[F]): PresentationBuilder[F, State with SlideAdded] =
    PresentationBuilder(
      Option(model.SlideSpecification(None, slide, None)),
      ongoing.fold(sat)(_ :: sat)
    )

  def addSlide(s: String): PresentationBuilder[F, State with SlideAdded] =
    addSlide(s.toSlide)

  def build()(implicit ev: State =:= Buildable): Presentation[F] =
    Presentation(
      slideSpecifications = ongoing.fold(sat)(_ :: sat).reverse
    )

  def addTransitions(
                      left: Transition[F] = null,
                      right: Transition[F] = null
                    )(implicit ev: State <:< SlideAdded): PresentationBuilder[F, State] = {
    PresentationBuilder(
      None,
      ongoing.fold(sat)(_.copy(left = Option(left), right = Option(right)) :: sat)
    )
  }

}

object PresentationBuilder {
  sealed trait BuildState

  sealed trait Empty extends BuildState
  sealed trait SlideAdded extends BuildState

  type Buildable = Empty with SlideAdded

  def apply[F[_] : Temporal : NConsole: Sync](): PresentationBuilder[F, Empty] =
    PresentationBuilder[F, Empty](None, List.empty)
}

