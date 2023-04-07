package com.github.morotsman.lote.builders

import com.github.morotsman.lote.algebra.{Slide, Transition}
import com.github.morotsman.lote.builders
import com.github.morotsman.lote.builders.SlideBuilder.{BuildState, SlideAdded}
import com.github.morotsman.lote.model.SlideSpecification

final case class SlideBuilder[F[_], State <: BuildState](
                                            slide: Slide[F],
                                            right: Option[Transition[F]]
                                          ) {

  def transition(
                     right: Transition[F] = null
                   ): SlideBuilder[F, State] =
    this.copy(right = Option(right))

  def addSlide(slide: Slide[F]): SlideBuilder[F, State with SlideAdded] =
    this.copy(slide = slide)

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = slide,
    out = right
  )

}

object SlideBuilder {
  sealed trait BuildState

  sealed trait WithoutSlide extends BuildState

  sealed trait SlideAdded extends BuildState

  type WithContentSlide = WithoutSlide with SlideAdded

  def apply[F[_]](): SlideBuilder[F, WithoutSlide] =
    builders.SlideBuilder(null, None)
}

