package com.github.morotsman
package lote

import com.github.morotsman.lote.SlideBuilder.{BuildState, SlideAdded}
import lote.algebra.{Slide, Transition}
import lote.model.SlideSpecification

final case class SlideBuilder[F[_], State <: BuildState](
                                            slide: Slide[F],
                                            left: Option[Transition[F]],
                                            right: Option[Transition[F]]
                                          ) {

  def transition(
                     left: Transition[F] = null,
                     right: Transition[F] = null
                   ): SlideBuilder[F, State] =
    this.copy(left = Option(left), right = Option(right))

  def slide(slide: Slide[F]): SlideBuilder[F, State with SlideAdded] =
    this.copy(slide = slide)

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = slide,
    left = left,
    right = right
  )

}

object SlideBuilder {
  sealed trait BuildState

  sealed trait WithoutSlide extends BuildState

  sealed trait SlideAdded extends BuildState

  type WithContentSlide = WithoutSlide with SlideAdded

  def apply[F[_]](): SlideBuilder[F, WithoutSlide] =
    SlideBuilder(null, None, None)
}

