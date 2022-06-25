package com.github.morotsman
package lote

import lote.algebra.{Slide, Transition}

import lote.model.SlideSpecification

case class SlideSpecificationBuilder[F[_]](
                                            slide: Slide[F],
                                            left: Option[Transition[F]],
                                            right: Option[Transition[F]]
                                          ) {

  def addTransition(
                     left: Transition[F] = null,
                     right: Transition[F] = null
                   ): SlideSpecificationBuilder[F] =
    this.copy(left = Option(left), right = Option(right))


  def addAlignment(): SlideSpecificationBuilder[F] =
    ???

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = slide,
    left = left,
    right = right
  )

}

