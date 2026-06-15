package com.github.morotsman
package lote.model

import com.github.morotsman.lote.algebra.Slide

case class Presentation[F[_]](
    slideSpecifications: List[SlideSpecification[F]],
    exitSlide: Option[Slide[F]] = None
) {
  def titles: Vector[String] = slideSpecifications.zipWithIndex.map {
    case (spec, index) => spec.title.getOrElse(s"Slide ${index + 1}")
  }.toVector
}
