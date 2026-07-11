package com.github.morotsman
package lote.model

import com.github.morotsman.lote.algebra.Overlay

case class Presentation[F[_]](
    slideSpecifications: List[SlideSpecification[F]],
    overlays: List[Overlay[F]] = List.empty
) {
  def titles: Vector[String] = slideSpecifications.zipWithIndex.map { case (spec, index) =>
    spec.title.getOrElse(s"Slide ${index + 1}")
  }.toVector
}
