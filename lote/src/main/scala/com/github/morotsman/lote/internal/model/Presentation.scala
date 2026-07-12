package com.github.morotsman.lote.internal.model

import com.github.morotsman.lote.api.spi.Overlay

private[lote] case class Presentation[F[_]](
    slideSpecifications: List[SlideSpecification[F]],
    overlays: List[Overlay[F]] = List.empty
) {
  def titles: Vector[String] = slideSpecifications.zipWithIndex.map { case (spec, index) =>
    spec.title.getOrElse(s"Slide ${index + 1}")
  }.toVector
}
