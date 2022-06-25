package com.github.morotsman
package lote.model

case class Presentation[F[_]](
                               slideSpecifications: List[SlideSpecification[F]],
                               exitSlide: Option[SlideSpecification[F]] = None
                             )
