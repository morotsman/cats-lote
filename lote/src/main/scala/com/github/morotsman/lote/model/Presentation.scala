package com.github.morotsman
package lote.model

import com.github.morotsman.lote.algebra.Slide

case class Presentation[F[_]](
                               slideSpecifications: List[SlideSpecification[F]],
                               exitSlide: Option[Slide[F]] = None
                             )
