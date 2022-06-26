package com.github.morotsman
package lote.model

import lote.algebra.{Slide, Transition}

final case class SlideSpecification[F[_]](
                                           left: Option[Transition[F]],
                                           slide: Slide[F],
                                           right: Option[Transition[F]]
                                         )
