package com.github.morotsman
package lote.model

import lote.algebra.{Slide, Transition}

final case class SlideSpecification[F[_]](
                                           in: Option[Transition[F]],
                                           slide: Slide[F],
                                           out: Option[Transition[F]]
                                         )
