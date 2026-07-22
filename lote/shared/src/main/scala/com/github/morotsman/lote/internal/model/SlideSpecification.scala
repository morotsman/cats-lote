package com.github.morotsman.lote.internal.model

import com.github.morotsman.lote.api.SlidePosition
import com.github.morotsman.lote.api.spi.{Slide, Transition}

private[lote] final case class SlideSpecification[F[_]](
    slide: Slide[F],
    out: Option[Transition[F]],
    title: Option[String] = None,
    position: Option[SlidePosition] = None,
    positionOffset: Option[(Double, Double, Double)] = None,
    rotationOffset: Option[(Double, Double, Double)] = None
)
