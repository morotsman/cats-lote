package com.github.morotsman
package lote.model

import com.github.morotsman.lote.algebra.{Slide, Transition}

final case class SlideSpecification[F[_]](slide: Slide[F], out: Option[Transition[F]])
