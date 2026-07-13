package com.github.morotsman.lote.api.builders

import cats.Functor
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.internal.model.SlideSpecification

trait SlideBuilderStart[F[_]] extends SlideMetadataDsl[F, SlideBuilderStart[F]] {
  def addSlide(slide: Slide[F]): SlideBuilderReady[F]

  def addSlide(slide: Contextual[F, Slide[F]]): SlideBuilderReady[F]

  def addSlideF(slide: ContextualF[F, Slide[F]])(implicit functor: Functor[F]): F[SlideBuilderReady[F]]
}

trait SlideBuilderReady[F[_]] extends SlideMetadataDsl[F, SlideBuilderReady[F]] {
  def addSlide(slide: Slide[F]): SlideBuilderReady[F]

  def addSlide(slide: Contextual[F, Slide[F]]): SlideBuilderReady[F]

  def addSlideF(slide: ContextualF[F, Slide[F]])(implicit functor: Functor[F]): F[SlideBuilderReady[F]]

  private[lote] def buildSpec(): SlideSpecification[F]
}

