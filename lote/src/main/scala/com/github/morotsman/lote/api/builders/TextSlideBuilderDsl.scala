package com.github.morotsman.lote.api.builders

import com.github.morotsman.lote.api.Alignment
import com.github.morotsman.lote.internal.model.SlideSpecification

trait TextSlideBuilderStart[F[_]] extends SlideMetadataDsl[F, TextSlideBuilderStart[F]] {
  def content(content: String): TextSlideBuilderReady[F]

  def separator(separator: String): TextSlideBuilderStart[F]

  def hint(hint: String): TextSlideBuilderStart[F]

  def alignment(alignment: Alignment): TextSlideBuilderStart[F]
}

trait TextSlideBuilderReady[F[_]] extends SlideMetadataDsl[F, TextSlideBuilderReady[F]] {
  def content(content: String): TextSlideBuilderReady[F]

  def step(step: String): TextSlideBuilderReady[F]

  def separator(separator: String): TextSlideBuilderReady[F]

  def hint(hint: String): TextSlideBuilderReady[F]

  def alignment(alignment: Alignment): TextSlideBuilderReady[F]

  private[lote] def buildSpec(): SlideSpecification[F]
}
