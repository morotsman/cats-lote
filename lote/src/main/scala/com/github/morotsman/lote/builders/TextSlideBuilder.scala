package com.github.morotsman.lote.builders

import cats.effect.Sync
import com.github.morotsman.lote.algebra.{NConsole, Transition}
import com.github.morotsman.lote.builders.TextSlideBuilder.{BuildState, ContentAdded}
import com.github.morotsman.lote.interpreter.TextSlide
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, SlideSpecification, VerticalAlignment}

final case class TextSlideBuilder[F[_] : Sync : NConsole, State <: BuildState](
                                                     alignment: Option[Alignment],
                                                     content: String,
                                                     left: Option[Transition[F]],
                                                     right: Option[Transition[F]]
                                                   ) {

  def transition(
                     left: Transition[F] = null,
                     right: Transition[F] = null
                   ): TextSlideBuilder[F, State] =
    this.copy(left = Option(left), right = Option(right))


  def content(content: String): TextSlideBuilder[F, State with ContentAdded] =
    this.copy(content = content)

  def alignment(alignment: Alignment): TextSlideBuilder[F, State] =
    this.copy(alignment = Option(alignment))

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = TextSlide(content, alignment.getOrElse(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))),
    left = left,
    right = right
  )

}

object TextSlideBuilder {
  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState

  type WithContent = WithoutContent with ContentAdded

  def apply[F[_] : NConsole : Sync](): TextSlideBuilder[F, WithoutContent] =
    TextSlideBuilder(None, null, None, None)
}

