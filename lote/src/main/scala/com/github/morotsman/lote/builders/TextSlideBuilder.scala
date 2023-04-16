package com.github.morotsman.lote.builders

import cats.effect.{Temporal}
import com.github.morotsman.lote.algebra.{NConsole, Transition}
import com.github.morotsman.lote.builders.TextSlideBuilder.{BuildState, ContentAdded}
import com.github.morotsman.lote.interpreter.TextSlide
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, SlideSpecification, VerticalAlignment}

final case class TextSlideBuilder[F[_] : Temporal, State <: BuildState](
                                                                     alignment: Option[Alignment],
                                                                     content: String,
                                                                     in: Option[Transition[F]],
                                                                     out: Option[Transition[F]]
                                                                   )(implicit console: NConsole[F]) {

  def transition(
                  in: Transition[F] = null,
                  out: Transition[F] = null
                ): TextSlideBuilder[F, State] =
    this.copy(in = Option(in), out = Option(out))


  def content(content: String): TextSlideBuilder[F, State with ContentAdded] =
    this.copy(content = content)

  def alignment(alignment: Alignment): TextSlideBuilder[F, State] =
    this.copy(alignment = Option(alignment))

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = TextSlide(content, alignment.getOrElse(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))),
    out = out
  )

}

object TextSlideBuilder {
  type WithContent = WithoutContent with ContentAdded

  def apply[F[_] : Temporal]()(implicit console: NConsole[F]): TextSlideBuilder[F, WithoutContent] =
    TextSlideBuilder(None, null, None, None)

  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState
}

