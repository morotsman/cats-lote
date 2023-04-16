package com.github.morotsman.lote.builders

import cats.effect.Sync
import com.github.morotsman.lote.algebra.{NConsole, Transition}
import com.github.morotsman.lote.builders.TextSlideBuilder.{BuildState, ContentAdded}
import com.github.morotsman.lote.interpreter.TextSlide
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, SlideSpecification, VerticalAlignment}

final case class TextSlideBuilder[F[_] : Sync, State <: BuildState](
                                                                     console: NConsole[F],
                                                                     alignment: Option[Alignment],
                                                                     content: String,
                                                                     in: Option[Transition[F]],
                                                                     out: Option[Transition[F]]
                                                                   ) {

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
    slide = TextSlide(console, content, alignment.getOrElse(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))),
    out = out
  )

}

object TextSlideBuilder {
  type WithContent = WithoutContent with ContentAdded

  def apply[F[_] : Sync](console: NConsole[F]): TextSlideBuilder[F, WithoutContent] =
    TextSlideBuilder(console, None, null, None, None)

  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState
}

