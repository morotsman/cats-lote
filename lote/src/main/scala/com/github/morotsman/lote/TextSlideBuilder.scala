package com.github.morotsman.lote

import cats.effect.Sync
import com.github.morotsman.lote.TextSlideBuilder._
import com.github.morotsman.lote.algebra.{NConsole, Transition}
import com.github.morotsman.lote.interpreter.SimpleSlide
import com.github.morotsman.lote.model.{Alignment, SlideSpecification}

final case class TextSlideBuilder[F[_] : Sync : NConsole, State <: BuildState](
                                                     alignment: Alignment,
                                                     content: String,
                                                     left: Transition[F],
                                                     right: Transition[F]
                                                   ) {

  def transition(
                     left: Transition[F] = null,
                     right: Transition[F] = null
                   ): TextSlideBuilder[F, State] =
    this.copy(left = left, right = right)


  def content(content: String): TextSlideBuilder[F, State with ContentAdded] =
    this.copy(content = content)

  def alignment(alignment: Alignment): TextSlideBuilder[F, State] =
    this.copy(alignment = alignment)

  def build(): SlideSpecification[F] = SlideSpecification(
    slide = SimpleSlide(content),
    left = Option(left),
    right = Option(right)
  )

}

object TextSlideBuilder {
  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState

  type WithContent = WithoutContent with ContentAdded

  def apply[F[_] : NConsole : Sync](): TextSlideBuilder[F, WithoutContent] =
    TextSlideBuilder(null, null, null, null)
}

