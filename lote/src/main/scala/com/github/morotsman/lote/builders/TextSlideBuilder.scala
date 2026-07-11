package com.github.morotsman.lote.builders

import cats.effect.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Transition}
import com.github.morotsman.lote.builders.TextSlideBuilder.{BuildState, ContentAdded}
import com.github.morotsman.lote.interpreter.TextSlide
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, SlideSpecification, VerticalAlignment}

/** Builder for text slides.
  *
  * Uses a phantom-type state parameter to ensure `build()` is only available after `content(...)` has been supplied.
  * In other words, this will not compile:
  * {{{
  * TextSlideBuilder[IO]().build()
  * }}}
  * while this will:
  * {{{
  * TextSlideBuilder[IO]().content("Hello").build()
  * }}}
  */
final class TextSlideBuilder[
    F[_]: Temporal: NConsole,
    State <: BuildState
] private (
    private val currentAlignment: Option[Alignment],
    private val currentContent: Option[String],
    private val currentOut: Option[Transition[F]],
    private val currentTitle: Option[String]
) {

  private def copy[NextState <: BuildState](
      alignment: Option[Alignment] = currentAlignment,
      content: Option[String] = currentContent,
      out: Option[Transition[F]] = currentOut,
      slideTitle: Option[String] = currentTitle
  ): TextSlideBuilder[F, NextState] =
    new TextSlideBuilder[F, NextState](alignment, content, out, slideTitle)

  def transition(
      transition: Transition[F]
  ): TextSlideBuilder[F, State] =
    this.copy[State](out = Some(transition))

  def content(content: String): TextSlideBuilder[F, State with ContentAdded] =
    this.copy[State with ContentAdded](content = Some(content))

  def alignment(alignment: Alignment): TextSlideBuilder[F, State] =
    this.copy[State](alignment = Some(alignment))

  def title(title: String): TextSlideBuilder[F, State] =
    this.copy[State](slideTitle = Some(title))

  /** Finalizes the slide specification.
    *
    * This method is only available after `content(...)` has been called, enforced by the `State <:< ContentAdded`
    * evidence parameter.
    */
  def build()(implicit ev: State <:< ContentAdded): SlideSpecification[F] = {
    val _ = ev
    SlideSpecification(
      slide = TextSlide(
        currentContent.getOrElse(
          throw new IllegalStateException("TextSlideBuilder.build called before content was set")
        ),
        currentAlignment.getOrElse(
          Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
        )
      ),
      out = currentOut,
      title = currentTitle
    )
  }

}

object TextSlideBuilder {
  type WithContent = WithoutContent with ContentAdded

  /** Creates a new text slide builder in the `WithoutContent` state. */
  def apply[F[_]: Temporal]()(implicit
      console: NConsole[F]
  ): TextSlideBuilder[F, WithoutContent] =
    new TextSlideBuilder(None, None, None, None)

  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState
}
