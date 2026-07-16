package com.github.morotsman.lote.internal.builders

import cats.effect.Temporal
import com.github.morotsman.lote.api.spi.{NConsole, Transition}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, VerticalAlignment}
import com.github.morotsman.lote.internal.builders.TextSlideBuilder.{BuildState, ContentAdded}
import com.github.morotsman.lote.internal.model.SlideSpecification
import com.github.morotsman.lote.internal.TextSlide

/** Builder for text slides.
  *
  * Uses a phantom-type state parameter to ensure `build()` is only available after `content(...)` has been supplied. In
  * other words, this will not compile:
  * {{{
  * TextSlideBuilder[IO]().build()
  * }}}
  * while this will:
  * {{{
  * TextSlideBuilder[IO]().content("Hello").build()
  * }}}
  */
private[lote] final class TextSlideBuilder[
    F[_]: Temporal: NConsole,
    State <: BuildState
] private (
    private val currentAlignment: Option[Alignment],
    private val currentContent: Option[String],
    private val currentSteps: Vector[String],
    private val currentSeparator: String,
    private val currentHint: Option[String],
    private val currentOut: Option[Transition[F]],
    private val currentTitle: Option[String]
) extends SlideMetadataBuilderOps[F, TextSlideBuilder[F, State]] {

  private def copy[NextState <: BuildState](
      alignment: Option[Alignment] = currentAlignment,
      content: Option[String] = currentContent,
      steps: Vector[String] = currentSteps,
      separator: String = currentSeparator,
      hint: Option[String] = currentHint,
      out: Option[Transition[F]] = currentOut,
      slideTitle: Option[String] = currentTitle
  ): TextSlideBuilder[F, NextState] =
    new TextSlideBuilder[F, NextState](alignment, content, steps, separator, hint, out, slideTitle)

  override protected def withTransition(
      transition: Transition[F]
  ): TextSlideBuilder[F, State] =
    this.copy[State](out = Some(transition))

  def content(content: String): TextSlideBuilder[F, State with ContentAdded] =
    this.copy[State with ContentAdded](content = Some(content))

  def step(step: String): TextSlideBuilder[F, State] =
    this.copy[State](steps = currentSteps :+ step)

  def separator(separator: String): TextSlideBuilder[F, State] =
    this.copy[State](separator = separator)

  def hint(hint: String): TextSlideBuilder[F, State] =
    this.copy[State](hint = Some(hint))

  def alignment(alignment: Alignment): TextSlideBuilder[F, State] =
    this.copy[State](alignment = Some(alignment))

  override protected def withTitle(title: String): TextSlideBuilder[F, State] =
    this.copy[State](slideTitle = Some(title))

  /** Finalizes the slide specification.
    *
    * This method is only available after `content(...)` has been called, enforced by the `State <:< ContentAdded`
    * evidence parameter.
    */
  private[lote] def build()(implicit ev: State <:< ContentAdded): SlideSpecification[F] = {
    val _ = ev
    val resolvedAlignment = currentAlignment.getOrElse(
      Alignment(VerticalAlignment.Center, HorizontalAlignment.Center)
    )
    val resolvedContent = currentContent.getOrElse(
      throw new IllegalStateException("TextSlideBuilder.build called before content was set")
    )
    val resolvedHint = currentHint match {
      case Some(value) if value.nonEmpty => Some(value)
      case Some(_)                       => None
      case None if currentSteps.nonEmpty => Some(TextSlide.DefaultStepHint)
      case None                          => None
    }
    val textSlide = TextSlide(
      resolvedContent,
      resolvedAlignment
    )
    val slide =
      if (currentSteps.isEmpty) {
        textSlide
      } else {
        TextSlide.staged(
          resolvedContent,
          currentSteps,
          resolvedAlignment,
          separator = currentSeparator,
          hint = resolvedHint
        )
      }

    val builderWithSlide = SlideBuilder[F]().addSlide(slide)
    val builderWithTransition = currentOut.fold(builderWithSlide)(builderWithSlide.transition)
    val builderWithMetadata = currentTitle.fold(builderWithTransition)(builderWithTransition.title)

    builderWithMetadata.build()
  }

}

private[lote] object TextSlideBuilder {
  type WithContent = WithoutContent with ContentAdded

  /** Creates a new text slide builder in the `WithoutContent` state. */
  def apply[F[_]: Temporal]()(implicit
      console: NConsole[F]
  ): TextSlideBuilder[F, WithoutContent] =
    new TextSlideBuilder(None, None, Vector.empty, TextSlide.DefaultStepSeparator, None, None, None)

  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState
}
