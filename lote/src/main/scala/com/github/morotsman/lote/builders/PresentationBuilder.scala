package com.github.morotsman.lote.builders

import cats.effect.Temporal
import com.github.morotsman.lote.algebra.{NConsole, Overlay}
import com.github.morotsman.lote.builders.PresentationBuilder.{BuildState, Buildable, SlideAdded}
import com.github.morotsman.lote.builders.SlideBuilder.{WithContentSlide, WithoutSlide}
import com.github.morotsman.lote.builders.TextSlideBuilder.{WithContent, WithoutContent}
import com.github.morotsman.lote.model.{Presentation, SlideSpecification}

/** Lower-level presentation builder.
  *
  * This builder uses phantom types to prevent incomplete presentations from being built:
  *   - `addTextSlide(...)` only accepts functions that return a `TextSlideBuilder` in the `WithContent` state
  *   - `addSlide(...)` only accepts functions that return a `SlideBuilder` in the `WithContentSlide` state
  *   - `build()` is only available after at least one slide has been added
  */
case class PresentationBuilder[F[
    _
]: Temporal: NConsole, State <: BuildState](
    slideSpecifications: List[SlideSpecification[F]],
    overlays: List[Overlay[F]]
) {
  /** Adds a custom slide.
    *
    * The supplied function must return a completed `SlideBuilder`, so forgetting to call `addSlide(...)` is a
    * compile-time error.
    */
  def addSlide(
      slideBuilder: SlideBuilder[F, WithoutSlide] => SlideBuilder[
        F,
        WithContentSlide
      ]
  ): PresentationBuilder[F, State with SlideAdded] = {
    val builder: SlideBuilder[F, WithoutSlide] = SlideBuilder()
    val slideSpecification = slideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  /** Adds a text slide.
    *
    * The supplied function must return a completed `TextSlideBuilder`, so forgetting to call `content(...)` is a
    * compile-time error.
    */
  def addTextSlide(
      textSlideBuilder: TextSlideBuilder[F, WithoutContent] => TextSlideBuilder[
        F,
        WithContent
      ]
  ): PresentationBuilder[F, State with SlideAdded] = {
    val builder = TextSlideBuilder()
    val slideSpecification = textSlideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  /** Finalizes the presentation.
    *
    * This method is only available after at least one slide has been added, enforced by the `State =:= Buildable`
    * evidence parameter.
    */
  def build()(implicit ev: State =:= Buildable): Presentation[F] = {
    val _ = ev
    Presentation(
      slideSpecifications = slideSpecifications.reverse,
      overlays = overlays.reverse
    )
  }

}

object PresentationBuilder {
  type Buildable = Empty with SlideAdded

  /** Creates an empty `PresentationBuilder`. */
  def apply[F[_]: Temporal: NConsole](): PresentationBuilder[F, Empty] =
    PresentationBuilder[F, Empty](List.empty, List.empty)

  sealed trait BuildState

  sealed trait Empty extends BuildState

  sealed trait SlideAdded extends BuildState
}
