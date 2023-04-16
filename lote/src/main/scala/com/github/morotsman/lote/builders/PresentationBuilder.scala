package com.github.morotsman.lote.builders

import cats.Functor
import cats.effect.Sync
import com.github.morotsman.lote.algebra.{NConsole, Overlay, Slide}
import com.github.morotsman.lote.builders.PresentationBuilder.{BuildState, Buildable, SlideAdded}
import com.github.morotsman.lote.builders.SlideBuilder.{WithContentSlide, WithoutSlide}
import com.github.morotsman.lote.builders.TextSlideBuilder.{WithContent, WithoutContent}
import com.github.morotsman.lote.interpreter.TextSlide.ToTextSlide
import com.github.morotsman.lote.model.{Presentation, SlideSpecification}

case class PresentationBuilder[F[_] : Sync : Functor: NConsole, State <: BuildState](
                                                                            slideSpecifications: List[SlideSpecification[F]],
                                                                            exitSlide: Option[Slide[F]],
                                                                            overlays: List[Overlay[F]]
                                                                          ) {
  def addSlide(
                slideBuilder: SlideBuilder[F, WithoutSlide] => SlideBuilder[F, WithContentSlide]
              ): PresentationBuilder[F, State with SlideAdded] = {
    val builder: SlideBuilder[F, WithoutSlide] = SlideBuilder()
    val slideSpecification = slideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  def addTextSlide(
                    textSlideBuilder: TextSlideBuilder[F, WithoutContent] => TextSlideBuilder[F, WithContent]
                  ): PresentationBuilder[F, State with SlideAdded] = {
    val builder = TextSlideBuilder()
    val slideSpecification = textSlideBuilder(builder).build()
    this.copy(slideSpecifications = slideSpecification :: slideSpecifications)
  }

  def addExitSlide(slide: Slide[F]): PresentationBuilder[F, State] =
    this.copy(exitSlide = Option(slide))

  def addExitSlide(s: String): PresentationBuilder[F, State] =
    this.copy(exitSlide = Option(s.toSlide()))

  def addOverlay(overlay: Overlay[F]): PresentationBuilder[F, State] = {
    this.copy(overlays = overlay :: overlays)
  }

  def build()(implicit ev: State =:= Buildable): Presentation[F] = Presentation(
    slideSpecifications = slideSpecifications.reverse,
    exitSlide = exitSlide
  )

}

object PresentationBuilder {
  type Buildable = Empty with SlideAdded

  def apply[F[_] : Sync: NConsole](): PresentationBuilder[F, Empty] =
    PresentationBuilder[F, Empty](List.empty, None, List.empty)

  sealed trait BuildState

  sealed trait Empty extends BuildState

  sealed trait SlideAdded extends BuildState
}

