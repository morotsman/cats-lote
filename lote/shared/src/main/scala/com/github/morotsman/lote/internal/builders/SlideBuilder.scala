package com.github.morotsman.lote.internal.builders

import com.github.morotsman.lote.api.SlidePosition
import com.github.morotsman.lote.api.spi.{Slide, Transition}
import com.github.morotsman.lote.internal.builders.SlideBuilder.{BuildState, SlideAdded}
import com.github.morotsman.lote.internal.model.SlideSpecification

/** Builder for custom slides.
  *
  * Uses a phantom-type state parameter to ensure `build()` is only available after `addSlide(...)` has been called.
  * This means incomplete builders are rejected by the compiler rather than relying on runtime placeholder values.
  */
private[lote] final class SlideBuilder[F[_], State <: BuildState] private (
    private val currentSlide: Option[Slide[F]],
    private val currentTransition: Option[Transition[F]],
    private val currentTitle: Option[String],
    private val currentPosition: Option[SlidePosition]
) extends SlideMetadataBuilderOps[F, SlideBuilder[F, State]] {

  private def copy[NextState <: BuildState](
      slide: Option[Slide[F]] = currentSlide,
      transition: Option[Transition[F]] = currentTransition,
      slideTitle: Option[String] = currentTitle,
      slidePosition: Option[SlidePosition] = currentPosition
  ): SlideBuilder[F, NextState] =
    new SlideBuilder[F, NextState](slide, transition, slideTitle, slidePosition)

  override protected def withTransition(
      right: Transition[F]
  ): SlideBuilder[F, State] =
    this.copy[State](transition = Some(right))

  override protected def withTitle(title: String): SlideBuilder[F, State] =
    this.copy[State](slideTitle = Some(title))

  override protected def withPosition(position: SlidePosition): SlideBuilder[F, State] =
    this.copy[State](slidePosition = currentPosition match {
      case Some(existing) =>
        // Merge: if new position has non-zero coords use them, otherwise keep existing
        Some(SlidePosition(
          x = if (position.x != 0.0 || existing.x == 0.0) position.x else existing.x,
          y = if (position.y != 0.0 || existing.y == 0.0) position.y else existing.y,
          z = if (position.z != 0.0 || existing.z == 0.0) position.z else existing.z,
          rotX = if (position.rotX != 0.0 || existing.rotX == 0.0) position.rotX else existing.rotX,
          rotY = if (position.rotY != 0.0 || existing.rotY == 0.0) position.rotY else existing.rotY,
          rotZ = if (position.rotZ != 0.0 || existing.rotZ == 0.0) position.rotZ else existing.rotZ,
          transparentBackground = position.transparentBackground || existing.transparentBackground
        ))
      case None => Some(position)
    })

  def addSlide(slide: Slide[F]): SlideBuilder[F, State with SlideAdded] =
    this.copy[State with SlideAdded](slide = Some(slide))

  /** Finalizes the slide specification.
    *
    * This method is only available after `addSlide(...)` has been called, enforced by the `State <:< SlideAdded`
    * evidence parameter.
    */
  private[lote] def build()(implicit ev: State <:< SlideAdded): SlideSpecification[F] = {
    val _ = ev
    SlideSpecification(
      slide = currentSlide.getOrElse(
        throw new IllegalStateException("SlideBuilder.build called before a slide was added")
      ),
      out = currentTransition,
      title = currentTitle,
      position = currentPosition
    )
  }

}

private[lote] object SlideBuilder {
  type WithContentSlide = WithoutSlide with SlideAdded

  /** Creates a new custom slide builder in the `WithoutSlide` state. */
  def apply[F[_]](): SlideBuilder[F, WithoutSlide] =
    new SlideBuilder[F, WithoutSlide](None, None, None, None)

  sealed trait BuildState

  sealed trait WithoutSlide extends BuildState

  sealed trait SlideAdded extends BuildState
}
