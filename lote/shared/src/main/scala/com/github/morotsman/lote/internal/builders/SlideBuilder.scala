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
    private val currentPosition: Option[SlidePosition],
    private val currentOffset: Option[(Double, Double, Double)],
    private val currentRotationOffset: Option[(Double, Double, Double)]
) extends SlideMetadataBuilderOps[F, SlideBuilder[F, State]] {

  private def copy[NextState <: BuildState](
      slide: Option[Slide[F]] = currentSlide,
      transition: Option[Transition[F]] = currentTransition,
      slideTitle: Option[String] = currentTitle,
      slidePosition: Option[SlidePosition] = currentPosition,
      slideOffset: Option[(Double, Double, Double)] = currentOffset,
      slideRotationOffset: Option[(Double, Double, Double)] = currentRotationOffset
  ): SlideBuilder[F, NextState] =
    new SlideBuilder[F, NextState](slide, transition, slideTitle, slidePosition, slideOffset, slideRotationOffset)

  override protected def withTransition(
      right: Transition[F]
  ): SlideBuilder[F, State] =
    this.copy[State](transition = Some(right))

  override protected def currentTransitionOpt: Option[Transition[F]] =
    currentTransition

  override protected def withTitle(title: String): SlideBuilder[F, State] =
    this.copy[State](slideTitle = Some(title))

  private def mergePositions(existing: SlidePosition, incoming: SlidePosition): SlidePosition =
    SlidePosition(
      x = if (incoming.x != 0.0 || existing.x == 0.0) incoming.x else existing.x,
      y = if (incoming.y != 0.0 || existing.y == 0.0) incoming.y else existing.y,
      z = if (incoming.z != 0.0 || existing.z == 0.0) incoming.z else existing.z,
      rotX = if (incoming.rotX != 0.0 || existing.rotX == 0.0) incoming.rotX else existing.rotX,
      rotY = if (incoming.rotY != 0.0 || existing.rotY == 0.0) incoming.rotY else existing.rotY,
      rotZ = if (incoming.rotZ != 0.0 || existing.rotZ == 0.0) incoming.rotZ else existing.rotZ,
      transparentBackground = incoming.transparentBackground || existing.transparentBackground
    )

  override protected def withPosition(position: SlidePosition): SlideBuilder[F, State] =
    this.copy[State](slidePosition = currentPosition match {
      case Some(existing) => Some(mergePositions(existing, position))
      case None => Some(position)
    }, slideOffset = None)

  override protected def withPositionMerge(position: SlidePosition): SlideBuilder[F, State] =
    this.copy[State](slidePosition = currentPosition match {
      case Some(existing) => Some(mergePositions(existing, position))
      case None => Some(position)
    })

  override protected def withOffset(dx: Double, dy: Double, dz: Double): SlideBuilder[F, State] = {
    val newOffset = currentOffset match {
      case Some((ox, oy, oz)) => Some((ox + dx, oy + dy, oz + dz))
      case None => Some((dx, dy, dz))
    }
    this.copy[State](slideOffset = newOffset)
  }

  override protected def withRotationOffset(drx: Double, dry: Double, drz: Double): SlideBuilder[F, State] = {
    val newRotOffset = currentRotationOffset match {
      case Some((rx, ry, rz)) => Some((rx + drx, ry + dry, rz + drz))
      case None => Some((drx, dry, drz))
    }
    this.copy[State](slideRotationOffset = newRotOffset)
  }

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
      position = currentPosition,
      positionOffset = currentOffset,
      rotationOffset = currentRotationOffset
    )
  }

}

private[lote] object SlideBuilder {
  type WithContentSlide = WithoutSlide with SlideAdded

  /** Creates a new custom slide builder in the `WithoutSlide` state. */
  def apply[F[_]](): SlideBuilder[F, WithoutSlide] =
    new SlideBuilder[F, WithoutSlide](None, None, None, None, None, None)

  sealed trait BuildState

  sealed trait WithoutSlide extends BuildState

  sealed trait SlideAdded extends BuildState
}
