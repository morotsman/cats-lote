package com.github.morotsman.lote.internal.builders

import cats.effect.Temporal
import com.github.morotsman.lote.api.spi.{NConsole, Transition}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, SlidePosition, VerticalAlignment}
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
    private val currentTitle: Option[String],
    private val currentPosition: Option[SlidePosition],
    private val currentOffset: Option[(Double, Double, Double)],
    private val currentRotationOffset: Option[(Double, Double, Double)]
) extends SlideMetadataBuilderOps[F, TextSlideBuilder[F, State]] {

  private def copy[NextState <: BuildState](
      alignment: Option[Alignment] = currentAlignment,
      content: Option[String] = currentContent,
      steps: Vector[String] = currentSteps,
      separator: String = currentSeparator,
      hint: Option[String] = currentHint,
      out: Option[Transition[F]] = currentOut,
      slideTitle: Option[String] = currentTitle,
      slidePosition: Option[SlidePosition] = currentPosition,
      slideOffset: Option[(Double, Double, Double)] = currentOffset,
      slideRotationOffset: Option[(Double, Double, Double)] = currentRotationOffset
  ): TextSlideBuilder[F, NextState] =
    new TextSlideBuilder[F, NextState](alignment, content, steps, separator, hint, out, slideTitle, slidePosition, slideOffset, slideRotationOffset)

  override protected def withTransition(
      transition: Transition[F]
  ): TextSlideBuilder[F, State] =
    this.copy[State](out = Some(transition))

  override protected def currentTransitionOpt: Option[Transition[F]] =
    currentOut

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

  override protected def withPosition(position: SlidePosition): TextSlideBuilder[F, State] =
    this.copy[State](slidePosition = currentPosition match {
      case Some(existing) => Some(mergePositions(existing, position))
      case None => Some(position)
    }, slideOffset = None)

  override protected def withPositionMerge(position: SlidePosition): TextSlideBuilder[F, State] =
    this.copy[State](slidePosition = currentPosition match {
      case Some(existing) => Some(mergePositions(existing, position))
      case None => Some(position)
    })

  override protected def withOffset(dx: Double, dy: Double, dz: Double): TextSlideBuilder[F, State] = {
    val newOffset = currentOffset match {
      case Some((ox, oy, oz)) => Some((ox + dx, oy + dy, oz + dz))
      case None => Some((dx, dy, dz))
    }
    this.copy[State](slideOffset = newOffset)
  }

  override protected def withRotationOffset(drx: Double, dry: Double, drz: Double): TextSlideBuilder[F, State] = {
    val newRotOffset = currentRotationOffset match {
      case Some((rx, ry, rz)) => Some((rx + drx, ry + dry, rz + drz))
      case None => Some((drx, dry, drz))
    }
    this.copy[State](slideRotationOffset = newRotOffset)
  }

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
    val builderWithTitle = currentTitle.fold(builderWithTransition)(builderWithTransition.title)
    val builderWithPosition = currentPosition.fold(builderWithTitle)(builderWithTitle.position)
    val builderWithOffset = currentOffset match {
      case Some((dx, dy, dz)) => builderWithPosition.offset(dx, dy, dz)
      case None => builderWithPosition
    }
    val builderWithRotOffset = currentRotationOffset match {
      case Some((rx, ry, rz)) => builderWithOffset.rotateX(rx).rotateY(ry).rotateZ(rz)
      case None => builderWithOffset
    }

    builderWithRotOffset.build()
  }

}

private[lote] object TextSlideBuilder {
  type WithContent = WithoutContent with ContentAdded

  /** Creates a new text slide builder in the `WithoutContent` state. */
  def apply[F[_]: Temporal]()(implicit
      console: NConsole[F]
  ): TextSlideBuilder[F, WithoutContent] =
    new TextSlideBuilder(None, None, Vector.empty, TextSlide.DefaultStepSeparator, None, None, None, None, None, None)

  sealed trait BuildState

  sealed trait WithoutContent extends BuildState

  sealed trait ContentAdded extends BuildState
}
