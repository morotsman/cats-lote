package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.kernel.Ref
import cats.implicits._
import com.github.morotsman.lote.api.{RenderEffect, ScreenAdjusted, SlidePosition}
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.internal.algebra.PlatformStrategy
import com.github.morotsman.lote.internal.model.SlideSpecification

/** WebGL / spatial platform strategy.
  *
  * Manages 3D spatial layout: initializes layers per slide, pre-renders the first slide at each unique position,
  * activates layers before content writes, and animates camera navigation between slide positions (interruptible by
  * user input).
  *
  * Use the companion `apply` method to create an instance (it allocates the internal `Ref`).
  */
private[lote] class SpatialPlatformStrategy[F[_]: Temporal: NConsole] private (
    specs: List[SlideSpecification[F]],
    lastCameraPosRef: Ref[F, Option[SlidePosition]]
) extends PlatformStrategy[F] {

  // Resolve positions: slides can have absolute position, relative offset, or inherit from previous.
  private val resolvedPositions: Vector[SlidePosition] =
    specs
      .foldLeft((SlidePosition(), Vector.empty[SlidePosition])) { case ((lastPos, acc), spec) =>
        val pos = (spec.positionOffset, spec.position) match {
          case (Some((dx, dy, dz)), maybePosForAttrs) =>
            // Relative offset from previous slide; pick up rotation/transparency from position if set
            val attrs = maybePosForAttrs.getOrElse(SlidePosition())
            SlidePosition(
              x = lastPos.x + dx,
              y = lastPos.y + dy,
              z = lastPos.z + dz,
              rotX = attrs.rotX,
              rotY = attrs.rotY,
              rotZ = attrs.rotZ,
              transparentBackground = attrs.transparentBackground
            )
          case (None, Some(absolutePos)) => absolutePos
          case (None, None)              => lastPos
        }
        // Apply relative rotation offset if set
        val rotated = spec.rotationOffset match {
          case Some((drx, dry, drz)) =>
            pos.copy(rotX = pos.rotX + drx, rotY = pos.rotY + dry, rotZ = pos.rotZ + drz)
          case None => pos
        }
        (rotated, acc :+ rotated)
      }
      ._2

  // Track which slides are the first at each unique position (for pre-rendering).
  private val isFirstAtPosition: Vector[Boolean] = {
    val seenPositions = scala.collection.mutable.Set.empty[(Double, Double, Double)]
    resolvedPositions.map { pos =>
      val key = (pos.x, pos.y, pos.z)
      if (seenPositions.contains(key)) false
      else { seenPositions += key; true }
    }
  }

  override def setupPlatform(): F[Unit] = {
    val positions: Vector[Option[SlidePosition]] = resolvedPositions.map(Some(_))
    val initLayout = NConsole[F].applyEffect(RenderEffect.InitSpatialLayout(positions))

    val preRender = specs.zipWithIndex.traverse_ { case (spec, idx) =>
      if (isFirstAtPosition(idx)) {
        for {
          _ <- NConsole[F].applyEffect(RenderEffect.ActivateLayer(idx))
          content <- spec.slide.content
          _ <- content match {
            case Some(c) => NConsole[F].writeString(c)
            case None    => NConsole[F].writeString(ScreenAdjusted(""))
          }
        } yield ()
      } else {
        Monad[F].unit
      }
    }

    initLayout >> preRender
  }

  override def activateSlide(index: Int): F[Unit] =
    NConsole[F].applyEffect(RenderEffect.ActivateLayer(index))

  override def navigateToSlide(index: Int): F[Unit] = {
    val targetPos = resolvedPositions(index)
    for {
      lastCameraPos <- lastCameraPosRef.getAndSet(Some(targetPos))
      posChanged = !lastCameraPos.contains(targetPos)
      _ <-
        if (posChanged)
          Temporal[F]
            .race(
              NConsole[F].applyEffect(RenderEffect.MoveCameraTo(targetPos)),
              NConsole[F].readInterruptible()
            )
            .flatMap {
              case Left(_)  => Monad[F].unit
              case Right(_) => NConsole[F].applyEffect(RenderEffect.JumpCameraTo(targetPos))
            }
        else Monad[F].unit
    } yield ()
  }
}

private[lote] object SpatialPlatformStrategy {

  /** Creates a new [[SpatialPlatformStrategy]], allocating the internal `Ref` for camera position tracking. */
  def apply[F[_]: Temporal: NConsole](
      specs: List[SlideSpecification[F]]
  ): F[SpatialPlatformStrategy[F]] =
    Ref[F].of(Option.empty[SlidePosition]).map { ref =>
      new SpatialPlatformStrategy[F](specs, ref)
    }
}
