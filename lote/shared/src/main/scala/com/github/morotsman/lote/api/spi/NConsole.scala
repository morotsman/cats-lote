package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.{Alignment, PlatformCapability, RenderEffect, Screen, ScreenAdjusted, UserInput}

import scala.annotation.implicitNotFound

@implicitNotFound("No implicit NConsole[${F}] found. An NConsole instance is provided by SlideContext inside slide/transition/overlay builders, or by SlideTestHarness in tests.")
trait NConsole[F[_]] {
  def read(timeoutInMillis: Long): F[UserInput]

  def read(): F[UserInput]

  def readInterruptible(): F[UserInput]

  def alignText(s: String, alignment: Alignment): F[ScreenAdjusted]

  def writeString(s: ScreenAdjusted): F[Unit]

  def clear(): F[Unit]

  def close(): F[Unit]

  def context: F[Screen]

  /** Returns the platform capabilities of the underlying terminal backend.
    *
    * Transitions can use this to decide whether to use plain character-grid animation or richer visual effects.
    */
  def capabilities: Set[PlatformCapability] = Set(PlatformCapability.CharacterGrid)

  /** Apply a visual effect to the current rendering.
    *
    * On capable backends (WebGL), this triggers GPU-accelerated effects like 3D flips, dissolve, smoke, etc.
    * On terminal-only backends (JLine, xterm.js), this is a no-op.
    */
  def applyEffect(effect: RenderEffect): F[Unit]

  /** Returns a reference to the shared 3D scene, if the backend supports spatial mode.
    *
    * On WebGL backends in spatial mode, this returns `Some(Scene3DRef)` which can be cast
    * to `com.github.morotsman.lote.api.Scene3DRef` for adding 3D geometry to the shared scene.
    * On terminal backends this returns `None`.
    */
  def scene3DRef: Option[Any] = None
}

object NConsole {
  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] =
    instance
}
