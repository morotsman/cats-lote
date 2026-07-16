package com.github.morotsman.lote.internal.algebra

import cats.Monad
import com.github.morotsman.lote.api.spi.Overlay
import com.github.morotsman.lote.internal.model.Presentation

/** A registered feature that bundles an overlay with lifecycle hooks.
  *
  * Instead of manually wiring each overlay's slide-change callback, title setup, and executor subscription in
  * `runSession`, each feature self-registers all its concerns through this trait. The session loop then processes
  * all features uniformly.
  */
private[lote] trait Feature[F[_]] {

  /** The overlay that renders this feature's visual output. */
  def overlay: Overlay[F]

  /** Called when the active slide changes. Use for progress updates, idle resets, etc. */
  def onSlideChange(index: Int): F[Unit]

  /** Called after the presentation is built. Use for wiring that needs slide metadata (e.g., titles). */
  def onPresentationBuilt(presentation: Presentation[F]): F[Unit]

  /** Called after the executor is created. Use for navigation subscriptions. */
  def onExecutorReady(executor: PresentationExecutor[F]): F[Unit]
}

private[lote] object Feature {

  /** Wraps a plain Overlay as a Feature with no-op lifecycle hooks. */
  def fromOverlay[F[_]: Monad](o: Overlay[F]): Feature[F] = new Feature[F] {
    val overlay: Overlay[F] = o
    def onSlideChange(index: Int): F[Unit] = Monad[F].unit
    def onPresentationBuilt(presentation: Presentation[F]): F[Unit] = Monad[F].unit
    def onExecutorReady(executor: PresentationExecutor[F]): F[Unit] = Monad[F].unit
  }
}

