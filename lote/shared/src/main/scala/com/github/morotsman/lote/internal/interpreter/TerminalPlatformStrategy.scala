package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import com.github.morotsman.lote.internal.algebra.PlatformStrategy

/** Terminal (JVM) platform strategy — all operations are no-ops.
  *
  * On character-grid terminals, slides are rendered in-place without spatial layout, layer management, or camera
  * navigation.
  */
private[lote] class TerminalPlatformStrategy[F[_]: Monad] extends PlatformStrategy[F] {
  override def setupPlatform(): F[Unit] = Monad[F].unit
  override def activateSlide(index: Int): F[Unit] = Monad[F].unit
  override def navigateToSlide(index: Int): F[Unit] = Monad[F].unit
}
