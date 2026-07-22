package com.github.morotsman.lote.internal.algebra

/** Platform-specific rendering strategy for the presentation executor.
  *
  * Implementations handle platform-specific concerns such as spatial layout, layer management, and camera navigation.
  * The executor delegates to this trait instead of directly calling `applyEffect` for platform-specific operations.
  *
  *   - On terminal (JVM): all operations are no-ops.
  *   - On WebGL (JS): operations manage 3D layers, camera movements, and pre-rendering.
  */
private[lote] trait PlatformStrategy[F[_]] {

  /** One-time platform setup at presentation start.
    *
    * On WebGL this initializes the spatial layout and pre-renders the first slide per position. On terminal this is a
    * no-op.
    */
  def setupPlatform(): F[Unit]

  /** Activate the rendering target for a slide.
    *
    * On WebGL this selects the slide's layer so that subsequent `writeString` calls render to it. On terminal this is a
    * no-op.
    *
    * @param index
    *   the slide index (0-based)
    */
  def activateSlide(index: Int): F[Unit]

  /** Animate navigation to a slide's position.
    *
    * On WebGL this performs an interruptible camera flight to the slide's 3D position. On terminal this is a no-op.
    *
    * @param index
    *   the slide index (0-based)
    */
  def navigateToSlide(index: Int): F[Unit]
}
