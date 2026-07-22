package com.github.morotsman.lote.api.support

import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.all._
import com.github.morotsman.lote.api.{AnimationSettings, PlatformCapability, RenderEffect, ScreenAdjusted}
import com.github.morotsman.lote.api.spi.NConsole

import scala.concurrent.duration.FiniteDuration

/** A character to be rendered on the smooth overlay.
  *
  * The user specifies integer cell positions. On WebGL backends, the library automatically interpolates between the
  * previous and current positions at sub-pixel precision, creating smooth motion at the display's native refresh rate.
  * On terminal backends, characters are rendered at their integer cell positions on the character grid.
  *
  * @param char
  *   the character to draw
  * @param col
  *   column position (0-based, integer cell)
  * @param row
  *   row position (0-based, integer cell)
  * @param fgColor
  *   CSS foreground color (default white)
  * @param key
  *   stable identity for this character across frames. When characters are added or removed between frames, setting a
  *   unique key ensures interpolation pairs the correct previous and current positions. When set to -1 (default),
  *   index-based matching is used instead.
  */
case class SmoothChar(char: Char, col: Int, row: Int, fgColor: String = "#ffffff", key: Int = -1)

/** Renders characters with automatic smooth interpolation on WebGL backends.
  *
  * `GlideLayer` eliminates the boilerplate of tracking previous positions, computing interpolation progress, detecting
  * wrap-around, and dispatching `RenderEffect.RenderFloatingChars` vs. grid rendering. The user simply provides integer
  * cell positions on each tick; the library handles the rest.
  *
  * ==Usage==
  * {{{
  * for {
  *   overlay <- GlideLayer.make[F](console)
  *   // In your tick callback:
  *   _       <- overlay.render(wormSegments)
  *   // When done:
  *   _       <- overlay.clear()
  * } yield ()
  * }}}
  *
  * ==How it works==
  *
  *   - On the first call to `render`, characters appear at their given positions (no interpolation).
  *   - On subsequent calls, the overlay detects position changes automatically and interpolates from the previous
  *     positions to the current positions.
  *   - Interpolation progress is computed internally from the elapsed time since the last position change and the
  *     configured `step` duration.
  *   - If a character jumps more than `wrapThreshold` cells in either axis (e.g. screen-edge wrap-around), it snaps to
  *     the current position instead of sliding across the screen.
  *   - On WebGL backends, characters are drawn at sub-pixel positions on a transparent overlay above the slide.
  *   - On terminal backends, `render` is a no-op (characters are rendered at integer positions on the character grid by
  *     the caller).
  */
object GlideLayer {

  private case class OverlayState(
      // The positions at the START of the current interpolation cycle (i.e., where segments were
      // just before the last position change). Interpolation goes from `previous` → current.
      // Keyed by SmoothChar.key for stable matching when elements are added/removed.
      // Falls back to index-based matching when keys are not set (all -1).
      previous: Map[Int, SmoothChar],
      previousVec: Vector[SmoothChar],
      // The positions set by the most recent stepped render call. When positions change again,
      // these become the new `previous`.
      lastStepped: Map[Int, SmoothChar],
      lastSteppedVec: Vector[SmoothChar],
      // When the last position change was detected (for computing interpolation progress).
      lastStepTime: FiniteDuration
  )

  trait Overlay[F[_]] {

    /** Render characters with smooth interpolation.
      *
      * Call this every tick with the current character positions. The overlay automatically detects when positions
      * change and interpolates between previous and current positions on WebGL backends. On terminal backends, this is
      * a no-op.
      *
      * @param chars
      *   the current character positions (integer cells)
      */
    def render(chars: Vector[SmoothChar]): F[Unit]

    /** Render characters onto a frame, abstracting away platform differences.
      *
      * On WebGL backends: dispatches `chars` to the floating overlay (with smooth interpolation) and returns the
      * `frame` unchanged — the caller's frame serves as the background beneath the overlay.
      *
      * On terminal backends: composites `chars` at their integer cell positions directly onto the `frame` content and
      * returns the modified frame.
      *
      * This eliminates the need for transition authors to manually branch on platform capabilities.
      *
      * @param frame
      *   the background frame (e.g., the grid content without the overlay characters)
      * @param chars
      *   the characters to render on the overlay / composite onto the frame
      * @return
      *   the frame to pass to `console.writeString` — unchanged on WebGL, composited on terminal
      */
    def renderOnto(frame: ScreenAdjusted, chars: Vector[SmoothChar]): F[ScreenAdjusted]

    /** Render characters onto a frame with smooth sub-pixel grid scrolling.
      *
      * This is a higher-level convenience that combines `SetCanvasOffset` (for smooth background grid scrolling) with
      * overlay rendering (for smooth character interpolation) in a single call, abstracting away all
      * platform-specific branching.
      *
      * '''Note:''' Unlike [[renderOnto]], which returns an `F[ScreenAdjusted]` for the caller to write,
      * this method writes the frame to the console internally and returns `F[Unit]`. This is because the
      * scrolled rendering path requires precise ordering of console effects (offset → write → reset → overlay)
      * that would be error-prone to leave to the caller.
      *
      * On WebGL backends:
      *   - Optionally clears / sets fixed rows (rows that should not scroll with the grid).
      *   - Applies `SetCanvasOffset(scrollX, scrollY)` to shift the character grid by a fractional pixel.
      *   - Writes the `frame` to the console.
      *   - Resets `SetCanvasOffset(0, 0)` so overlay characters render at true screen positions.
      *   - Dispatches `chars` to the floating overlay with smooth interpolation.
      *
      * On terminal backends:
      *   - Composites `chars` at integer positions onto `frame`.
      *   - Writes the composited frame to the console.
      *   - Sub-pixel scrolling is ignored (no effect on terminal).
      *
      * @param frame
      *   the background frame rendered at integer grid positions
      * @param chars
      *   the characters to render on the overlay / composite onto the frame
      * @param scrollX
      *   horizontal sub-pixel offset for the grid (e.g., `-fracOffset` for smooth horizontal scrolling)
      * @param scrollY
      *   vertical sub-pixel offset for the grid
      * @param fixedRows
      *   row indices that should remain fixed (not affected by the grid scroll), e.g., an info bar row.
      *   Pass an empty set to skip fixed-row management entirely.
      */
    def renderOntoScrolled(
        frame: ScreenAdjusted,
        chars: Vector[SmoothChar],
        scrollX: Double = 0.0,
        scrollY: Double = 0.0,
        fixedRows: Set[Int] = Set.empty
    ): F[Unit]

    /** Remove the overlay and clean up resources. Call this when the slide stops. */
    def clear(): F[Unit]
  }

  /** Create a new `Overlay[F]`.
    *
    * @param console
    *   the console to render to
    * @param step
    *   the simulation step duration, used to compute interpolation progress internally. Default is
    *   `AnimationSettings.DefaultStep` (40ms).
    * @param wrapThreshold
    *   maximum cell distance in any axis before snapping instead of interpolating. Default is 1, which is appropriate
    *   for game elements that wrap around screen edges. Set to a larger value (e.g., `columnsPerStep` or screen width)
    *   for transitions where elements legitimately move multiple cells per step.
    */
  def make[F[_]: Monad: Ref.Make](
      console: NConsole[F],
      step: FiniteDuration = AnimationSettings.DefaultStep,
      wrapThreshold: Int = 1
  )(implicit clock: AnimationClock[F]): F[Overlay[F]] =
    for {
      now <- clock.monotonic
      stateRef <- Ref[F].of(
        OverlayState(
          previous = Map.empty,
          previousVec = Vector.empty,
          lastStepped = Map.empty,
          lastSteppedVec = Vector.empty,
          lastStepTime = now
        )
      )
    } yield {
      val supportsFloating = console.capabilities.contains(PlatformCapability.Effects)
      val stepNanos = Math.max(1L, step.toNanos)

      new Overlay[F] {

        private def toKeyMap(chars: Vector[SmoothChar]): Map[Int, SmoothChar] =
          chars.iterator.filter(_.key >= 0).map(c => c.key -> c).toMap

        private def interpolate(prev: SmoothChar, curr: SmoothChar, progress: Double): RenderEffect.FloatingChar = {
          val dx = Math.abs(curr.col - prev.col)
          val dy = Math.abs(curr.row - prev.row)
          if (dx > wrapThreshold || dy > wrapThreshold) {
            // Wrap-around: snap to current position
            RenderEffect.FloatingChar(curr.char, curr.col.toDouble, curr.row.toDouble, curr.fgColor)
          } else {
            val x = prev.col + (curr.col - prev.col) * progress
            val y = prev.row + (curr.row - prev.row) * progress
            RenderEffect.FloatingChar(curr.char, x, y, curr.fgColor)
          }
        }

        /** Detect whether positions have changed by comparing char/col/row fields. */
        private def positionsChanged(newChars: Vector[SmoothChar], oldChars: Vector[SmoothChar]): Boolean = {
          if (newChars.length != oldChars.length) return true
          var i = 0
          while (i < newChars.length) {
            val n = newChars(i)
            val o = oldChars(i)
            if (n.col != o.col || n.row != o.row || n.char != o.char) return true
            i += 1
          }
          false
        }

        override def render(chars: Vector[SmoothChar]): F[Unit] =
          if (!supportsFloating) {
            Monad[F].unit
          } else {
            for {
              now <- clock.monotonic
              state <- stateRef.get
              // Detect whether this is a new step by comparing positions
              stepped = positionsChanged(chars, state.lastSteppedVec)
              // Compute interpolation progress from elapsed time
              elapsed = (now - state.lastStepTime).toNanos
              progress = if (stepped) 0.0 else Math.min(1.0, elapsed.toDouble / stepNanos.toDouble)
              // When a step fires, the old "lastStepped" positions become the new "previous"
              // (the interpolation start point), and the new chars become "lastStepped".
              // Between steps, previous and lastStepped stay unchanged.
              prevMap = if (stepped) state.lastStepped else state.previous
              prevVec = if (stepped) state.lastSteppedVec else state.previousVec
              charsMap = toKeyMap(chars)
              _ <-
                if (stepped)
                  stateRef.set(
                    OverlayState(
                      previous = state.lastStepped,
                      previousVec = state.lastSteppedVec,
                      lastStepped = charsMap,
                      lastSteppedVec = chars,
                      lastStepTime = now
                    )
                  )
                else Monad[F].unit
              floatingChars = chars.zipWithIndex.map { case (curr, i) =>
                // Use key-based lookup when keys are set, fall back to index-based
                val prevChar =
                  if (curr.key >= 0) prevMap.getOrElse(curr.key, curr)
                  else prevVec.lift(i).getOrElse(curr)
                interpolate(prevChar, curr, progress)
              }
              _ <- console.applyEffect(RenderEffect.RenderFloatingChars(floatingChars))
            } yield ()
          }

        override def renderOnto(frame: ScreenAdjusted, chars: Vector[SmoothChar]): F[ScreenAdjusted] =
          if (supportsFloating) {
            // WebGL: dispatch chars to the floating overlay, return frame unchanged
            render(chars).as(frame)
          } else {
            // Terminal: composite chars at integer positions onto the frame
            Monad[F].pure(compositeOntoFrame(frame, chars))
          }

        private def compositeOntoFrame(frame: ScreenAdjusted, chars: Vector[SmoothChar]): ScreenAdjusted = {
          if (chars.isEmpty) return frame
          val lines = frame.content.split("\n", -1)
          val mutableLines = lines.map(_.toArray)
          chars.foreach { sc =>
            if (sc.row >= 0 && sc.row < mutableLines.length) {
              val line = mutableLines(sc.row)
              if (sc.col >= 0 && sc.col < line.length) {
                line(sc.col) = sc.char
              }
            }
          }
          ScreenAdjusted(mutableLines.map(new String(_)).mkString("\n"))
        }

        override def renderOntoScrolled(
            frame: ScreenAdjusted,
            chars: Vector[SmoothChar],
            scrollX: Double,
            scrollY: Double,
            fixedRows: Set[Int]
        ): F[Unit] =
          if (supportsFloating) {
            val hasScroll = scrollX != 0.0 || scrollY != 0.0
            val setupFixedRows =
              if (fixedRows.nonEmpty)
                console.applyEffect(RenderEffect.ClearFixedRows) *>
                  console.applyEffect(RenderEffect.SetFixedRows(fixedRows))
              else Monad[F].unit
            val applyOffset =
              if (hasScroll) console.applyEffect(RenderEffect.SetCanvasOffset(scrollX, scrollY))
              else Monad[F].unit
            val resetOffset =
              if (hasScroll) console.applyEffect(RenderEffect.SetCanvasOffset(0.0, 0.0))
              else Monad[F].unit
            setupFixedRows *> applyOffset *> console.writeString(frame) *> resetOffset *> render(chars)
          } else {
            val composited = compositeOntoFrame(frame, chars)
            console.writeString(composited)
          }

        override def clear(): F[Unit] =
          clock.monotonic.flatMap { now =>
            stateRef.set(
              OverlayState(
                previous = Map.empty,
                previousVec = Vector.empty,
                lastStepped = Map.empty,
                lastSteppedVec = Vector.empty,
                lastStepTime = now
              )
            )
          } *> (if (supportsFloating) console.applyEffect(RenderEffect.ClearFloatingChars) else Monad[F].unit)
      }
    }
}
