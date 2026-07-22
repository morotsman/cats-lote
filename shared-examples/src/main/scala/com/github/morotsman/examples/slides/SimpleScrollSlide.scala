package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Character, RenderEffect, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{AnimationClock, TickedSlide}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}
import scala.concurrent.duration.FiniteDuration

/** A minimal slide that demonstrates `GlideLayer.renderOntoScrolled` combined with a perfectly smooth elliptical orbit.
  *
  * A repeating pattern scrolls horizontally across the screen. A marker character (`>`) orbits the screen centre at
  * sub-pixel precision using `RenderEffect.RenderFloatingChars` directly.
  *
  * Press `a` to scroll left, `d` to scroll right, `s` to stop.
  *
  * ==Why not GlideLayer for the orbit?==
  *
  * GlideLayer is designed for integer cell positions with linear interpolation between discrete steps. This works well
  * for game elements that move cell-by-cell, but for a smooth elliptical orbit it introduces two artefacts:
  *
  *   - '''Staircase motion''' — col and row cross integer boundaries at different times, causing
  *     horizontal-then-vertical movement instead of diagonal.
  *   - '''Limited resolution''' — the path is quantized to integer cells regardless of the circle radius.
  *
  * By emitting `FloatingChar` with exact fractional `(cellX, cellY)` every frame, the WebGL overlay renders the marker
  * at true sub-pixel positions, tracing a mathematically perfect ellipse.
  *
  * ==Usage==
  * {{{
  * SessionBuilder[F]()
  *   .addSlideF {
  *     _.addSlideF(SimpleScrollSlide.contextual[F]())
  *       .map(_.title("Scroll Demo"))
  *   }
  * }}}
  */
object SimpleScrollSlide {

  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      create[F](builder.console, builder.ticker, builder.animationSettings)
    }

  // ── World and state ──────────────────────────────────────────────

  /** The world is a repeating pattern wider than any reasonable screen. */
  private val WorldWidth = 200

  /** Build the world line for a given row (repeating pattern). */
  private def worldLine(row: Int, screenHeight: Int): String = {
    val pattern = row match {
      case r if r == 0                => "~" * WorldWidth
      case r if r == screenHeight - 2 => "-" * WorldWidth
      case _ =>
        (0 until WorldWidth).map { x =>
          val wave = (Math.sin(x * 0.15 + row * 0.3) * 3).toInt
          if (wave == 0) '~'
          else if (wave > 0) '^'
          else '.'
        }.mkString
    }
    pattern
  }

  /** Scroll state for the logical text-grid camera. */
  private case class ScrollState(
      cameraX: Double, // left edge of viewport in world coordinates (fractional for sub-pixel smoothness)
      speed: Double, // cells per step (positive = scroll right, negative = scroll left, zero = stopped)
      startTime: FiniteDuration // wall-clock time when the slide started; used to compute the marker's orbit angle
  )

  /** Orbit angular velocity in radians per second. */
  private val OrbitRadPerSec = 0.8

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): F[Slide[F]] =
    for {
      now <- clock.monotonic
      stateRef <- Ref[F].of(ScrollState(cameraX = 0.0, speed = 0.5, startTime = now))
      slide <- TickedSlide[F](console, ticker, animationSettings)
        .withGlideLayer(wrapThreshold = 20)
        .buildWithGlide(
          onTick = { (nrOfSteps, glide) =>
            for {
              screen <- console.context
              w = screen.screenWidth
              h = screen.screenHeight
              st <- stateRef.updateAndGet { s =>
                val newCamX = (s.cameraX + s.speed * nrOfSteps + WorldWidth) % WorldWidth
                s.copy(cameraX = newCamX)
              }
              // Use wall-clock time for a perfectly smooth elliptical orbit.
              // No quantization needed — we emit exact fractional positions
              // directly via RenderFloatingChars every frame.
              now <- clock.monotonic
              elapsedSec = (now - st.startTime).toMillis / 1000.0
              intCameraX = math.floor(st.cameraX).toInt
              fracOffset = st.cameraX - intCameraX
              bg = renderBackground(intCameraX, w, h)
              // Render the scrolled background with an empty overlay
              // (we handle the marker separately via RenderFloatingChars).
              _ <- glide.renderOntoScrolled(
                bg,
                Vector.empty,
                scrollX = -fracOffset,
                fixedRows = Set(h - 1)
              )
              // Emit the marker at exact fractional coordinates for a
              // perfectly smooth elliptical path — no integer quantization.
              marker = orbitMarker(w, h, elapsedSec)
              _ <- console.applyEffect(RenderEffect.RenderFloatingChars(marker))
            } yield ()
          },
          onInput = {
            case Character(c) if c == 'a' => stateRef.update(_.copy(speed = -0.5))
            case Character(c) if c == 'd' => stateRef.update(_.copy(speed = 0.5))
            case Character(c) if c == 's' => stateRef.update(_.copy(speed = 0.0))
            case _                        => Monad[F].unit
          },
          onStart =
            clock.monotonic.flatMap(now => stateRef.set(ScrollState(cameraX = 0.0, speed = 0.5, startTime = now)))
        )
    } yield slide

  // ── Rendering ────────────────────────────────────────────────────

  /** Render the visible portion of the world at integer camera position. */
  private def renderBackground(intCamX: Int, w: Int, h: Int): ScreenAdjusted = {
    val lines = (0 until h).map { row =>
      if (row == h - 1) {
        val bar = " [A] left  [S] stop  [D] right "
        val pad = math.max(0, (w - bar.length) / 2)
        (" " * pad + bar).take(w).padTo(w, ' ')
      } else {
        val world = worldLine(row, h)
        (0 until w).map { col =>
          val worldX = ((intCamX + col) % WorldWidth + WorldWidth) % WorldWidth
          world.charAt(worldX)
        }.mkString
      }
    }
    ScreenAdjusted(lines.mkString("\n"))
  }

  /** Compute the marker's position as exact fractional cell coordinates.
    *
    * Unlike `SmoothChar` (which takes integer col/row and relies on GlideLayer for interpolation), `FloatingChar`
    * accepts Double coordinates that the WebGL renderer uses directly. This gives a mathematically perfect elliptical
    * path with zero staircase artefacts.
    */
  private def orbitMarker(width: Int, height: Int, elapsedSec: Double): Vector[RenderEffect.FloatingChar] = {
    val angle = elapsedSec * OrbitRadPerSec
    val radiusX = (width / 2.0) - 4.0
    val radiusY = (height / 2.0) - 6.0
    val cellX = width / 2.0 + radiusX * math.cos(angle)
    val cellY = height / 2.0 + radiusY * math.sin(angle)
    Vector(RenderEffect.FloatingChar('>', cellX, cellY, "#ffff00"))
  }
}
