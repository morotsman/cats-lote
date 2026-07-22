package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Character, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{AnimationClock, SmoothChar, TickedSlide}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

/** A scroll slide with a fixed marker at screen centre.
  *
  * This is the simpler version of `SimpleScrollSlide` — the background scrolls horizontally while a `>` marker stays
  * fixed at the centre of the viewport. It demonstrates `renderOntoScrolled` without any overlay animation, making it
  * easier to observe the sub-pixel grid scrolling in isolation.
  *
  * Press `a` to scroll left, `d` to scroll right, `s` to stop.
  *
  * ==Usage==
  * {{{
  * SessionBuilder[F]()
  *   .addSlideF {
  *     _.addSlideF(SimpleStaticMarkerScrollSlide.contextual[F]())
  *       .map(_.title("Static Marker Scroll"))
  *   }
  * }}}
  */
object SimpleStaticMarkerScrollSlide {

  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      create[F](builder.console, builder.ticker, builder.animationSettings)
    }

  // ── World ────────────────────────────────────────────────────────

  private val WorldWidth = 200

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

  // ── State ────────────────────────────────────────────────────────

  private case class ScrollState(
      cameraX: Double, // left edge of viewport in world coordinates
      speed: Double // cells per step
  )

  // ── Slide creation ───────────────────────────────────────────────

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): F[Slide[F]] =
    for {
      stateRef <- Ref[F].of(ScrollState(cameraX = 0.0, speed = 0.5))
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
              intCameraX = math.floor(st.cameraX).toInt
              fracOffset = st.cameraX - intCameraX
              bg = renderBackground(intCameraX, w, h)
              marker = staticMarker(w, h)
              _ <- glide.renderOntoScrolled(
                bg,
                marker,
                scrollX = -fracOffset,
                fixedRows = Set(h - 1)
              )
            } yield ()
          },
          onInput = {
            case Character(c) if c == 'a' => stateRef.update(_.copy(speed = -0.5))
            case Character(c) if c == 'd' => stateRef.update(_.copy(speed = 0.5))
            case Character(c) if c == 's' => stateRef.update(_.copy(speed = 0.0))
            case _                        => Monad[F].unit
          },
          onStart = stateRef.set(ScrollState(cameraX = 0.0, speed = 0.5))
        )
    } yield slide

  // ── Rendering ────────────────────────────────────────────────────

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

  /** Fixed marker at screen centre. */
  private def staticMarker(w: Int, h: Int): Vector[SmoothChar] = {
    val col = w / 2
    val row = h / 2
    Vector(SmoothChar('>', col, row, "#ffff00", key = 0))
  }
}
