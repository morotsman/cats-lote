package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{Async, Resource, Sync}
import com.github.morotsman.lote.api.{PlatformCapability, RenderEffect, Screen}
import com.github.morotsman.lote.api.spi.{EffectfulTerminal, Terminal => TerminalAlgebra}
import org.scalajs.dom.HTMLElement

import scala.concurrent.duration._
import cats.syntax.flatMap._
import cats.syntax.functor._

/** Browser-backed `Terminal[F]` implementation using Three.js / WebGL.
  *
  * Orchestrates four focused components:
  *
  *  - [[WebGLScene]] — Three.js scene, camera, renderer, and textured plane
  *  - [[WebGLCanvasRenderer]] — ANSI-styled text → offscreen Canvas2D
  *  - [[WebGLEffectRenderer]] — GPU-accelerated visual effects (flip, rotate, smoke, dissolve, …)
  *  - [[WebGLInputHandler]] — DOM keyboard/mouse events → character queue
  *
  * Three.js is expected to be available as `window.THREE` (loaded via CDN).
  */
object ThreeJsTerminal {

  private val CellWidth  = 10
  private val CellHeight = 20
  private val FontFamily = "'Courier New', Courier, monospace"

  /** Creates a Three.js-backed `Terminal[F]` as a `Resource` that disposes all WebGL resources on release. */
  def resource[F[_]: Async](container: HTMLElement): Resource[F, TerminalAlgebra[F]] =
    for {
      dispatcher <- Dispatcher.sequential[F]
      terminal   <- Resource.make(create[F](container, dispatcher))(_.close())
    } yield terminal

  private def create[F[_]: Async](
      container: HTMLElement,
      dispatcher: Dispatcher[F]
  ): F[TerminalAlgebra[F]] = {
    val F = Async[F]
    for {
      queue <- Queue.unbounded[F, Int]
      result <- Sync[F].delay {

        // ---- Scene setup ----
        val glScene = new WebGLScene(container, CellWidth, CellHeight)

        // ---- Frame state (shared with effect renderer for smoke particles) ----
        var previousFrame = Vector.empty[String]

        // ---- Effect renderer ----
        val effectRenderer = new WebGLEffectRenderer(
          glScene,
          glScene.offscreen,
          () => previousFrame
        )

        // ---- Input handling ----
        val keyListener = WebGLInputHandler.attach(
          glScene.renderer.domElement,
          queue,
          dispatcher,
          CellWidth,
          CellHeight
        )

        // ---- Build Terminal ----
        new TerminalAlgebra[F] with EffectfulTerminal[F] {

          override def read(timeoutInMillis: Long): F[Int] =
            if (timeoutInMillis <= 0) queue.take
            else F.timeoutTo(queue.take, timeoutInMillis.millis, F.pure(65534))

          override def size: F[Screen] =
            Sync[F].delay(Screen(screenWidth = glScene.cols, screenHeight = glScene.rows))

          override def write(s: String): F[Unit] =
            Sync[F].delay {
              val nextFrame = AnsiFrameRenderer.normalize(s, glScene.cols, glScene.rows)
              val maxRows = Math.max(previousFrame.length, nextFrame.length)
              var dirty = false

              (0 until maxRows).foreach { rowIndex =>
                val prev = previousFrame.lift(rowIndex).getOrElse("")
                val next = nextFrame.lift(rowIndex).getOrElse("")
                if (prev != next) {
                  WebGLCanvasRenderer.renderLine(
                    glScene.ctx, next, rowIndex, glScene.cols,
                    CellWidth, CellHeight, FontFamily
                  )
                  dirty = true
                }
              }

              previousFrame = nextFrame

              if (dirty) {
                glScene.texture.needsUpdate = true
                glScene.render()
              }
            }

          override def flush(): F[Unit] = F.unit

          override def close(): F[Unit] =
            Sync[F].delay {
              glScene.renderer.domElement.removeEventListener("keydown", keyListener)
              effectRenderer.cleanup()
              glScene.dispose()
            }

          override def capabilities: Set[PlatformCapability] = Set(
            PlatformCapability.CharacterGrid,
            PlatformCapability.SubPixelRendering,
            PlatformCapability.Effects,
            PlatformCapability.Transforms3D
          )

          override def applyEffect(effect: RenderEffect): F[Unit] =
            Sync[F].delay(effectRenderer(effect))
        }
      }
    } yield result
  }
}
