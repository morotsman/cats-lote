package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{Async, Resource, Sync}
import com.github.morotsman.lote.api.{PlatformCapability, RenderEffect, Screen, SlidePosition}
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

        // ---- Camera animator ----
        val cameraAnimator = new CameraAnimator(glScene)

        // ---- Frame state (shared with effect renderer for smoke particles) ----
        var previousFrame = Vector.empty[String]

        // ---- Spatial mode state ----
        var spatialMode = false
        var slideLayers: Vector[SlideLayer] = Vector.empty
        var slideToLayerIndex: Vector[Int] = Vector.empty  // slide index → layer index
        var activeLayerIndex: Int = -1

        // ---- Effect renderer ----
        val effectRenderer = new WebGLEffectRenderer(
          glScene,
          () => {
            if (spatialMode && activeLayerIndex >= 0 && activeLayerIndex < slideLayers.length)
              slideLayers(activeLayerIndex).offscreen
            else
              glScene.offscreen
          },
          () => {
            if (spatialMode && activeLayerIndex >= 0 && activeLayerIndex < slideLayers.length)
              slideLayers(activeLayerIndex).previousFrame
            else
              previousFrame
          },
          cameraAnimator,
          () => {
            if (spatialMode && activeLayerIndex >= 0 && activeLayerIndex < slideLayers.length)
              Some(slideLayers(activeLayerIndex).mesh.asInstanceOf[scalajs.js.Dynamic])
            else
              None
          }
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
              if (spatialMode && activeLayerIndex >= 0 && activeLayerIndex < slideLayers.length) {
                // Spatial mode: write to the active layer
                val layer = slideLayers(activeLayerIndex)
                val nextFrame = AnsiFrameRenderer.normalize(s, layer.cols, layer.rows)
                val maxRows = Math.max(layer.previousFrame.length, nextFrame.length)
                var dirty = false

                (0 until maxRows).foreach { rowIndex =>
                  val prev = layer.previousFrame.lift(rowIndex).getOrElse("")
                  val next = nextFrame.lift(rowIndex).getOrElse("")
                  if (prev != next) {
                    WebGLCanvasRenderer.renderLine(
                      layer.ctx, next, rowIndex, layer.cols,
                      CellWidth, CellHeight, FontFamily
                    )
                    dirty = true
                  }
                }

                layer.previousFrame = nextFrame

                if (dirty) {
                  layer.texture.needsUpdate = true
                  glScene.render()
                }
              } else if (spatialMode) {
                // Spatial mode but invalid layer index — ignore
              } else {
                // Normal mode: write to the single plane
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
            }

          override def flush(): F[Unit] = F.unit

          override def close(): F[Unit] =
            Sync[F].delay {
              glScene.renderer.domElement.removeEventListener("keydown", keyListener)
              cameraAnimator.cleanup()
              effectRenderer.cleanup()
              slideLayers.foreach(_.dispose())
              glScene.dispose()
            }

          override def capabilities: Set[PlatformCapability] = Set(
            PlatformCapability.CharacterGrid,
            PlatformCapability.SubPixelRendering,
            PlatformCapability.Effects,
            PlatformCapability.Transforms3D
          )

          override def applyEffect(effect: RenderEffect): F[Unit] =
            Sync[F].delay {
              effect match {
                case RenderEffect.InitSpatialLayout(positions) =>
                  initSpatialMode(positions)

                case RenderEffect.ActivateLayer(index) =>
                  val layerIdx = if (index >= 0 && index < slideToLayerIndex.length) slideToLayerIndex(index) else index
                  activeLayerIndex = layerIdx

                case RenderEffect.MoveCameraTo(target) =>
                  effectRenderer(effect)

                case _ =>
                  effectRenderer(effect)
              }
            }

          private def initSpatialMode(positions: Vector[Option[SlidePosition]]): Unit = {
            spatialMode = true

            // Hide the default single plane
            glScene.scene.remove(glScene.plane)

            // Resolve positions: slides without a position inherit from the previous one
            var lastPos = SlidePosition(0, 0, 0)
            val resolvedPositions: Vector[SlidePosition] = positions.map {
              case Some(pos) =>
                lastPos = pos
                pos
              case None =>
                lastPos
            }

            // Deduplicate: create one layer per unique position.
            // Build a mapping from slide index → layer index.
            val posToLayerIndex = scala.collection.mutable.LinkedHashMap.empty[(Double, Double, Double, Double, Double, Double), Int]
            val layerPositions = scala.collection.mutable.ArrayBuffer.empty[SlidePosition]
            val mapping = scala.collection.mutable.ArrayBuffer.empty[Int]

            resolvedPositions.foreach { pos =>
              val key = (pos.x, pos.y, pos.z, pos.rotX, pos.rotY, pos.rotZ)
              val layerIdx = posToLayerIndex.getOrElseUpdate(key, {
                val idx = layerPositions.length
                layerPositions += pos
                idx
              })
              mapping += layerIdx
            }

            slideToLayerIndex = mapping.toVector

            // Create a SlideLayer for each unique position
            slideLayers = layerPositions.zipWithIndex.map { case (pos, idx) =>
              val layer = new SlideLayer(
                index = idx,
                worldX = pos.x,
                worldY = pos.y,
                worldZ = pos.z,
                rotXDeg = pos.rotX,
                rotYDeg = pos.rotY,
                rotZDeg = pos.rotZ,
                viewportWidth = glScene.viewportWidth,
                viewportHeight = glScene.viewportHeight,
                cellWidth = CellWidth,
                cellHeight = CellHeight
              )
              glScene.scene.add(layer.mesh)
              layer
            }.toVector

            // Set up camera for the new scene bounds
            cameraAnimator.initSpatialMode(
              glScene.viewportWidth,
              glScene.viewportHeight
            )
          }
        }
      }
    } yield result
  }
}
