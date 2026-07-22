package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{Async, Resource, Sync, Temporal}
import com.github.morotsman.lote.api.{PlatformCapability, RenderEffect, Screen, WebGLConfig}
import com.github.morotsman.lote.api.spi.{EffectfulTerminal, Terminal => TerminalAlgebra}
import org.scalajs.dom
import org.scalajs.dom.HTMLElement

import scala.concurrent.duration._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.Monad

/** Browser-backed `Terminal[F]` implementation using Three.js / WebGL.
  *
  * ==Overview==
  *
  * `ThreeJsTerminal` is the entry-point for the browser presentation engine. It implements both
  * [[com.github.morotsman.lote.api.spi.Terminal Terminal]] (low-level character I/O) and
  * [[com.github.morotsman.lote.api.spi.EffectfulTerminal EffectfulTerminal]] (GPU-accelerated visual effects), making
  * it the richest backend in the system.
  *
  * The terminal always operates in '''spatial mode''': each slide is a textured plane mesh positioned in 3-D
  * world-space and rendered through a perspective camera managed by [[CameraAnimator]]. Slide transitions are animated
  * camera movements between positions, giving the audience the impression of flying through a scene.
  *
  * ==Architecture==
  *
  * Internally the terminal is a composition of four single-purpose components:
  *
  *   - '''[[WebGLScene]]''' — owns the Three.js `Scene`, `WebGLRenderer`, and active camera reference. Handles browser
  *     resize events and exposes viewport dimensions.
  *   - '''[[WebGLCanvasRenderer]]''' — renders a single row of ANSI-styled text onto an offscreen `<canvas>` using the
  *     Canvas 2-D API. Each [[SlideLayer]] has its own offscreen canvas; the renderer is stateless and called per-line.
  *   - '''[[WebGLEffectRenderer]]''' — drives GPU-accelerated visual effects (dissolve, smoke, glow, fade) by
  *     manipulating Three.js meshes, particle systems, and shader uniforms.
  *   - '''[[WebGLInputHandler]]''' — translates DOM `keydown`, `mousedown`, and `paste` events into an `F`-effectful
  *     `Queue[F, Int]` of character codes (JLine-compatible encoding).
  *
  * ==Slide rendering pipeline==
  *
  * {{{
  *   write(ansiText)
  *     │
  *     ▼
  *   AnsiFrameRenderer.normalize   ← split ANSI string into rows
  *     │
  *     ▼
  *   Dirty-check against previousFrame
  *     │  (only changed rows continue)
  *     ▼
  *   WebGLCanvasRenderer.renderLine ← draw row onto offscreen canvas
  *     │
  *     ▼
  *   texture.needsUpdate = true     ← tell Three.js to re-upload
  *     │
  *     ▼
  *   glScene.render()               ← composite all meshes to screen
  * }}}
  *
  * ==Spatial layout & deduplication==
  *
  * When `RenderEffect.InitSpatialLayout(positions)` is applied the terminal creates one [[SlideLayer]] '''per unique
  * position'''. Multiple slides that share the same `(x, y, z, rotX, rotY, rotZ)` will map to the same layer (and
  * therefore the same offscreen canvas / mesh). The `slideToLayerIndex` vector records this many-to-one mapping so that
  * `ActivateLayer(slideIndex)` can look up the correct layer.
  *
  * ==Resource lifecycle==
  *
  * The terminal is acquired as a `cats.effect.Resource`. On release it:
  *   1. removes the DOM keyboard listener,
  *   2. cleans up the camera animator's animation frame and zoom listener,
  *   3. disposes all effect-renderer resources (particles, glow meshes),
  *   4. disposes every `SlideLayer` (textures, materials, geometries), and
  *   5. disposes the `WebGLScene` (renderer, camera, DOM element).
  *
  * ==External dependencies==
  *
  * Three.js must be loaded as a global (`window.THREE`) before any `ThreeJsTerminal` instance is created — typically
  * via a `<script>` tag pointing to the Three.js CDN.
  *
  * ==Thread-safety==
  *
  * All mutable state lives inside the `Sync[F].delay` block that constructs the terminal; mutations happen only within
  * `Sync[F].delay` calls, so the implementation is safe as long as the effect type `F` provides single-threaded
  * evaluation (as is the case on JS).
  */
object ThreeJsTerminal {

  /** Width of a single character cell in CSS pixels. */
  private val DefaultCellWidth = 10

  /** Height of a single character cell in CSS pixels. */
  private val DefaultCellHeight = 20

  /** CSS font-family stack used for monospace text rendering on the offscreen canvases. */
  private val FontFamily = "'Courier New', Courier, monospace"

  /** Creates a Three.js-backed `Terminal[F]` wrapped in a `Resource`.
    *
    * The returned `Resource` guarantees that all WebGL objects (renderer, textures, geometries, materials, particle
    * systems), DOM event listeners, and animation frames are properly cleaned up when the resource is released.
    *
    * ===Usage===
    * {{{
    * ThreeJsTerminal.resource[IO](containerDiv).use { terminal =>
    *   // terminal is ready — write, read, apply effects …
    * }
    * }}}
    *
    * @param container
    *   the DOM element that will host the WebGL `<canvas>`. The renderer's canvas is appended as a child of this
    *   element and sized to fill it.
    * @tparam F
    *   an effect type with an `Async` instance (typically `IO`).
    * @return
    *   a `Resource[F, Terminal[F]]` that yields the terminal on acquire and disposes all WebGL resources on release.
    */
  def resource[F[_]: Async](
      container: HTMLElement,
      config: WebGLConfig = WebGLConfig()
  ): Resource[F, TerminalAlgebra[F]] =
    for {
      dispatcher <- Dispatcher.sequential[F]
      terminal <- Resource.make(create[F](container, dispatcher, config))(_.close())
    } yield terminal

  /** Constructs the terminal instance and all its sub-components.
    *
    * This is called once during resource acquisition. The bulk of the work (scene creation, input wiring, building the
    * anonymous `Terminal` / `EffectfulTerminal` implementation) runs inside a single `Sync[F].delay` to guarantee
    * referential transparency.
    *
    * @param container
    *   the DOM element that will host the `<canvas>`
    * @param dispatcher
    *   a [[cats.effect.std.Dispatcher]] used by [[WebGLInputHandler]] to enqueue key events from DOM callbacks into the
    *   effectful `Queue[F, Int]`.
    * @return
    *   the fully initialised `Terminal[F]` (also `EffectfulTerminal[F]`).
    */
  private def create[F[_]: Async](
      container: HTMLElement,
      dispatcher: Dispatcher[F],
      config: WebGLConfig
  ): F[TerminalAlgebra[F]] = {
    val F = Async[F]
    val CellWidth = config.cellWidth
    val CellHeight = config.cellHeight
    for {
      queue <- Queue.unbounded[F, Int]
      result <- Sync[F].delay {

        // ---- Scene setup ----
        val glScene = new WebGLScene(container, CellWidth, CellHeight, config)

        // ---- Camera animator ----
        val cameraAnimator = new CameraAnimator(glScene, config)

        // ---- Spatial state ----
        // Manages slide layers, the slide-to-layer mapping, the active
        // layer index, and the shared Scene3DRef.  All mutable state
        // that was previously captured by closures as bare vars now
        // lives in this single, testable object.
        val spatialState = new SpatialState(glScene, cameraAnimator, CellWidth, CellHeight)

        // ---- Effect renderer ----
        val effectRenderer = new WebGLEffectRenderer(
          glScene,
          () => spatialState.activeOffscreen,
          () => spatialState.activeFrame,
          cameraAnimator,
          () => spatialState.activeLayerMesh
        )

        // ---- Input handling ----
        val keyListener = WebGLInputHandler.attach(
          glScene.renderer.domElement,
          queue,
          dispatcher,
          CellWidth,
          CellHeight
        )

        // ---- Floating-chars overlay ----
        // A transparent canvas + mesh layered just in front of the active slide,
        // used for rendering characters at sub-pixel positions (e.g. smooth worm motion).
        var floatingOverlay
            : Option[(dom.HTMLCanvasElement, dom.CanvasRenderingContext2D, ThreeCanvasTexture, ThreeMesh)] = None

        def disposeFloatingOverlay(): Unit = {
          floatingOverlay.foreach { case (_, _, tex, mesh) =>
            glScene.scene.remove(mesh)
            tex.dispose()
            mesh.geometry.dispose()
            mesh.material.dispose()
          }
          floatingOverlay = None
        }

        def ensureFloatingOverlay(
            layer: SlideLayer
        ): (dom.HTMLCanvasElement, dom.CanvasRenderingContext2D, ThreeCanvasTexture, ThreeMesh) =
          floatingOverlay.getOrElse {
            val dpr = dom.window.devicePixelRatio.max(1.0)
            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
            canvas.width = (layer.cols * CellWidth * dpr).toInt
            canvas.height = (layer.rows * CellHeight * dpr).toInt
            val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
            ctx.scale(dpr, dpr)

            val tex = new ThreeCanvasTexture(canvas)
            tex.minFilter = ThreeLinearFilter
            tex.magFilter = ThreeLinearFilter

            val geo = new ThreePlaneGeometry(glScene.viewportWidth, glScene.viewportHeight)
            val mat = new ThreeMeshBasicMaterial(
              ThreeMaterialOptions(map = tex, transparent = true, side = 2)
            )
            mat.depthWrite = false
            val mesh = new ThreeMesh(geo, mat)

            // Position just in front of the slide mesh
            val pos = layer.mesh.position
            mesh.position.set(pos.x, pos.y, pos.z + 0.1)
            val rot = layer.mesh.rotation
            mesh.rotation.set(rot.x, rot.y, rot.z)

            glScene.scene.add(mesh)
            val result = (canvas, ctx, tex, mesh)
            floatingOverlay = Some(result)
            result
          }

        // ---- Build Terminal ----
        new TerminalAlgebra[F] with EffectfulTerminal[F] {

          /** Reads a single character code from the input queue.
            *
            * Blocks (semantically, via `F`) until a key event is available, or until `timeoutInMillis` elapses — in
            * which case the sentinel value `65534` is returned to signal "no input".
            *
            * @param timeoutInMillis
            *   maximum time to wait, in milliseconds. A value ≤ 0 means "wait forever".
            * @return
            *   the character code, or `65534` on timeout.
            */
          override def read(timeoutInMillis: Long): F[Int] =
            if (timeoutInMillis <= 0) queue.take
            else F.timeoutTo(queue.take, timeoutInMillis.millis, F.pure(65534))

          /** Returns the current terminal dimensions in character cells.
            *
            * The values are derived from the WebGL viewport size divided by `CellWidth` / `CellHeight`.
            */
          override def size: F[Screen] =
            Sync[F].delay(Screen(screenWidth = glScene.cols, screenHeight = glScene.rows))

          /** Writes ANSI-styled text to the currently active slide layer.
            *
            * The string is first normalised into a grid of rows via [[AnsiFrameRenderer.normalize]]. Each row is then
            * compared to the corresponding row in the layer's `previousFrame`; only rows that have actually changed are
            * re-rendered onto the offscreen canvas (via [[WebGLCanvasRenderer.renderLine]]).
            *
            * After all dirty rows are drawn the Three.js texture is flagged for re-upload (`needsUpdate = true`) and a
            * scene render is triggered.
            *
            * If no layer is active (e.g. before `InitSpatialLayout` has been applied), the write is silently dropped.
            *
            * @param s
            *   the ANSI-encoded string to render.
            */
          override def write(s: String): F[Unit] =
            Sync[F].delay {
              spatialState.activeLayer.foreach { layer =>
                val nextFrame = AnsiFrameRenderer.normalize(s, layer.cols, layer.rows)
                val maxRows = Math.max(layer.previousFrame.length, nextFrame.length)
                var dirty = false
                val hasOffset = layer.canvasOffsetX != 0.0 || layer.canvasOffsetY != 0.0

                // When a canvas offset is active, apply it as a translate and re-render all rows
                // (the offset shifts everything, so dirty-checking individual rows is insufficient).
                if (hasOffset) {
                  val ctx = layer.ctx
                  // Clear the entire canvas first (the translate shifts content, leaving edges exposed)
                  ctx.save()
                  ctx.setTransform(
                    layer.offscreen.width / (layer.cols * CellWidth).toDouble,
                    0,
                    0,
                    layer.offscreen.height / (layer.rows * CellHeight).toDouble,
                    0,
                    0
                  )
                  if (layer.transparentBg) {
                    ctx.clearRect(0, 0, layer.cols * CellWidth, layer.rows * CellHeight)
                  } else {
                    ctx.fillStyle = "#000000"
                    ctx.fillRect(0, 0, layer.cols * CellWidth, layer.rows * CellHeight)
                  }
                  ctx.restore()

                  val dpr = layer.offscreen.width.toDouble / (layer.cols * CellWidth)

                  // Pass 1: render all rows WITH the canvas offset (scrollable content)
                  ctx.save()
                  ctx.setTransform(
                    dpr,
                    0,
                    0,
                    dpr,
                    layer.canvasOffsetX * CellWidth * dpr,
                    layer.canvasOffsetY * CellHeight * dpr
                  )

                  (0 until maxRows).foreach { rowIndex =>
                    val next = nextFrame.lift(rowIndex).getOrElse("")
                    WebGLCanvasRenderer.renderLine(
                      ctx,
                      next,
                      rowIndex,
                      layer.cols,
                      CellWidth,
                      CellHeight,
                      FontFamily,
                      transparentBg = layer.transparentBg
                    )
                  }
                  ctx.restore()

                  // Pass 2: re-render fixed rows WITHOUT offset so overlays (timer, progress bar)
                  // stay in place while the scene content scrolls.
                  if (layer.fixedRows.nonEmpty) {
                    ctx.save()
                    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
                    layer.fixedRows.foreach { rowIndex =>
                      if (rowIndex >= 0 && rowIndex < maxRows) {
                        // Clear the row area first (remove the shifted content)
                        val y = rowIndex * CellHeight
                        if (layer.transparentBg) {
                          ctx.clearRect(0, y, layer.cols * CellWidth, CellHeight)
                        } else {
                          ctx.fillStyle = "#000000"
                          ctx.fillRect(0, y, layer.cols * CellWidth, CellHeight)
                        }
                        val next = nextFrame.lift(rowIndex).getOrElse("")
                        WebGLCanvasRenderer.renderLine(
                          ctx,
                          next,
                          rowIndex,
                          layer.cols,
                          CellWidth,
                          CellHeight,
                          FontFamily,
                          transparentBg = layer.transparentBg
                        )
                      }
                    }
                    ctx.restore()
                  }

                  dirty = true
                } else {
                  (0 until maxRows).foreach { rowIndex =>
                    val prev = layer.previousFrame.lift(rowIndex).getOrElse("")
                    val next = nextFrame.lift(rowIndex).getOrElse("")
                    if (prev != next) {
                      WebGLCanvasRenderer.renderLine(
                        layer.ctx,
                        next,
                        rowIndex,
                        layer.cols,
                        CellWidth,
                        CellHeight,
                        FontFamily,
                        transparentBg = layer.transparentBg
                      )
                      dirty = true
                    }
                  }
                }

                layer.previousFrame = nextFrame

                if (dirty) {
                  layer.texture.needsUpdate = true
                  glScene.render()
                }
              }
            }

          /** No-op — WebGL rendering is immediate; there is no output buffer to flush.
            */
          override def flush(): F[Unit] = F.unit

          /** Releases all resources held by this terminal.
            *
            * Removes the DOM keyboard listener, cleans up the camera animator (cancels animation frames, removes zoom
            * listener), disposes effect-renderer resources, disposes every [[SlideLayer]] via [[SpatialState]], and
            * finally tears down the [[WebGLScene]] (renderer, cameras, DOM canvas).
            */
          override def close(): F[Unit] =
            Sync[F].delay {
              disposeFloatingOverlay()
              glScene.renderer.domElement.removeEventListener("keydown", keyListener)
              cameraAnimator.cleanup()
              effectRenderer.cleanup()
              spatialState.dispose()
              glScene.dispose()
            }

          /** Advertises the full set of rendering capabilities.
            *
            *   - `CharacterGrid` — can render a grid of styled text
            *   - `SubPixelRendering` — canvas anti-aliasing gives sub-pixel quality
            *   - `Effects` — supports dissolve / smoke / glow / fade
            *   - `Transforms3D` — slides live in 3-D space with camera animation
            *
            * Transition implementations inspect these capabilities to choose between simple text redraws and richer
            * animations.
            */
          override def capabilities: Set[PlatformCapability] = Set(
            PlatformCapability.CharacterGrid,
            PlatformCapability.SubPixelRendering,
            PlatformCapability.Effects,
            PlatformCapability.Transforms3D
          )

          /** Returns the shared [[com.github.morotsman.lote.api.Scene3DRef]] for scene-aware slides.
            *
            * `None` until `InitSpatialLayout` has been applied. Once available, slides can use the ref to add custom
            * Three.js objects (meshes, lights, particle systems) to the shared scene and have them rendered alongside
            * the slide planes.
            */
          override def scene3DRef: Option[Any] = spatialState.sharedSceneRef

          /** Applies a [[RenderEffect]] to this terminal.
            *
            * Effect dispatch:
            *
            *   - '''`InitSpatialLayout`''' — delegates to [[SpatialState.init]] which creates the [[SlideLayer]]s, sets
            *     up the camera, and builds the [[com.github.morotsman.lote.api.Scene3DRef]].
            *   - '''`ActivateLayer(index)`''' — delegates to [[SpatialState.activateLayer]] to switch the active layer.
            *   - '''`MoveCameraTo(target)`''' — starts an animated camera transition to `target` and suspends the fibre
            *     until the animation completes (~16 ms poll loop).
            *   - '''`JumpCameraTo(target)`''' — instantly repositions the camera with no animation.
            *   - All other effects (dissolve, smoke, glow, fade, clear, …) are delegated to [[WebGLEffectRenderer]].
            */
          override def applyEffect(effect: RenderEffect): F[Unit] =
            effect match {
              case RenderEffect.InitSpatialLayout(positions) =>
                Sync[F].delay { spatialState.init(positions) }

              case RenderEffect.ActivateLayer(index) =>
                Sync[F].delay {
                  disposeFloatingOverlay()
                  spatialState.activateLayer(index)
                }

              case RenderEffect.MoveCameraTo(target) =>
                Sync[F].delay { cameraAnimator.moveTo(target) } >>
                  Monad[F].tailRecM(()) { _ =>
                    Temporal[F].sleep(16.millis) >>
                      Sync[F].delay(cameraAnimator.isAnimating).map {
                        case true  => Left(())
                        case false => Right(())
                      }
                  }

              case RenderEffect.JumpCameraTo(target) =>
                Sync[F].delay { cameraAnimator.jumpTo(target) }

              case RenderEffect.RenderFloatingChars(chars) =>
                Sync[F].delay {
                  spatialState.activeLayer.foreach { layer =>
                    val (_, ctx, tex, _) = ensureFloatingOverlay(layer)
                    val w = layer.cols * CellWidth
                    val h = layer.rows * CellHeight

                    // Clear the overlay
                    ctx.clearRect(0, 0, w, h)

                    // Draw each character at its sub-pixel position
                    val fontFamily = FontFamily
                    ctx.textBaseline = "top"
                    chars.foreach { fc =>
                      ctx.fillStyle = fc.fgColor
                      ctx.font = s"${CellHeight - 4}px $fontFamily"
                      val x = fc.cellX * CellWidth
                      val y = fc.cellY * CellHeight
                      ctx.fillText(fc.char.toString, x, y + 2)
                    }

                    tex.needsUpdate = true
                    glScene.render()
                  }
                }

              case RenderEffect.ClearFloatingChars =>
                Sync[F].delay { disposeFloatingOverlay() }

              case RenderEffect.SetCanvasOffset(cellsX, cellsY) =>
                Sync[F].delay {
                  spatialState.activeLayer.foreach { layer =>
                    layer.canvasOffsetX = cellsX
                    layer.canvasOffsetY = cellsY
                  }
                }

              case RenderEffect.SetFixedRows(rows) =>
                Sync[F].delay {
                  spatialState.activeLayer.foreach { layer =>
                    layer.fixedRows = layer.fixedRows ++ rows
                  }
                }

              case RenderEffect.ClearFixedRows =>
                Sync[F].delay {
                  spatialState.activeLayer.foreach { layer =>
                    layer.fixedRows = Set.empty
                  }
                }

              case _ =>
                Sync[F].delay { effectRenderer(effect) }
            }
        }
      }
    } yield result
  }
}
