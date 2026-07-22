package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.WebGLConfig
import org.scalajs.dom
import org.scalajs.dom.HTMLElement

/** Manages the Three.js scene, renderer, and shared state for the WebGL terminal.
  *
  * Provides the scene graph and renderer that spatial-mode components (SlideLayer, CameraAnimator) attach to. The
  * perspective camera is set up by CameraAnimator when spatial mode initializes.
  */
private[nconsole] class WebGLScene(
    container: HTMLElement,
    val cellWidth: Int,
    val cellHeight: Int,
    config: WebGLConfig = WebGLConfig()
) {

  private val containerWidth: Int = container.clientWidth.max(100)
  private val containerHeight: Int = container.clientHeight.max(100)

  // Performance: cols/rows are mutable vars updated by the resize listener below. They are read
  // synchronously inside Sync[F].delay by ThreeJsTerminal.size. On single-threaded JS this is
  // safe and avoids the overhead of wrapping two ints in a Ref.
  var cols: Int = containerWidth / cellWidth
  var rows: Int = containerHeight / cellHeight

  // ---- Three.js scene ----
  ThreeJsFacade.checkVersion()
  val scene = new ThreeScene()
  scene.background = new ThreeColor(config.backgroundColor)

  /** The camera currently used for rendering. Set by `CameraAnimator.initSpatialMode`. */
  var activeCamera: scalajs.js.Any = _

  private val rendererOpts = ThreeRendererOptions(antialias = config.antialias, alpha = true)
  val renderer = new ThreeWebGLRenderer(rendererOpts.asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]])
  renderer.setSize(containerWidth, containerHeight)
  renderer.setPixelRatio(config.devicePixelRatio.getOrElse(dom.window.devicePixelRatio))
  container.appendChild(renderer.domElement)

  renderer.domElement.style.display = "block"
  renderer.domElement.style.width = "100%"
  renderer.domElement.style.height = "100%"

  // ---- Resize handling ----
  dom.window.addEventListener(
    "resize",
    { (_: dom.Event) =>
      val w = container.clientWidth.max(100)
      val h = container.clientHeight.max(100)
      cols = w / cellWidth
      rows = h / cellHeight
      renderer.setSize(w, h)
    }
  )

  // ---- Convenience accessors ----

  def centerX: Double = container.clientWidth / 2.0
  def centerY: Double = container.clientHeight / 2.0
  def viewportWidth: Int = container.clientWidth
  def viewportHeight: Int = container.clientHeight

  def render(): Unit =
    if (activeCamera != null) renderer.render(scene, activeCamera)

  /** Dispose all Three.js resources. */
  def dispose(): Unit = {
    renderer.dispose()
    if (container.contains(renderer.domElement)) {
      container.removeChild(renderer.domElement)
    }
  }
}
