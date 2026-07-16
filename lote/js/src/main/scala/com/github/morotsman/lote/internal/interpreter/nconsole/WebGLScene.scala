package com.github.morotsman.lote.internal.interpreter.nconsole

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement, HTMLElement}

/** Manages the Three.js scene, camera, renderer, and the textured plane that displays terminal content.
  *
  * All mutable Three.js state lives here. Other components receive a `WebGLScene` reference
  * to render frames and apply transforms.
  */
private[nconsole] class WebGLScene(container: HTMLElement, val cellWidth: Int, val cellHeight: Int) {

  private val containerWidth: Int  = container.clientWidth.max(100)
  private val containerHeight: Int = container.clientHeight.max(100)

  var cols: Int = containerWidth / cellWidth
  var rows: Int = containerHeight / cellHeight

  // ---- Offscreen canvas for text rendering ----
  val offscreen: HTMLCanvasElement =
    dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  offscreen.width = cols * cellWidth
  offscreen.height = rows * cellHeight

  val ctx: CanvasRenderingContext2D =
    offscreen.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  // ---- Three.js scene ----
  val scene    = new ThreeScene()
  scene.background = new ThreeColor("#000000")

  val camera = new ThreeOrthographicCamera(
    0, containerWidth, containerHeight, 0,
    0.1, 100
  )
  camera.position.set(0, 0, 5)

  private val rendererOpts = scalajs.js.Dynamic.literal(antialias = true, alpha = true)
  val renderer = new ThreeWebGLRenderer(rendererOpts.asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]])
  renderer.setSize(containerWidth, containerHeight)
  renderer.setPixelRatio(dom.window.devicePixelRatio)
  container.appendChild(renderer.domElement)

  renderer.domElement.style.display = "block"
  renderer.domElement.style.width = "100%"
  renderer.domElement.style.height = "100%"

  // ---- Textured plane ----
  val texture = new ThreeCanvasTexture(offscreen)
  texture.minFilter = ThreeLinearFilter
  texture.magFilter = ThreeLinearFilter

  val planeGeo = new ThreePlaneGeometry(containerWidth, containerHeight)
  val planeMat = new ThreeMeshBasicMaterial(
    scalajs.js.Dynamic.literal(map = texture, transparent = true, side = 2)
      .asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]]
  )
  val plane = new ThreeMesh(planeGeo, planeMat)
  plane.position.set(containerWidth / 2.0, containerHeight / 2.0, 0)
  scene.add(plane)

  // Initial render
  renderer.render(scene, camera)

  // ---- Resize handling ----
  dom.window.addEventListener("resize", { (_: dom.Event) =>
    val w = container.clientWidth.max(100)
    val h = container.clientHeight.max(100)
    cols = w / cellWidth
    rows = h / cellHeight

    offscreen.width = cols * cellWidth
    offscreen.height = rows * cellHeight

    renderer.setSize(w, h)
    camera.right = w
    camera.top = h
    camera.updateProjectionMatrix()

    plane.position.set(w / 2.0, h / 2.0, 0)
    renderer.render(scene, camera)
  })

  // ---- Convenience accessors ----

  def centerX: Double = container.clientWidth / 2.0
  def centerY: Double = container.clientHeight / 2.0
  def viewportWidth: Int = container.clientWidth
  def viewportHeight: Int = container.clientHeight

  def render(): Unit = renderer.render(scene, camera)

  /** Dispose all Three.js resources. */
  def dispose(): Unit = {
    texture.dispose()
    planeMat.dispose()
    planeGeo.dispose()
    renderer.dispose()
    if (container.contains(renderer.domElement)) {
      container.removeChild(renderer.domElement)
    }
  }
}

