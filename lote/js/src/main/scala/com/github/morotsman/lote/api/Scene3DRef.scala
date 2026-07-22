package com.github.morotsman.lote.api

import scala.scalajs.js

/** Provides access to the shared Three.js 3D scene for scene-aware slides.
  *
  * Scene-aware slides (like `Landscape3DSlide`) can add their own 3D geometry to the shared presentation scene instead
  * of creating a separate WebGL context. This enables smooth camera transitions into and out of 3D content, world
  * continuity between slides, and avoids multi-context overhead.
  *
  * Obtain this via `SlideContext.scene3DRef` (available only on WebGL backends in spatial mode).
  *
  * Example:
  * {{{
  * val sceneRef = ctx.scene3DRef.map(_.asInstanceOf[Scene3DRef])
  * sceneRef.foreach { ref =>
  *   val group = js.Dynamic.newInstance(js.Dynamic.global.THREE.Group)()
  *   // ... add meshes to group ...
  *   group.position.set(ref.centerX, ref.centerY, ref.centerZ)
  *   ref.addToScene(group)
  * }
  * }}}
  */
class Scene3DRef private[lote] (
    val threeScene: js.Dynamic,
    private val _render: () => Unit,
    val perspectiveCamera: js.Dynamic,
    private var _centerX: Double,
    private var _centerY: Double,
    private var _centerZ: Double,
    val viewportWidth: Int,
    val viewportHeight: Int
) {

  /** Trigger a re-render of the shared scene. Call this after modifying scene objects. */
  def render(): Unit = _render()

  /** X coordinate of the active slide's center in world space. */
  def centerX: Double = _centerX

  /** Y coordinate of the active slide's center in world space. */
  def centerY: Double = _centerY

  /** Z coordinate of the active slide's center in world space. */
  def centerZ: Double = _centerZ

  /** Add a Three.js object (mesh, group, light, etc.) to the shared scene. */
  def addToScene(obj: js.Any): Unit = threeScene.add(obj)

  /** Remove a Three.js object from the shared scene. */
  def removeFromScene(obj: js.Any): Unit = threeScene.remove(obj)

  /** Update the slide center coordinates (called internally when the active layer changes).
    *
    * Performance: direct var mutation is used here because this is called from within a Sync[F].delay block on every
    * slide activation — Ref would add unnecessary allocation for a simple coordinate update on single-threaded JS.
    */
  private[lote] def updateCenter(x: Double, y: Double, z: Double): Unit = {
    _centerX = x
    _centerY = y
    _centerZ = z
  }
}
