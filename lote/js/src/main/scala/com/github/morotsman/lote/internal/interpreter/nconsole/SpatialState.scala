package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.{Scene3DRef, SlidePosition}
import org.scalajs.dom.HTMLCanvasElement

/** Manages the mutable spatial-mode state for [[ThreeJsTerminal]].
  *
  * ==Responsibilities==
  *
  *   - Resolving slide positions (filling in defaults for slides without explicit positions)
  *   - Deduplicating positions so that slides sharing a 6-DOF position share a single [[SlideLayer]]
  *   - Creating [[SlideLayer]] instances and adding their meshes to the Three.js scene
  *   - Tracking the active layer index and providing access to the active layer's rendering surfaces
  *   - Creating and maintaining the shared [[Scene3DRef]] for scene-aware slides
  *
  * ==Thread-safety==
  *
  * All mutations happen inside `Sync[F].delay` blocks in `ThreeJsTerminal`. This class is not thread-safe on its own —
  * it relies on the single-threaded JS execution model.
  */
private[nconsole] class SpatialState(
    private val glScene: WebGLScene,
    private val cameraAnimator: CameraAnimator,
    private val cellWidth: Int,
    private val cellHeight: Int
) {

  /** All unique slide layers (one per distinct 3-D position). Empty before [[init]]. */
  private var _slideLayers: Vector[SlideLayer] = Vector.empty

  /** Maps a slide index (0-based) to the corresponding index into `_slideLayers`. Multiple slides may map to the same
    * layer when they share a position.
    */
  private var _slideToLayerIndex: Vector[Int] = Vector.empty

  /** Index into `_slideLayers` of the currently active layer. `-1` means no layer is active yet. */
  private var _activeLayerIndex: Int = -1

  /** Shared Scene3DRef for scene-aware slides. `None` until [[init]]. */
  private var _sharedSceneRef: Option[Scene3DRef] = None

  // ---- Accessors ----

  /** Returns the currently active layer, or `None` if no layer is active. */
  def activeLayer: Option[SlideLayer] =
    if (_activeLayerIndex >= 0 && _activeLayerIndex < _slideLayers.length)
      Some(_slideLayers(_activeLayerIndex))
    else
      None

  /** Returns all slide layers. */
  def slideLayers: Vector[SlideLayer] = _slideLayers

  /** Returns the shared [[Scene3DRef]], if spatial mode has been initialised. */
  def sharedSceneRef: Option[Scene3DRef] = _sharedSceneRef

  /** Returns the offscreen canvas of the currently active layer, or `null` if none is active. */
  def activeOffscreen: HTMLCanvasElement =
    activeLayer.map(_.offscreen).orNull

  /** Returns the previous frame of the currently active layer, or empty if none is active. */
  def activeFrame: Vector[String] =
    activeLayer.map(_.previousFrame).getOrElse(Vector.empty)

  /** Returns the mesh of the currently active layer, if one is active. */
  def activeLayerMesh: Option[ThreeMesh] =
    activeLayer.map(_.mesh)

  // ---- Mutations ----

  /** Activates the layer backing the given presentation-level slide index.
    *
    * Maps the slide index through `slideToLayerIndex` to find the deduplicated layer, updates the active layer index,
    * and keeps the [[Scene3DRef]] center in sync with the active layer's mesh position.
    *
    * @param slideIndex
    *   the 0-based slide index from the presentation engine
    */
  def activateLayer(slideIndex: Int): Unit = {
    val layerIdx =
      if (slideIndex >= 0 && slideIndex < _slideToLayerIndex.length)
        _slideToLayerIndex(slideIndex)
      else
        slideIndex
    _activeLayerIndex = layerIdx

    // Keep the Scene3DRef's center in sync with the active layer's mesh position
    _sharedSceneRef.foreach { ref =>
      if (layerIdx >= 0 && layerIdx < _slideLayers.length) {
        val pos = _slideLayers(layerIdx).mesh.position
        ref.updateCenter(pos.x, pos.y, pos.z)
      }
    }
  }

  /** Initialises spatial mode by creating slide layers and configuring the camera and scene ref.
    *
    * ==Position resolution==
    *
    * Each slide in `positions` may carry an explicit [[SlidePosition]] or `None`. Slides without a position inherit the
    * position of the preceding slide (the first slide defaults to the origin).
    *
    * ==Layer deduplication==
    *
    * A [[SlideLayer]] (offscreen canvas + Three.js mesh) is expensive, so slides that share the exact same 6-DOF
    * position `(x, y, z, rotX, rotY, rotZ)` are backed by a single layer. The mapping from slide index to layer index
    * is stored internally.
    *
    * ==Side-effects==
    *
    *   1. Populates `slideLayers` and `slideToLayerIndex`.
    *   2. Adds every layer's mesh to the Three.js scene.
    *   3. Calls `cameraAnimator.initSpatialMode` to set up the perspective camera.
    *   4. Creates the shared `Scene3DRef` instance.
    *
    * @param positions
    *   one entry per slide; `Some(pos)` places the slide explicitly, `None` inherits the previous slide's position.
    */
  def init(positions: Vector[Option[SlidePosition]]): Unit = {

    // ---- Step 1: Resolve positions ----
    var lastPos = SlidePosition(0, 0, 0)
    val resolvedPositions: Vector[SlidePosition] = positions.map {
      case Some(pos) =>
        lastPos = pos
        pos
      case None =>
        lastPos
    }

    // ---- Step 2: Deduplicate by position ----
    val posToLayerIndex =
      scala.collection.mutable.LinkedHashMap.empty[(Double, Double, Double, Double, Double, Double), Int]
    val layerPositions = scala.collection.mutable.ArrayBuffer.empty[SlidePosition]
    val mapping = scala.collection.mutable.ArrayBuffer.empty[Int]

    resolvedPositions.foreach { pos =>
      val key = (pos.x, pos.y, pos.z, pos.rotX, pos.rotY, pos.rotZ)
      val layerIdx = posToLayerIndex.getOrElseUpdate(
        key, {
          val idx = layerPositions.length
          layerPositions += pos
          idx
        }
      )
      mapping += layerIdx
    }

    _slideToLayerIndex = mapping.toVector

    // ---- Step 3: Create SlideLayer instances ----
    _slideLayers = layerPositions.zipWithIndex.map { case (pos, idx) =>
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
        cellWidth = cellWidth,
        cellHeight = cellHeight,
        transparentBg = pos.transparentBackground
      )
      glScene.scene.add(layer.mesh)
      layer
    }.toVector

    // ---- Step 4: Configure the perspective camera ----
    cameraAnimator.initSpatialMode(
      glScene.viewportWidth,
      glScene.viewportHeight
    )

    // ---- Step 5: Create Scene3DRef ----
    _sharedSceneRef = Some(
      new Scene3DRef(
        threeScene = glScene.scene.asInstanceOf[scalajs.js.Dynamic],
        _render = () => glScene.render(),
        perspectiveCamera = cameraAnimator.perspCameraRef.asInstanceOf[scalajs.js.Dynamic],
        _centerX = 0,
        _centerY = 0,
        _centerZ = 0,
        viewportWidth = glScene.viewportWidth,
        viewportHeight = glScene.viewportHeight
      )
    )
  }

  /** Disposes all slide layers. Should be called during terminal cleanup. */
  def dispose(): Unit = {
    _slideLayers.foreach(_.dispose())
  }
}
