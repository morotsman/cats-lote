package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.SlidePosition

/** Pure helper functions extracted from [[SpatialState.init]] for testability.
  *
  * These algorithms resolve and deduplicate slide positions without any side-effects or platform dependencies.
  */
private[lote] object SpatialMath {

  /** Fill-forward: slides without an explicit position inherit the previous slide's position.
    *
    * The first slide defaults to the origin `SlidePosition(0, 0, 0)` if it has no explicit position.
    *
    * @param positions
    *   one entry per slide; `Some(pos)` for an explicit position, `None` to inherit
    * @return
    *   a fully-resolved position for every slide
    */
  def resolvePositions(positions: Vector[Option[SlidePosition]]): Vector[SlidePosition] = {
    var lastPos = SlidePosition(0, 0, 0)
    positions.map {
      case Some(pos) =>
        lastPos = pos
        pos
      case None =>
        lastPos
    }
  }

  /** Deduplicate slides that share the same 6-DOF position `(x, y, z, rotX, rotY, rotZ)`.
    *
    * Returns:
    *   - `uniquePositions`: the distinct positions in insertion order
    *   - `slideToLayerIndex`: for each input slide, the index into `uniquePositions` of the layer it maps to
    *
    * @param resolved
    *   fully-resolved positions (one per slide, no `None`s)
    * @return
    *   `(uniquePositions, slideToLayerIndex)`
    */
  def deduplicatePositions(resolved: Vector[SlidePosition]): (Vector[SlidePosition], Vector[Int]) = {
    val posToLayerIndex =
      scala.collection.mutable.LinkedHashMap.empty[(Double, Double, Double, Double, Double, Double), Int]
    val layerPositions = scala.collection.mutable.ArrayBuffer.empty[SlidePosition]
    val mapping = scala.collection.mutable.ArrayBuffer.empty[Int]

    resolved.foreach { pos =>
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

    (layerPositions.toVector, mapping.toVector)
  }
}
