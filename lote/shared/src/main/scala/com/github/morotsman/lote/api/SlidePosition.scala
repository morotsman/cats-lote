package com.github.morotsman.lote.api

/** Describes the 3D world-space position and rotation of a slide.
  *
  * On WebGL backends, this determines where the slide surface is placed in the scene and where the camera navigates to.
  * On terminal backends (JLine, xterm.js), this metadata is ignored — slides are rendered in-place as today.
  *
  * @param x  world-space X position
  * @param y  world-space Y position
  * @param z  world-space Z position
  * @param rotX  rotation around X axis in degrees
  * @param rotY  rotation around Y axis in degrees
  * @param rotZ  rotation around Z axis in degrees
  */
final case class SlidePosition(
    x: Double = 0.0,
    y: Double = 0.0,
    z: Double = 0.0,
    rotX: Double = 0.0,
    rotY: Double = 0.0,
    rotZ: Double = 0.0
)

