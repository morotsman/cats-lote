package com.github.morotsman.lote.api

/** Describes the 3D world-space position and rotation of a slide.
 *
 * On WebGL backends, this determines where the slide surface is placed in the scene and where the camera navigates to.
 * On terminal backends (JLine, xterm.js), this metadata is ignored — slides are rendered in-place as today.
 *
 * ==Coordinate system==
 *
 * Coordinates are in CSS-pixel-scale world units. Each slide's surface size is determined by the number of character
 * cells multiplied by the `cellWidth` / `cellHeight` from `WebGLConfig` (default 10 × 20 pixels). For example, an
 * 80×24 terminal produces a slide surface of 800 × 480 world units.
 *
 * When placing slides, use spacing large enough to avoid overlapping surfaces. Typical spacing is 1800 units
 * horizontally (x) and 1200–2400 units vertically (y), depending on desired density. The z-axis controls depth:
 * positive values move the slide toward the camera, negative values move it away.
 *
 * Example layout:
 * {{{
 * .at(0, 0, 0)         // origin — first slide
 * .at(1800, 0, 0)      // one column to the right
 * .at(0, 2400, 0)      // one row down
 * .at(3600, 2400, 500) // two columns right, one row down, slightly toward camera
 * }}}
 *
 * @param x
 *   world-space X position (CSS-pixel-scale units; positive = right)
 * @param y
 *   world-space Y position (CSS-pixel-scale units; positive = down)
 * @param z
 *   world-space Z position (CSS-pixel-scale units; positive = toward camera, negative = away)
  * @param rotX
  *   rotation around X axis in degrees
  * @param rotY
  *   rotation around Y axis in degrees
  * @param rotZ
  *   rotation around Z axis in degrees
  * @param transparentBackground
  *   if true, the slide's background is transparent (WebGL spatial mode only)
  */
final case class SlidePosition(
    x: Double = 0.0,
    y: Double = 0.0,
    z: Double = 0.0,
    rotX: Double = 0.0,
    rotY: Double = 0.0,
    rotZ: Double = 0.0,
    transparentBackground: Boolean = false
)
