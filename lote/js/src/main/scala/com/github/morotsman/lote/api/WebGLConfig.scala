package com.github.morotsman.lote.api

/** Configuration for the WebGL / Three.js rendering backend.
  *
  * All values have sensible defaults. Pass a customised instance to [[TerminalPlatform.threeJsTerminal]] to tune the
  * renderer without touching internals.
  *
  * ==Clipping planes==
  * `nearClip` and `farClip` control the perspective camera's frustum. Geometry closer than `nearClip` or farther than
  * `farClip` world units from the camera is clipped (not rendered). Tightening the range improves depth-buffer
  * precision but may clip content in large or deep scenes.
  *
  * ==Field of view==
  * `fieldOfView` sets the base vertical field-of-view in degrees. A smaller FOV zooms in (telephoto), a larger FOV
  * zooms out (wide-angle). Interactive zoom (`+`/`-`/`0`) adjusts the FOV at runtime relative to this base value.
  *
  * ==Camera transitions==
  * `cameraTransitionBaseMs` is the minimum duration (in milliseconds) of an animated camera fly-through between slides.
  * `cameraTransitionFactor` scales with the square root of the travel distance to add extra time for longer journeys.
  *
  * ==Rendering==
  * `antialias` enables multi-sample anti-aliasing on the WebGL renderer (smoother edges, slightly more GPU work).
  * `backgroundColor` sets the scene background as a CSS hex colour string. `devicePixelRatio` overrides the display
  * scaling factor; `None` uses the browser's `window.devicePixelRatio`.
  *
  * ==Zoom==
  * `maxZoom` caps the interactive zoom multiplier (default 20×). `maxFieldOfView` caps the widest FOV allowed when
  * zooming out (default 160°).
  *
  * ==Cell grid==
  * `cellWidth` and `cellHeight` control the size (in CSS pixels) of a single monospace character cell. Changing these
  * affects how many columns/rows fit on screen and the resolution of the offscreen text canvases.
  *
  * Example:
  * {{{
  * val config = WebGLConfig(
  *   nearClip = 0.1,
  *   farClip = 50000.0,
  *   fieldOfView = 60.0,
  *   backgroundColor = "#1a1a2e"
  * )
  * TerminalPlatform.threeJsTerminal[IO](container, config).use { implicit terminal => ... }
  * }}}
  */
case class WebGLConfig(
    nearClip: Double = 1.0,
    farClip: Double = 100000.0,
    fieldOfView: Double = 50.0,
    antialias: Boolean = true,
    backgroundColor: String = "#000000",
    devicePixelRatio: Option[Double] = None,
    cameraTransitionBaseMs: Double = 800.0,
    cameraTransitionFactor: Double = 1.8,
    maxZoom: Double = 20.0,
    maxFieldOfView: Double = 160.0,
    cellWidth: Int = 10,
    cellHeight: Int = 20
) {
  require(nearClip > 0, "nearClip must be positive")
  require(farClip > nearClip, "farClip must be greater than nearClip")
  require(fieldOfView > 0 && fieldOfView < 180, "fieldOfView must be between 0 and 180 degrees (exclusive)")
  require(maxZoom > 0, "maxZoom must be positive")
  require(maxFieldOfView > 0 && maxFieldOfView <= 180, "maxFieldOfView must be between 0 and 180 degrees")
  require(cellWidth > 0, "cellWidth must be positive")
  require(cellHeight > 0, "cellHeight must be positive")
  require(cameraTransitionBaseMs >= 0, "cameraTransitionBaseMs must be non-negative")
  require(cameraTransitionFactor >= 0, "cameraTransitionFactor must be non-negative")
}
