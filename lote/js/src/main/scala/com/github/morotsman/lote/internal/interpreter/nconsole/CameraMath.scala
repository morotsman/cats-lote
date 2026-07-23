package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.SlidePosition

/** Pure mathematical utilities for camera positioning and animation in 3-D space.
  *
  * Extracted from [[CameraAnimator]] to enable unit testing of the rotation matrix, interpolation, and timing logic
  * without requiring a Three.js scene or DOM.
  */
private[nconsole] object CameraMath {

  /** Conversion factor from degrees to radians. */
  val Deg2Rad: Double = Math.PI / 180.0

  /** Computes the outward-facing normal of a slide given its Euler XYZ rotation.
    *
    * The slide's local +Z axis points outward. Applying the rotation R = Rx · Ry · Rz transforms local Z into world
    * space. The result is the third column of the combined rotation matrix.
    *
    * @return
    *   a unit vector `(nx, ny, nz)` pointing away from the slide surface.
    */
  def computeNormal(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    (s2, -s1 * c2, c1 * c2)
  }

  /** Computes the up vector of a slide given its Euler XYZ rotation.
    *
    * The up vector is the second column of the rotation matrix R = Rx · Ry · Rz.
    *
    * @return
    *   a unit vector `(ux, uy, uz)` representing the slide's "up" in world space.
    */
  def computeUp(pos: SlidePosition): (Double, Double, Double) = {
    val rx = pos.rotX * Deg2Rad
    val ry = pos.rotY * Deg2Rad
    val rz = pos.rotZ * Deg2Rad
    val c1 = Math.cos(rx); val s1 = Math.sin(rx)
    val c2 = Math.cos(ry); val s2 = Math.sin(ry)
    val c3 = Math.cos(rz); val s3 = Math.sin(rz)
    (-c2 * s3, c1 * c3 - s1 * s2 * s3, s1 * c3 + c1 * s2 * s3)
  }

  /** Computes the Euclidean distance between two slide positions in 3-D space.
    *
    * Only the translational components (x, y, z) are considered; rotations are ignored.
    */
  def euclidean(a: SlidePosition, b: SlidePosition): Double = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    val dz = b.z - a.z
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  /** Computes the center of a slide in world space.
    *
    * The slide's origin is at its top-left corner, so the center is offset by half the dimensions.
    */
  def slideCenter(pos: SlidePosition, slideWidth: Double, slideHeight: Double): (Double, Double, Double) =
    (pos.x + slideWidth / 2.0, pos.y + slideHeight / 2.0, pos.z)

  /** Computes the world-space camera position that frames the given slide.
    *
    * The camera is placed at the slide center offset along the outward surface normal by `cameraDistance`.
    */
  def cameraPositionFor(
      pos: SlidePosition,
      slideWidth: Double,
      slideHeight: Double,
      cameraDistance: Double
  ): (Double, Double, Double) = {
    val (cx, cy, cz) = slideCenter(pos, slideWidth, slideHeight)
    val (nx, ny, nz) = computeNormal(pos)
    (cx + nx * cameraDistance, cy + ny * cameraDistance, cz + nz * cameraDistance)
  }

  /** Computes the animation duration in milliseconds for a given travel distance.
    *
    * Uses a base duration plus a square-root scaling factor for sub-linear growth.
    */
  def transitionDuration(distance: Double, baseMs: Double, kFactor: Double): Double =
    baseMs + kFactor * Math.sqrt(distance)

  /** Smoothstep ease-in-out interpolation function.
    *
    * Maps `t` in [0, 1] to a smooth S-curve using the Hermite polynomial 3t² − 2t³. Values outside [0, 1] are clamped.
    */
  def easeInOut(t: Double): Double = {
    val c = t.max(0.0).min(1.0)
    c * c * (3.0 - 2.0 * c)
  }

  /** Linearly interpolates between two values. */
  def lerp(a: Double, b: Double, t: Double): Double =
    a + (b - a) * t
}
