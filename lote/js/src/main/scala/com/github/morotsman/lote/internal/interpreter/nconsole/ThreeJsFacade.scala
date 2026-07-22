package com.github.morotsman.lote.internal.interpreter.nconsole

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom
import scala.annotation.nowarn

// ---------------------------------------------------------------------------
// Three.js Scala.js facades — typed surface for the terminal rendering layer.
// Three.js is expected to be available as a global (`window.THREE`) via CDN.
//
// Minimum supported Three.js version: 0.160.0
// These facades were written against the Three.js r160 API surface.
// If you upgrade Three.js, verify that the facades still compile and that the
// rendering pipeline behaves correctly. Pin the exact version in the HTML
// <script> tags (e.g. three@0.160.0) to avoid unexpected breakage.
//
// These facades cover every Three.js type used in the codebase, eliminating
// the need for js.Dynamic casts in the rendering pipeline.
// ---------------------------------------------------------------------------

object ThreeJsFacade {

  /** The minimum Three.js version these facades are tested against. */
  val MinimumThreeJsVersion: String = "0.160.0"

  /** Log a console warning if the loaded Three.js revision doesn't match. */
  def checkVersion(): Unit = {
    val revision = js.Dynamic.global.THREE.REVISION.asInstanceOf[js.UndefOr[String]]
    revision.foreach { rev =>
      if (rev != "160") {
        dom.console.warn(
          s"[cats-lote] Three.js revision $rev detected, but facades target r160 " +
            s"(version $MinimumThreeJsVersion). Rendering issues may occur."
        )
      }
    }
  }
}

// ---- Material options (typed constructor parameters) ----

/** Typed options for `new THREE.MeshBasicMaterial({ ... })`.
  *
  * Use `ThreeMaterialOptions()` to construct, then pass to `new ThreeMeshBasicMaterial(opts)`.
  */
@nowarn("cat=unused")
trait ThreeMaterialOptions extends js.Object {
  var map: js.UndefOr[ThreeCanvasTexture] = js.undefined
  var transparent: js.UndefOr[Boolean] = js.undefined
  var opacity: js.UndefOr[Double] = js.undefined
  var side: js.UndefOr[Int] = js.undefined
  var depthWrite: js.UndefOr[Boolean] = js.undefined
  var color: js.UndefOr[String] = js.undefined
}

@nowarn("cat=unused")
object ThreeMaterialOptions {
  def apply(
      map: js.UndefOr[ThreeCanvasTexture] = js.undefined,
      transparent: js.UndefOr[Boolean] = js.undefined,
      opacity: js.UndefOr[Double] = js.undefined,
      side: js.UndefOr[Int] = js.undefined,
      depthWrite: js.UndefOr[Boolean] = js.undefined,
      color: js.UndefOr[String] = js.undefined
  ): ThreeMaterialOptions = {
    val opts = (new js.Object).asInstanceOf[ThreeMaterialOptions]
    if (map != js.undefined) opts.map = map
    if (transparent != js.undefined) opts.transparent = transparent
    if (opacity != js.undefined) opts.opacity = opacity
    if (side != js.undefined) opts.side = side
    if (depthWrite != js.undefined) opts.depthWrite = depthWrite
    if (color != js.undefined) opts.color = color
    opts
  }
}

/** Typed options for `new THREE.WebGLRenderer({ ... })`. */
@nowarn("cat=unused")
trait ThreeRendererOptions extends js.Object {
  var antialias: js.UndefOr[Boolean] = js.undefined
  var alpha: js.UndefOr[Boolean] = js.undefined
}

@nowarn("cat=unused")
object ThreeRendererOptions {
  def apply(
      antialias: js.UndefOr[Boolean] = js.undefined,
      alpha: js.UndefOr[Boolean] = js.undefined
  ): ThreeRendererOptions = {
    val opts = (new js.Object).asInstanceOf[ThreeRendererOptions]
    if (antialias != js.undefined) opts.antialias = antialias
    if (alpha != js.undefined) opts.alpha = alpha
    opts
  }
}

// ---- Core scene graph ----

@js.native
@JSGlobal("THREE.Scene")
@nowarn("cat=unused")
class ThreeScene() extends js.Object {
  def add(obj: js.Any): Unit = js.native
  def remove(obj: js.Any): Unit = js.native
  var background: js.Any = js.native
}

@js.native
@JSGlobal("THREE.OrthographicCamera")
@nowarn("cat=unused")
class ThreeOrthographicCamera(
    left0: Double,
    right0: Double,
    top0: Double,
    bottom0: Double,
    near: Double,
    far: Double
) extends js.Object {
  var position: ThreeVector3 = js.native
  var up: ThreeVector3 = js.native
  def updateProjectionMatrix(): Unit = js.native
  def lookAt(x: Double, y: Double, z: Double): Unit = js.native
  var left: Double = js.native
  var right: Double = js.native
  var top: Double = js.native
  var bottom: Double = js.native
}

@js.native
@JSGlobal("THREE.Vector3")
@nowarn("cat=unused")
class ThreeVector3(x0: Double, y0: Double, z0: Double) extends js.Object {
  def this() = this(0, 0, 0)
  def set(x: Double, y: Double, z: Double): ThreeVector3 = js.native
  var x: Double = js.native
  var y: Double = js.native
  var z: Double = js.native
}

@js.native
@JSGlobal("THREE.PerspectiveCamera")
@nowarn("cat=unused")
class ThreePerspectiveCamera(
    fov0: Double,
    aspect0: Double,
    near0: Double,
    far0: Double
) extends js.Object {
  var position: ThreeVector3 = js.native
  var up: ThreeVector3 = js.native
  def updateProjectionMatrix(): Unit = js.native
  def lookAt(x: Double, y: Double, z: Double): Unit = js.native
  var fov: Double = js.native
  var aspect: Double = js.native
  var far: Double = js.native
  var near: Double = js.native
}

// ---- Renderer ----

@js.native
@JSGlobal("THREE.WebGLRenderer")
@nowarn("cat=unused")
class ThreeWebGLRenderer(params: js.UndefOr[js.Object] = js.undefined) extends js.Object {
  def setSize(width: Double, height: Double): Unit = js.native
  def setPixelRatio(ratio: Double): Unit = js.native
  def render(scene: ThreeScene, camera: js.Any): Unit = js.native
  def dispose(): Unit = js.native
  val domElement: dom.HTMLCanvasElement = js.native
}

// ---- Geometry ----

@js.native
@JSGlobal("THREE.PlaneGeometry")
@nowarn("cat=unused")
class ThreePlaneGeometry(width: Double, height: Double) extends js.Object {
  def dispose(): Unit = js.native
}

// ---- Materials ----

@js.native
@JSGlobal("THREE.MeshBasicMaterial")
@nowarn("cat=unused")
class ThreeMeshBasicMaterial(params: js.UndefOr[js.Object] = js.undefined) extends js.Object {
  var map: js.Any = js.native
  var opacity: Double = js.native
  var transparent: Boolean = js.native
  var depthWrite: Boolean = js.native
  var side: Int = js.native
  var needsUpdate: Boolean = js.native
  def dispose(): Unit = js.native
}

// ---- Mesh ----

@js.native
@JSGlobal("THREE.Mesh")
@nowarn("cat=unused")
class ThreeMesh(geometry0: ThreePlaneGeometry, material0: ThreeMeshBasicMaterial) extends js.Object {
  var position: ThreeVector3 = js.native
  var rotation: ThreeEuler = js.native
  var scale: ThreeVector3 = js.native
  var material: ThreeMeshBasicMaterial = js.native
  var userData: js.Any = js.native
  var geometry: ThreePlaneGeometry = js.native
}

// ---- Euler ----

@js.native
@JSGlobal("THREE.Euler")
@nowarn("cat=unused")
class ThreeEuler() extends js.Object {
  def set(x: Double, y: Double, z: Double): ThreeEuler = js.native
  var x: Double = js.native
  var y: Double = js.native
  var z: Double = js.native
}

// ---- Textures ----

@js.native
@JSGlobal("THREE.CanvasTexture")
@nowarn("cat=unused")
class ThreeCanvasTexture(canvas: dom.HTMLCanvasElement) extends js.Object {
  var needsUpdate: Boolean = js.native
  var minFilter: js.Any = js.native
  var magFilter: js.Any = js.native
  def dispose(): Unit = js.native
}

@js.native
@JSGlobal("THREE.NearestFilter")
@nowarn("cat=unused")
object ThreeNearestFilter extends js.Object

@js.native
@JSGlobal("THREE.LinearFilter")
@nowarn("cat=unused")
object ThreeLinearFilter extends js.Object

// ---- Misc ----

@js.native
@JSGlobal("THREE.Color")
@nowarn("cat=unused")
class ThreeColor(color: String) extends js.Object

/** Constants for `THREE.DoubleSide`, etc. Access via `ThreeSide.DoubleSide`. */
@js.native
@JSGlobal("THREE.DoubleSide")
@nowarn("cat=unused")
object ThreeDoubleSide extends js.Object {

  /** Integer value representing DoubleSide (2). Cast to Int for material `side` property. */
  override def toString(): String = js.native
}

// ---- Per-particle user data (typed alternatives to js.Dynamic.literal) ----

/** Typed user data for smoke-effect particles. */
trait SmokeParticleData extends js.Object {
  val startX: Double
  val startY: Double
  val driftX: Double
  val driftY: Double
  val rotSpeed: Double
  val fadeDelay: Double
  val shrinkRate: Double
}

object SmokeParticleData {
  def apply(
      startX: Double,
      startY: Double,
      driftX: Double,
      driftY: Double,
      rotSpeed: Double,
      fadeDelay: Double,
      shrinkRate: Double
  ): SmokeParticleData =
    js.Dynamic
      .literal(
        startX = startX,
        startY = startY,
        driftX = driftX,
        driftY = driftY,
        rotSpeed = rotSpeed,
        fadeDelay = fadeDelay,
        shrinkRate = shrinkRate
      )
      .asInstanceOf[SmokeParticleData]
}

/** Typed user data for dissolve-effect particles. */
trait DissolveParticleData extends js.Object {
  val fadeDelay: Double
  val fadeSpeed: Double
}

object DissolveParticleData {
  def apply(fadeDelay: Double, fadeSpeed: Double): DissolveParticleData =
    js.Dynamic
      .literal(fadeDelay = fadeDelay, fadeSpeed = fadeSpeed)
      .asInstanceOf[DissolveParticleData]
}
