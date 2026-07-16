package com.github.morotsman.lote.internal.interpreter.nconsole

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom
import scala.annotation.nowarn

// ---------------------------------------------------------------------------
// Three.js Scala.js facades — minimal surface needed for terminal rendering.
// Three.js is expected to be available as a global (`window.THREE`) via CDN.
// ---------------------------------------------------------------------------

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
  def updateProjectionMatrix(): Unit = js.native
  var left: Double = js.native
  var right: Double = js.native
  var top: Double = js.native
  var bottom: Double = js.native
}

@js.native
@JSGlobal("THREE.Vector3")
@nowarn("cat=unused")
class ThreeVector3(x: Double, y: Double, z: Double) extends js.Object {
  def set(x: Double, y: Double, z: Double): ThreeVector3 = js.native
}

@js.native
@JSGlobal("THREE.WebGLRenderer")
@nowarn("cat=unused")
class ThreeWebGLRenderer(params: js.UndefOr[js.Object] = js.undefined) extends js.Object {
  def setSize(width: Double, height: Double): Unit = js.native
  def setPixelRatio(ratio: Double): Unit = js.native
  def render(scene: ThreeScene, camera: ThreeOrthographicCamera): Unit = js.native
  def dispose(): Unit = js.native
  val domElement: dom.HTMLCanvasElement = js.native
}

@js.native
@JSGlobal("THREE.PlaneGeometry")
@nowarn("cat=unused")
class ThreePlaneGeometry(width: Double, height: Double) extends js.Object {
  def dispose(): Unit = js.native
}

@js.native
@JSGlobal("THREE.MeshBasicMaterial")
@nowarn("cat=unused")
class ThreeMeshBasicMaterial(params: js.UndefOr[js.Object] = js.undefined) extends js.Object {
  var map: js.Any = js.native
  var needsUpdate: Boolean = js.native
  def dispose(): Unit = js.native
}

@js.native
@JSGlobal("THREE.Mesh")
@nowarn("cat=unused")
class ThreeMesh(geometry: ThreePlaneGeometry, material: ThreeMeshBasicMaterial) extends js.Object {
  var position: ThreeVector3 = js.native
}

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

@js.native
@JSGlobal("THREE.Color")
@nowarn("cat=unused")
class ThreeColor(color: String) extends js.Object


