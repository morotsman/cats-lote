package com.github.morotsman.lote.internal.interpreter.nconsole.scene

/** Abstracts over the Three.js scene graph to decouple rendering logic from the browser environment.
  *
  * Components like [[SpatialState]], [[CameraAnimator]], [[WebGLEffectRenderer]], and [[SlideLayer]] interact with the
  * scene graph through this trait instead of directly manipulating `ThreeScene`, `ThreeMesh`, and
  * `ThreePerspectiveCamera` instances.
  *
  * In production, [[WebGLScene]] provides the real Three.js implementation. In tests, [[StubSceneBackend]] records all
  * operations in a log for assertion.
  *
  * ==Design rationale==
  *
  * The `MeshRef` and `CameraRef` type members allow the production implementation to use the actual Three.js types
  * while test stubs can use simple identifiers (e.g. `Int` or `String`).
  */
private[nconsole] trait SceneBackend {
  type MeshRef
  type CameraRef

  // ---- Mesh operations ----

  def createPlaneMesh(width: Double, height: Double, textureSource: Any): MeshRef
  def addToScene(mesh: MeshRef): Unit
  def removeFromScene(mesh: MeshRef): Unit
  def setMeshPosition(mesh: MeshRef, x: Double, y: Double, z: Double): Unit
  def setMeshRotation(mesh: MeshRef, rx: Double, ry: Double, rz: Double): Unit
  def setMeshOpacity(mesh: MeshRef, opacity: Double): Unit
  def setMeshScale(mesh: MeshRef, sx: Double, sy: Double, sz: Double): Unit
  def setMeshDepthWrite(mesh: MeshRef, depthWrite: Boolean): Unit
  def getMeshPosition(mesh: MeshRef): (Double, Double, Double)
  def getMeshRotation(mesh: MeshRef): (Double, Double, Double)
  def disposeMesh(mesh: MeshRef): Unit

  // ---- Camera operations ----

  def createCamera(fov: Double, aspect: Double, near: Double, far: Double): CameraRef
  def setCameraPosition(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def setCameraUp(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def setCameraLookAt(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def getCameraPosition(cam: CameraRef): (Double, Double, Double)
  def getCameraUp(cam: CameraRef): (Double, Double, Double)
  def setCameraFov(cam: CameraRef, fov: Double): Unit
  def updateCameraProjection(cam: CameraRef): Unit

  // ---- Scene operations ----

  def setBackgroundColor(color: String): Unit
  def render(camera: CameraRef): Unit

  // ---- Viewport ----

  def viewportWidth: Int
  def viewportHeight: Int
  def centerX: Double
  def centerY: Double
}
