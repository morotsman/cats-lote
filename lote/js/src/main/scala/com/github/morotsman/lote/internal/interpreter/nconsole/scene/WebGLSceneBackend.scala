package com.github.morotsman.lote.internal.interpreter.nconsole.scene

import com.github.morotsman.lote.internal.interpreter.nconsole.facade._

/** Production implementation of [[SceneBackend]] backed by a real [[WebGLScene]] and Three.js types.
  *
  * Delegates all scene graph and camera operations to the actual Three.js objects. Used in production; tests use
  * [[StubSceneBackend]] instead.
  */
private[nconsole] class WebGLSceneBackend(glScene: WebGLScene) extends SceneBackend {

  type MeshRef = ThreeMesh
  type CameraRef = ThreePerspectiveCamera

  // ---- Mesh operations ----

  override def createPlaneMesh(width: Double, height: Double, textureSource: Any): ThreeMesh = {
    val tex = new ThreeCanvasTexture(textureSource.asInstanceOf[org.scalajs.dom.HTMLCanvasElement])
    tex.minFilter = ThreeLinearFilter
    tex.magFilter = ThreeNearestFilter
    val geo = new ThreePlaneGeometry(width, height)
    val mat = new ThreeMeshBasicMaterial(
      ThreeMaterialOptions(map = tex, transparent = true, side = 2)
    )
    new ThreeMesh(geo, mat)
  }

  override def addToScene(mesh: ThreeMesh): Unit = glScene.scene.add(mesh)
  override def removeFromScene(mesh: ThreeMesh): Unit = glScene.scene.remove(mesh)

  override def setMeshPosition(mesh: ThreeMesh, x: Double, y: Double, z: Double): Unit =
    mesh.position.set(x, y, z)

  override def setMeshRotation(mesh: ThreeMesh, rx: Double, ry: Double, rz: Double): Unit =
    mesh.rotation.set(rx, ry, rz)

  override def setMeshOpacity(mesh: ThreeMesh, opacity: Double): Unit = {
    mesh.material.opacity = opacity
    mesh.material.needsUpdate = true
  }

  override def setMeshScale(mesh: ThreeMesh, sx: Double, sy: Double, sz: Double): Unit =
    mesh.scale.set(sx, sy, sz)

  override def setMeshDepthWrite(mesh: ThreeMesh, depthWrite: Boolean): Unit =
    mesh.material.depthWrite = depthWrite

  override def getMeshPosition(mesh: ThreeMesh): (Double, Double, Double) =
    (mesh.position.x, mesh.position.y, mesh.position.z)

  override def getMeshRotation(mesh: ThreeMesh): (Double, Double, Double) =
    (mesh.rotation.x, mesh.rotation.y, mesh.rotation.z)

  override def disposeMesh(mesh: ThreeMesh): Unit = {
    mesh.geometry.dispose()
    mesh.material.dispose()
  }

  // ---- Camera operations ----

  override def createCamera(fov: Double, aspect: Double, near: Double, far: Double): ThreePerspectiveCamera = {
    val cam = new ThreePerspectiveCamera(fov, aspect, near, far)
    glScene.activeCamera = cam
    cam
  }

  override def setCameraPosition(cam: ThreePerspectiveCamera, x: Double, y: Double, z: Double): Unit =
    cam.position.set(x, y, z)

  override def setCameraUp(cam: ThreePerspectiveCamera, x: Double, y: Double, z: Double): Unit =
    cam.up.set(x, y, z)

  override def setCameraLookAt(cam: ThreePerspectiveCamera, x: Double, y: Double, z: Double): Unit =
    cam.lookAt(x, y, z)

  override def setCameraFov(cam: ThreePerspectiveCamera, fov: Double): Unit =
    cam.fov = fov

  override def updateCameraProjection(cam: ThreePerspectiveCamera): Unit =
    cam.updateProjectionMatrix()

  override def getCameraPosition(cam: ThreePerspectiveCamera): (Double, Double, Double) =
    (cam.position.x, cam.position.y, cam.position.z)

  override def getCameraUp(cam: ThreePerspectiveCamera): (Double, Double, Double) =
    (cam.up.x, cam.up.y, cam.up.z)

  // ---- Scene operations ----

  override def setBackgroundColor(color: String): Unit =
    glScene.scene.background = new ThreeColor(color)

  override def render(camera: ThreePerspectiveCamera): Unit =
    glScene.render()

  // ---- Viewport ----

  override def viewportWidth: Int = glScene.viewportWidth
  override def viewportHeight: Int = glScene.viewportHeight
  override def centerX: Double = glScene.centerX
  override def centerY: Double = glScene.centerY
}
