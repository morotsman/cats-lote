package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.internal.interpreter.nconsole.scene.SceneBackend

/** Sealed trait for recording scene graph operations performed against a [[SceneBackend]]. */
sealed trait SceneOp

object SceneOp {
  case class CreatePlaneMesh(width: Double, height: Double) extends SceneOp
  case class AddToScene(meshId: Int) extends SceneOp
  case class RemoveFromScene(meshId: Int) extends SceneOp
  case class SetMeshPosition(meshId: Int, x: Double, y: Double, z: Double) extends SceneOp
  case class SetMeshRotation(meshId: Int, rx: Double, ry: Double, rz: Double) extends SceneOp
  case class SetMeshOpacity(meshId: Int, opacity: Double) extends SceneOp
  case class SetMeshScale(meshId: Int, sx: Double, sy: Double, sz: Double) extends SceneOp
  case class SetMeshDepthWrite(meshId: Int, depthWrite: Boolean) extends SceneOp
  case class DisposeMesh(meshId: Int) extends SceneOp
  case class CreateCamera(fov: Double, aspect: Double, near: Double, far: Double) extends SceneOp
  case class SetCameraPosition(camId: Int, x: Double, y: Double, z: Double) extends SceneOp
  case class SetCameraUp(camId: Int, x: Double, y: Double, z: Double) extends SceneOp
  case class SetCameraLookAt(camId: Int, x: Double, y: Double, z: Double) extends SceneOp
  case class SetCameraFov(camId: Int, fov: Double) extends SceneOp
  case class UpdateCameraProjection(camId: Int) extends SceneOp
  case class SetBackgroundColor(color: String) extends SceneOp
  case class Render(camId: Int) extends SceneOp
}

/** Test stub for [[SceneBackend]] that records all operations in an observable log.
  *
  * Uses `Int` IDs for mesh and camera references. All operations are appended to the [[ops]] list which can be
  * inspected in assertions.
  *
  * @param _viewportWidth
  *   the viewport width to report
  * @param _viewportHeight
  *   the viewport height to report
  */
class StubSceneBackend(
    _viewportWidth: Int = 800,
    _viewportHeight: Int = 600
) extends SceneBackend {

  type MeshRef = Int
  type CameraRef = Int

  private var _nextMeshId = 1
  private var _nextCameraId = 1

  /** Mutable log of all operations performed on this backend. */
  var ops: List[SceneOp] = Nil

  /** Mesh positions tracked for [[getMeshPosition]]. */
  private var _meshPositions: Map[Int, (Double, Double, Double)] = Map.empty

  /** Mesh rotations tracked for [[getMeshRotation]]. */
  private var _meshRotations: Map[Int, (Double, Double, Double)] = Map.empty

  /** Camera positions tracked for [[getCameraPosition]]. */
  private var _cameraPositions: Map[Int, (Double, Double, Double)] = Map.empty

  /** Camera up vectors tracked for [[getCameraUp]]. */
  private var _cameraUps: Map[Int, (Double, Double, Double)] = Map.empty

  private def log(op: SceneOp): Unit = ops = ops :+ op

  override def createPlaneMesh(width: Double, height: Double, textureSource: Any): Int = {
    val id = _nextMeshId; _nextMeshId += 1
    log(SceneOp.CreatePlaneMesh(width, height))
    _meshPositions += (id -> (0.0, 0.0, 0.0))
    id
  }

  override def addToScene(mesh: Int): Unit = log(SceneOp.AddToScene(mesh))
  override def removeFromScene(mesh: Int): Unit = log(SceneOp.RemoveFromScene(mesh))

  override def setMeshPosition(mesh: Int, x: Double, y: Double, z: Double): Unit = {
    _meshPositions += (mesh -> (x, y, z))
    log(SceneOp.SetMeshPosition(mesh, x, y, z))
  }

  override def setMeshRotation(mesh: Int, rx: Double, ry: Double, rz: Double): Unit = {
    _meshRotations += (mesh -> (rx, ry, rz))
    log(SceneOp.SetMeshRotation(mesh, rx, ry, rz))
  }

  override def setMeshOpacity(mesh: Int, opacity: Double): Unit =
    log(SceneOp.SetMeshOpacity(mesh, opacity))

  override def setMeshScale(mesh: Int, sx: Double, sy: Double, sz: Double): Unit =
    log(SceneOp.SetMeshScale(mesh, sx, sy, sz))

  override def setMeshDepthWrite(mesh: Int, depthWrite: Boolean): Unit =
    log(SceneOp.SetMeshDepthWrite(mesh, depthWrite))

  override def getMeshPosition(mesh: Int): (Double, Double, Double) =
    _meshPositions.getOrElse(mesh, (0.0, 0.0, 0.0))

  override def getMeshRotation(mesh: Int): (Double, Double, Double) =
    _meshRotations.getOrElse(mesh, (0.0, 0.0, 0.0))

  override def disposeMesh(mesh: Int): Unit = log(SceneOp.DisposeMesh(mesh))

  override def createCamera(fov: Double, aspect: Double, near: Double, far: Double): Int = {
    val id = _nextCameraId; _nextCameraId += 1
    log(SceneOp.CreateCamera(fov, aspect, near, far))
    id
  }

  override def setCameraPosition(cam: Int, x: Double, y: Double, z: Double): Unit = {
    _cameraPositions += (cam -> (x, y, z))
    log(SceneOp.SetCameraPosition(cam, x, y, z))
  }

  override def setCameraUp(cam: Int, x: Double, y: Double, z: Double): Unit = {
    _cameraUps += (cam -> (x, y, z))
    log(SceneOp.SetCameraUp(cam, x, y, z))
  }

  override def getCameraPosition(cam: Int): (Double, Double, Double) =
    _cameraPositions.getOrElse(cam, (0.0, 0.0, 0.0))

  override def getCameraUp(cam: Int): (Double, Double, Double) =
    _cameraUps.getOrElse(cam, (0.0, 1.0, 0.0))

  override def setCameraLookAt(cam: Int, x: Double, y: Double, z: Double): Unit =
    log(SceneOp.SetCameraLookAt(cam, x, y, z))

  override def setCameraFov(cam: Int, fov: Double): Unit =
    log(SceneOp.SetCameraFov(cam, fov))

  override def updateCameraProjection(cam: Int): Unit =
    log(SceneOp.UpdateCameraProjection(cam))

  override def setBackgroundColor(color: String): Unit =
    log(SceneOp.SetBackgroundColor(color))

  override def render(camera: Int): Unit =
    log(SceneOp.Render(camera))

  override def viewportWidth: Int = _viewportWidth
  override def viewportHeight: Int = _viewportHeight
  override def centerX: Double = _viewportWidth / 2.0
  override def centerY: Double = _viewportHeight / 2.0

  /** Clears the operation log. */
  def clearOps(): Unit = ops = Nil
}
