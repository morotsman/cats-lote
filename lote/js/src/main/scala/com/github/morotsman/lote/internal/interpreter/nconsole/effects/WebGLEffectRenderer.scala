package com.github.morotsman.lote.internal.interpreter.nconsole.effects

import com.github.morotsman.lote.api.RenderEffect
import com.github.morotsman.lote.internal.interpreter.nconsole.{AnsiParser, EffectMath}
import com.github.morotsman.lote.internal.interpreter.nconsole.camera.CameraAnimator
import com.github.morotsman.lote.internal.interpreter.nconsole.facade._
import com.github.morotsman.lote.internal.interpreter.nconsole.scene.WebGLScene
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}

/** Handles all `RenderEffect` rendering for the Three.js terminal backend.
  *
  * Each effect type (dissolve, smoke particles, glow, fade) is implemented as a method that manipulates the active
  * layer mesh in the Three.js scene.
  *
  * New effects can be added here without touching `ThreeJsTerminal` or any other file.
  *
  * ==Performance note==
  * Mutable `var` fields are used for particle lists and effect state rather than `cats.effect.Ref` because the `smoke`
  * and `dissolve` methods are called at up to 60 fps during effect animations. Direct field mutation avoids allocating
  * `IO` thunks and case-class copies on every frame. This is safe on single-threaded JS.
  *
  * '''Particle spawning''' (`spawnSmokeParticles` / `spawnDissolveParticles`) is intentionally expensive: it creates a
  * `Canvas`, `CanvasTexture`, `PlaneGeometry`, and `Mesh` per visible character. This runs once when an effect starts
  * (not per frame), so the up-front cost is acceptable for the visual result. However, slides with very dense text
  * (e.g. full 80×25 grids) will allocate thousands of GPU objects — keep this in mind for low-end devices.
  */
private[nconsole] class WebGLEffectRenderer(
    glScene: WebGLScene,
    activeOffscreen: () => HTMLCanvasElement,
    activeFrame: () => Vector[String],
    cameraAnimator: CameraAnimator,
    activeLayerMesh: () => Option[ThreeMesh]
) {

  // ---- Smoke particle state ----
  private var smokeParticles: List[ThreeMesh] = Nil
  private var smokeActive: Boolean = false
  private var hiddenLayerMesh: Option[ThreeMesh] = None

  // ---- Dissolve particle state ----
  private var dissolveParticles: List[ThreeMesh] = Nil
  private var dissolveActive: Boolean = false
  private var dissolveHiddenLayerMesh: Option[ThreeMesh] = None

  /** Apply a render effect. Called synchronously from `Sync[F].delay`. */
  def apply(effect: RenderEffect): Unit = effect match {
    case RenderEffect.Dissolve(progress)     => dissolve(progress)
    case RenderEffect.Smoke(progress)        => smoke(progress)
    case RenderEffect.Glow(color, intensity) => glow(color, intensity)
    case RenderEffect.Fade(opacity)          => fade(opacity)
    case RenderEffect.MoveCameraTo(target)   => cameraAnimator.moveTo(target)
    case RenderEffect.JumpCameraTo(target)   => cameraAnimator.jumpTo(target)
    case RenderEffect.ClearEffects           => clearEffects()
    case _                                   => // InitSpatialLayout and ActivateLayer handled by ThreeJsTerminal
  }

  /** Clean up any active particle effects and dispose their resources. */
  def cleanup(): Unit = {
    cleanupSmokeParticles()
    cleanupDissolveParticles()
  }

  // ---- Effect implementations ----

  /** Helper to get the active layer's mesh and material as typed references. */
  private def withActiveMesh(f: (ThreeMesh, ThreeMeshBasicMaterial) => Unit): Unit = {
    activeLayerMesh().foreach { mesh =>
      f(mesh, mesh.material)
    }
  }

  private def dissolve(progress: Double): Unit = {
    if (!dissolveActive) {
      dissolveActive = true
      spawnDissolveParticles()
      // Hide the source mesh (slide layer)
      activeLayerMesh().foreach { layerMesh =>
        dissolveHiddenLayerMesh = Some(layerMesh)
        layerMesh.material.opacity = 0.0
        layerMesh.material.needsUpdate = true
      }
    }

    dissolveParticles.foreach { p =>
      val ud = p.userData.asInstanceOf[DissolveParticleData]
      p.material.opacity = EffectMath.dissolveOpacity(progress, ud.fadeDelay, ud.fadeSpeed)
    }
    glScene.render()
  }

  private def smoke(progress: Double): Unit = {
    if (!smokeActive) {
      smokeActive = true
      spawnSmokeParticles()
      // Hide the source mesh (slide layer)
      activeLayerMesh().foreach { layerMesh =>
        hiddenLayerMesh = Some(layerMesh)
        layerMesh.material.opacity = 0.0
        layerMesh.material.needsUpdate = true
      }
    }

    // Per-frame iteration over all particles — the list is traversed linearly on every
    // requestAnimationFrame tick. For typical slide content (a few hundred particles) this
    // is fast enough. The particles are stored as a List (prepend-only) which avoids
    // array-copy overhead during spawning at the cost of sequential traversal here.
    smokeParticles.foreach { p =>
      val ud = p.userData.asInstanceOf[SmokeParticleData]
      val frame = EffectMath.smokeFrame(
        progress,
        ud.startX,
        ud.startY,
        ud.driftX,
        ud.driftY,
        ud.rotSpeed,
        ud.fadeDelay,
        ud.shrinkRate
      )
      p.position.x = frame.x
      p.position.y = frame.y
      p.rotation.z = frame.rotZ
      p.material.opacity = frame.opacity
      p.scale.set(frame.scale, frame.scale, 1.0)
    }
    glScene.render()
  }

  private def glow(color: String, intensity: Double): Unit = {
    val (glowR, glowG, glowB) = EffectMath.glowColor(color, intensity)
    glScene.scene.background = new ThreeColor(s"rgb($glowR, $glowG, $glowB)")
    glScene.render()
  }

  private def fade(opacity: Double): Unit = {
    withActiveMesh { (_, mat) =>
      mat.opacity = EffectMath.clampOpacity(opacity)
      mat.needsUpdate = true
    }
    glScene.render()
  }

  private def clearEffects(): Unit = {
    cleanupSmokeParticles()
    cleanupDissolveParticles()
    withActiveMesh { (mesh, mat) =>
      mesh.scale.set(1.0, 1.0, 1.0)
      mat.opacity = 1.0
      mat.needsUpdate = true
    }
    glScene.scene.background = new ThreeColor("#000000")
    glScene.render()
  }

  // ---- Smoke particle management ----

  private def cleanupSmokeParticles(): Unit = {
    smokeParticles.foreach { p =>
      glScene.scene.remove(p)
      p.geometry.dispose()
      p.material.map.asInstanceOf[ThreeCanvasTexture].dispose()
      p.material.dispose()
    }
    smokeParticles = Nil
    smokeActive = false
    // Restore hidden layer mesh opacity
    hiddenLayerMesh.foreach { lm =>
      lm.material.opacity = 1.0
      lm.material.needsUpdate = true
    }
    hiddenLayerMesh = None
  }

  private def spawnSmokeParticles(): Unit = {
    val h = glScene.viewportHeight
    val cw = glScene.cellWidth
    val ch = glScene.cellHeight

    // Determine world-space offset for particles
    val (offsetX, offsetY, offsetZ) = activeLayerMesh() match {
      case Some(layerMesh) =>
        val pos = layerMesh.position
        val ox = pos.x - glScene.viewportWidth / 2.0
        val oy = pos.y - glScene.viewportHeight / 2.0
        val oz = pos.z + 0.1
        (ox, oy, oz)
      case None =>
        (0.0, 0.0, 0.1)
    }

    activeFrame().zipWithIndex.foreach { case (line, row) =>
      val styledChars = AnsiParser.parseLine(line)
      styledChars.zipWithIndex.foreach { case (sc, col) =>
        if (sc.char != ' ' && sc.char != '\n') {
          val dpr = dom.window.devicePixelRatio.max(1.0)
          val charCanvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
          charCanvas.width = (cw * dpr).toInt
          charCanvas.height = (ch * dpr).toInt
          val charCtx = charCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
          charCtx.drawImage(
            activeOffscreen(),
            (col * cw * dpr).toInt,
            (row * ch * dpr).toInt,
            (cw * dpr).toInt,
            (ch * dpr).toInt,
            0,
            0,
            (cw * dpr).toInt,
            (ch * dpr).toInt
          )

          val charTex = new ThreeCanvasTexture(charCanvas)
          charTex.minFilter = ThreeLinearFilter
          charTex.magFilter = ThreeLinearFilter
          val charMat = new ThreeMeshBasicMaterial(
            ThreeMaterialOptions(map = charTex, transparent = true, opacity = 1.0)
          )
          val charGeo = new ThreePlaneGeometry(cw, ch)
          val charMesh = new ThreeMesh(charGeo, charMat)

          // Position in world space: local position + offset
          val localX = col * cw + cw / 2.0
          val localY = h - (row * ch + ch / 2.0)
          val sceneX = localX + offsetX
          val sceneY = localY + offsetY
          charMesh.position.set(sceneX, sceneY, offsetZ)

          charMesh.userData = SmokeParticleData(
            startX = sceneX,
            startY = sceneY,
            driftX = (Math.random() - 0.5) * 120.0,
            driftY = Math.random() * 180.0 + 60.0,
            rotSpeed = (Math.random() - 0.5) * Math.PI * 3.0,
            fadeDelay = Math.random() * 0.25,
            shrinkRate = 0.3 + Math.random() * 0.4
          )

          glScene.scene.add(charMesh)
          smokeParticles = charMesh :: smokeParticles
        }
      }
    }
  }

  // ---- Dissolve particle management ----

  private def cleanupDissolveParticles(): Unit = {
    dissolveParticles.foreach { p =>
      glScene.scene.remove(p)
      p.geometry.dispose()
      p.material.map.asInstanceOf[ThreeCanvasTexture].dispose()
      p.material.dispose()
    }
    dissolveParticles = Nil
    dissolveActive = false
    dissolveHiddenLayerMesh.foreach { lm =>
      lm.material.opacity = 1.0
      lm.material.needsUpdate = true
    }
    dissolveHiddenLayerMesh = None
  }

  private def spawnDissolveParticles(): Unit = {
    val h = glScene.viewportHeight
    val cw = glScene.cellWidth
    val ch = glScene.cellHeight

    val (offsetX, offsetY, offsetZ) = activeLayerMesh() match {
      case Some(layerMesh) =>
        val pos = layerMesh.position
        val ox = pos.x - glScene.viewportWidth / 2.0
        val oy = pos.y - glScene.viewportHeight / 2.0
        val oz = pos.z + 0.1
        (ox, oy, oz)
      case None =>
        (0.0, 0.0, 0.1)
    }

    activeFrame().zipWithIndex.foreach { case (line, row) =>
      val styledChars = AnsiParser.parseLine(line)
      styledChars.zipWithIndex.foreach { case (sc, col) =>
        if (sc.char != ' ' && sc.char != '\n') {
          val dpr = dom.window.devicePixelRatio.max(1.0)
          val charCanvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
          charCanvas.width = (cw * dpr).toInt
          charCanvas.height = (ch * dpr).toInt
          val charCtx = charCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
          charCtx.drawImage(
            activeOffscreen(),
            (col * cw * dpr).toInt,
            (row * ch * dpr).toInt,
            (cw * dpr).toInt,
            (ch * dpr).toInt,
            0,
            0,
            (cw * dpr).toInt,
            (ch * dpr).toInt
          )

          val charTex = new ThreeCanvasTexture(charCanvas)
          charTex.minFilter = ThreeLinearFilter
          charTex.magFilter = ThreeLinearFilter
          val charMat = new ThreeMeshBasicMaterial(
            ThreeMaterialOptions(map = charTex, transparent = true, opacity = 1.0)
          )
          val charGeo = new ThreePlaneGeometry(cw, ch)
          val charMesh = new ThreeMesh(charGeo, charMat)

          val localX = col * cw + cw / 2.0
          val localY = h - (row * ch + ch / 2.0)
          val sceneX = localX + offsetX
          val sceneY = localY + offsetY
          charMesh.position.set(sceneX, sceneY, offsetZ)

          charMesh.userData = DissolveParticleData(
            fadeDelay = Math.random() * 0.6,
            fadeSpeed = 1.2 + Math.random() * 0.8
          )

          glScene.scene.add(charMesh)
          dissolveParticles = charMesh :: dissolveParticles
        }
      }
    }
  }
}
