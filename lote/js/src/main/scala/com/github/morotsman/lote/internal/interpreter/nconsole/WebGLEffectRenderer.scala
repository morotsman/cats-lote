package com.github.morotsman.lote.internal.interpreter.nconsole

import com.github.morotsman.lote.api.RenderEffect
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}

/** Handles all `RenderEffect` rendering for the Three.js terminal backend.
  *
  * Each effect type (flip, rotate, dissolve, smoke particles, glow, fade) is implemented
  * as a method that manipulates the Three.js scene owned by the `WebGLScene`.
  *
  * New effects can be added here without touching `ThreeJsTerminal` or any other file.
  */
private[nconsole] class WebGLEffectRenderer(
    glScene: WebGLScene,
    activeOffscreen: () => HTMLCanvasElement,
    activeFrame: () => Vector[String],
    cameraAnimator: CameraAnimator,
    activeLayerMesh: () => Option[scalajs.js.Dynamic]
) {

  private val meshDyn = glScene.plane.asInstanceOf[scalajs.js.Dynamic]
  private val matDyn  = glScene.planeMat.asInstanceOf[scalajs.js.Dynamic]

  // ---- Smoke particle state ----
  private var smokeParticles: List[scalajs.js.Dynamic] = Nil
  private var smokeActive: Boolean = false
  private var hiddenLayerMesh: Option[scalajs.js.Dynamic] = None

  /** Apply a render effect. Called synchronously from `Sync[F].delay`. */
  def apply(effect: RenderEffect): Unit = effect match {
    case RenderEffect.Dissolve(progress)         => dissolve(progress)
    case RenderEffect.Smoke(progress)            => smoke(progress)
    case RenderEffect.Glow(color, intensity)     => glow(color, intensity)
    case RenderEffect.Fade(opacity)              => fade(opacity)
    case RenderEffect.MoveCameraTo(target)       => cameraAnimator.moveTo(target)
    case RenderEffect.ClearEffects               => clearEffects()
    case _                                       => // InitSpatialLayout and ActivateLayer handled by ThreeJsTerminal
  }

  /** Clean up any active particle effects and dispose their resources. */
  def cleanup(): Unit = cleanupSmokeParticles()

  // ---- Effect implementations ----


  private def dissolve(progress: Double): Unit = {
    val opacity = Math.max(0.0, 1.0 - progress)
    matDyn.opacity = opacity
    matDyn.needsUpdate = true
    val scale = 1.0 - progress * 0.05
    meshDyn.scale.set(scale, scale, 1.0)
    glScene.render()
  }

  private def smoke(progress: Double): Unit = {
    if (!smokeActive) {
      smokeActive = true
      spawnSmokeParticles()
      // Hide the source mesh (slide layer in spatial mode, main plane otherwise)
      activeLayerMesh() match {
        case Some(layerMesh) =>
          hiddenLayerMesh = Some(layerMesh)
          layerMesh.material.opacity = 0.0
          layerMesh.material.needsUpdate = true
        case None =>
          matDyn.opacity = 0.0
          matDyn.needsUpdate = true
      }
    }

    smokeParticles.foreach { p =>
      val ud        = p.userData
      val startX    = ud.startX.asInstanceOf[Double]
      val startY    = ud.startY.asInstanceOf[Double]
      val driftX    = ud.driftX.asInstanceOf[Double]
      val driftY    = ud.driftY.asInstanceOf[Double]
      val rotSpeed  = ud.rotSpeed.asInstanceOf[Double]
      val fadeDelay = ud.fadeDelay.asInstanceOf[Double]
      val shrinkRate = ud.shrinkRate.asInstanceOf[Double]

      val t     = Math.max(0.0, (progress - fadeDelay) / (1.0 - fadeDelay))
      val eased = 1.0 - Math.pow(1.0 - t, 2.0)

      p.position.x = startX + driftX * eased
      p.position.y = startY + driftY * eased
      p.rotation.z = rotSpeed * eased
      p.material.opacity = Math.max(0.0, 1.0 - t * 1.3)
      val s = Math.max(0.0, 1.0 - t * shrinkRate)
      p.scale.set(s, s, 1.0)
    }
    glScene.render()
  }

  private def glow(color: String, intensity: Double): Unit = {
    val r = java.lang.Integer.parseInt(color.stripPrefix("#").substring(0, 2), 16)
    val g = java.lang.Integer.parseInt(color.stripPrefix("#").substring(2, 4), 16)
    val b = java.lang.Integer.parseInt(color.stripPrefix("#").substring(4, 6), 16)
    val glowR = (r * intensity / 255.0).min(1.0)
    val glowG = (g * intensity / 255.0).min(1.0)
    val glowB = (b * intensity / 255.0).min(1.0)
    glScene.scene.background = new ThreeColor(s"rgb($glowR, $glowG, $glowB)")
    glScene.render()
  }

  private def fade(opacity: Double): Unit = {
    matDyn.opacity = Math.max(0.0, Math.min(1.0, opacity))
    matDyn.needsUpdate = true
    glScene.render()
  }

  private def clearEffects(): Unit = {
    cleanupSmokeParticles()
    meshDyn.rotation.x = 0.0
    meshDyn.rotation.y = 0.0
    meshDyn.rotation.z = 0.0
    meshDyn.scale.set(1.0, 1.0, 1.0)
    meshDyn.position.set(glScene.centerX, glScene.centerY, 0)
    matDyn.opacity = 1.0
    matDyn.needsUpdate = true
    glScene.scene.background = new ThreeColor("#000000")
    glScene.render()
  }

  // ---- Smoke particle management ----

  private def cleanupSmokeParticles(): Unit = {
    smokeParticles.foreach { p =>
      glScene.scene.remove(p.asInstanceOf[scalajs.js.Any])
      p.geometry.dispose()
      p.material.map.dispose()
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

    // Determine world-space offset for particles (needed in spatial mode)
    val (offsetX, offsetY, offsetZ) = activeLayerMesh() match {
      case Some(layerMesh) =>
        val pos = layerMesh.position
        // Mesh center is at (worldX + w/2, worldY + h/2, worldZ)
        // Particle coords are relative to bottom-left of the viewport area
        // so offset = meshCenter - (viewportWidth/2, viewportHeight/2, 0)
        val ox = pos.x.asInstanceOf[Double] - glScene.viewportWidth / 2.0
        val oy = pos.y.asInstanceOf[Double] - glScene.viewportHeight / 2.0
        val oz = pos.z.asInstanceOf[Double] + 0.1
        (ox, oy, oz)
      case None =>
        (0.0, 0.0, 0.1)
    }

    activeFrame().zipWithIndex.foreach { case (line, row) =>
      val styledChars = AnsiParser.parseLine(line)
      styledChars.zipWithIndex.foreach { case (sc, col) =>
        if (sc.char != ' ' && sc.char != '\n') {
          val charCanvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
          charCanvas.width = cw
          charCanvas.height = ch
          val charCtx = charCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
          charCtx.drawImage(activeOffscreen(), col * cw, row * ch, cw, ch, 0, 0, cw, ch)

          val charTex = new ThreeCanvasTexture(charCanvas)
          charTex.minFilter = ThreeLinearFilter
          charTex.magFilter = ThreeLinearFilter
          val charMatOpts = scalajs.js.Dynamic.literal(map = charTex, transparent = true, opacity = 1.0)
          val charMat = new ThreeMeshBasicMaterial(charMatOpts.asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]])
          val charGeo = new ThreePlaneGeometry(cw, ch)
          val charMesh = new ThreeMesh(charGeo, charMat)

          // Position in world space: local position + offset
          val localX = col * cw + cw / 2.0
          val localY = h - (row * ch + ch / 2.0)
          val sceneX = localX + offsetX
          val sceneY = localY + offsetY
          charMesh.position.set(sceneX, sceneY, offsetZ)

          val p = charMesh.asInstanceOf[scalajs.js.Dynamic]
          p.userData = scalajs.js.Dynamic.literal(
            startX = sceneX,
            startY = sceneY,
            driftX = (Math.random() - 0.5) * 120.0,
            driftY = Math.random() * 180.0 + 60.0,
            rotSpeed = (Math.random() - 0.5) * Math.PI * 3.0,
            fadeDelay = Math.random() * 0.25,
            shrinkRate = 0.3 + Math.random() * 0.4
          )

          glScene.scene.add(charMesh)
          smokeParticles = p :: smokeParticles
        }
      }
    }
  }
}
