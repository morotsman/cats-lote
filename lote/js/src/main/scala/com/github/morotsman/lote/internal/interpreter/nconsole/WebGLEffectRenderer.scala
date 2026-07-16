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
    offscreen: HTMLCanvasElement,
    currentFrame: () => Vector[String]
) {

  private val meshDyn = glScene.plane.asInstanceOf[scalajs.js.Dynamic]
  private val matDyn  = glScene.planeMat.asInstanceOf[scalajs.js.Dynamic]

  // ---- Smoke particle state ----
  private var smokeParticles: List[scalajs.js.Dynamic] = Nil
  private var smokeActive: Boolean = false

  /** Apply a render effect. Called synchronously from `Sync[F].delay`. */
  def apply(effect: RenderEffect): Unit = effect match {
    case RenderEffect.FlipHorizontal(progress)  => flipHorizontal(progress)
    case RenderEffect.FlipVertical(progress)     => flipVertical(progress)
    case RenderEffect.Dissolve(progress)         => dissolve(progress)
    case RenderEffect.Smoke(progress)            => smoke(progress)
    case RenderEffect.Glow(color, intensity)     => glow(color, intensity)
    case RenderEffect.Fade(opacity)              => fade(opacity)
    case RenderEffect.Rotate(progress)           => rotate(progress)
    case RenderEffect.ClearEffects               => clearEffects()
  }

  /** Clean up any active particle effects and dispose their resources. */
  def cleanup(): Unit = cleanupSmokeParticles()

  // ---- Effect implementations ----

  private def flipHorizontal(progress: Double): Unit = {
    val scaleY = if (progress <= 0.5)
      Math.max(0.01, 1.0 - progress / 0.5)
    else
      Math.max(0.01, (progress - 0.5) / 0.5)
    val scaleX = 0.85 + 0.15 * scaleY
    meshDyn.scale.set(scaleX, scaleY, 1.0)
    meshDyn.position.set(glScene.centerX, glScene.centerY, 0)
    glScene.render()
  }

  private def flipVertical(progress: Double): Unit = {
    val scaleX = if (progress <= 0.5)
      Math.max(0.01, 1.0 - progress / 0.5)
    else
      Math.max(0.01, (progress - 0.5) / 0.5)
    val scaleY = 0.85 + 0.15 * scaleX
    meshDyn.scale.set(scaleX, scaleY, 1.0)
    meshDyn.position.set(glScene.centerX, glScene.centerY, 0)
    glScene.render()
  }

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
      matDyn.opacity = 0.0
      matDyn.needsUpdate = true
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

  private def rotate(progress: Double): Unit = {
    val angle = progress * Math.PI
    val cosA  = Math.cos(angle)
    val sinA  = Math.sin(angle)
    val scaleX = Math.max(0.01, Math.abs(cosA))
    meshDyn.scale.set(scaleX, 1.0, 1.0)
    val shift = sinA * glScene.viewportWidth * 0.15
    meshDyn.position.set(glScene.centerX + shift, glScene.centerY, 0)
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
  }

  private def spawnSmokeParticles(): Unit = {
    val h = glScene.viewportHeight
    val cw = glScene.cellWidth
    val ch = glScene.cellHeight

    currentFrame().zipWithIndex.foreach { case (line, row) =>
      val styledChars = AnsiParser.parseLine(line)
      styledChars.zipWithIndex.foreach { case (sc, col) =>
        if (sc.char != ' ' && sc.char != '\n') {
          val charCanvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
          charCanvas.width = cw
          charCanvas.height = ch
          val charCtx = charCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
          charCtx.drawImage(offscreen, col * cw, row * ch, cw, ch, 0, 0, cw, ch)

          val charTex = new ThreeCanvasTexture(charCanvas)
          charTex.minFilter = ThreeLinearFilter
          charTex.magFilter = ThreeLinearFilter
          val charMatOpts = scalajs.js.Dynamic.literal(map = charTex, transparent = true, opacity = 1.0)
          val charMat = new ThreeMeshBasicMaterial(charMatOpts.asInstanceOf[scalajs.js.UndefOr[scalajs.js.Object]])
          val charGeo = new ThreePlaneGeometry(cw, ch)
          val charMesh = new ThreeMesh(charGeo, charMat)

          val sceneX = col * cw + cw / 2.0
          val sceneY = h - (row * ch + ch / 2.0)
          charMesh.position.set(sceneX, sceneY, 0.1)

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

