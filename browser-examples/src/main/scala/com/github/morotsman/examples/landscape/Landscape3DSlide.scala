package com.github.morotsman.examples.landscape

import cats.Monad
import cats.effect.{Async, Ref, Sync}
import cats.implicits._
import com.github.morotsman.lote.api.{
  Alignment,
  AnimationSettings,
  HorizontalAlignment,
  ScreenAdjusted,
  UserInput,
  VerticalAlignment
}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{Clock, FixedStep}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker, TickerSubscription}
import org.scalajs.dom
import org.scalajs.dom.HTMLElement

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

/** A non-ASCII 3D fairy-tale landscape slide rendered with Three.js / WebGL.
  *
  * Same idea as `LandscapeSlide` (rolling hills, castles, trees, roaming figures) but rendered as actual 3D geometry
  * with realistic fairy-tale appearance.
  *
  * Controls: A/D to rotate camera left/right, W/S to zoom in/out.
  */
object Landscape3DSlide {

  def contextual[F[_]: Async: Ref.Make](): ContextualF[F, Slide[F]] =
    ContextualF { ctx =>
      Landscape3DAnimator
        .create[F](ctx.console, ctx.ticker, ctx.animationSettings)
        .map { animator =>
          new Slide[F] {
            override def content: F[ScreenAdjusted] =
              ctx.console.alignText("", Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))

            override def startShow: F[Unit] = animator.start()
            override def stopShow: F[Unit] = animator.stop()

            override def userInput(input: UserInput): F[Unit] =
              animator.handleInput(input)
          }
        }
    }
}

// ---- 3D Animator ----

private[landscape] trait Landscape3DAnimator[F[_]] {
  def start(): F[Unit]
  def stop(): F[Unit]
  def handleInput(input: UserInput): F[Unit]
}

private[landscape] object Landscape3DAnimator {

  def create[F[_]: Async: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: Clock[F]): F[Landscape3DAnimator[F]] = {

    for {
      sceneRef <- Ref[F].of(Option.empty[js.Dynamic])
      cameraAngleRef <- Ref[F].of(0.0)
      cameraDistRef <- Ref[F].of(35.0)
      rotSpeedRef <- Ref[F].of(0.0)
      zoomSpeedRef <- Ref[F].of(0.0)
      subRef <- Ref[F].of(Option.empty[TickerSubscription[F]])
      stepperRef <- FixedStep.makeRef[F]
    } yield new Landscape3DAnimator[F] {

      private def buildScene(): F[js.Dynamic] = Sync[F].delay {
        val THREE = g.THREE

        // ---- Renderer ----
        val container = dom.document.getElementById("terminal").asInstanceOf[HTMLElement]
        val renderer = js.Dynamic.newInstance(THREE.WebGLRenderer)(
          js.Dynamic.literal(antialias = true, alpha = false)
        )
        renderer.setSize(container.clientWidth, container.clientHeight)
        renderer.setPixelRatio(dom.window.devicePixelRatio)
        renderer.shadowMap.enabled = true
        renderer.shadowMap.`type` = THREE.PCFSoftShadowMap
        renderer.domElement.style.position = "absolute"
        renderer.domElement.style.top = "0"
        renderer.domElement.style.left = "0"
        renderer.domElement.style.zIndex = "1000"
        container.appendChild(renderer.domElement.asInstanceOf[dom.Node])

        // ---- Scene ----
        val scene = js.Dynamic.newInstance(THREE.Scene)()
        scene.background = js.Dynamic.newInstance(THREE.Color)("#1a0a2e")

        // Fog for depth/fairy-tale atmosphere
        scene.fog = js.Dynamic.newInstance(THREE.FogExp2)("#1a0a2e", 0.012)

        // ---- Camera ----
        val aspect = container.clientWidth.toDouble / container.clientHeight.toDouble
        val camera = js.Dynamic.newInstance(THREE.PerspectiveCamera)(60, aspect, 0.1, 200)
        camera.position.set(0, 12, 35)
        camera.lookAt(0, 3, 0)

        // ---- Lighting (warm fairy-tale) ----
        val ambientLight = js.Dynamic.newInstance(THREE.AmbientLight)("#2d1b69", 0.4)
        scene.add(ambientLight)

        val moonLight = js.Dynamic.newInstance(THREE.DirectionalLight)("#aabbff", 0.8)
        moonLight.position.set(-20, 30, 10)
        moonLight.castShadow = true
        moonLight.shadow.mapSize.width = 2048
        moonLight.shadow.mapSize.height = 2048
        scene.add(moonLight)

        val warmLight = js.Dynamic.newInstance(THREE.PointLight)("#ffaa44", 0.6, 60)
        warmLight.position.set(5, 8, 5)
        scene.add(warmLight)

        // Castle glow
        val castleGlow = js.Dynamic.newInstance(THREE.PointLight)("#ffdd88", 1.0, 25)
        castleGlow.position.set(-8, 6, -5)
        scene.add(castleGlow)

        // ---- Ground / Terrain ----
        val terrainGeo = js.Dynamic.newInstance(THREE.PlaneGeometry)(120, 120, 80, 80)
        terrainGeo.rotateX(-Math.PI / 2)

        // Displace vertices for rolling hills
        val positions = terrainGeo.attributes.position
        val count = positions.count.asInstanceOf[Int]
        for (i <- 0 until count) {
          val x = positions.getX(i).asInstanceOf[Double]
          val z = positions.getZ(i).asInstanceOf[Double]
          val height =
            2.5 * Math.sin(x * 0.08) * Math.cos(z * 0.06) +
              1.5 * Math.sin(x * 0.15 + 1.0) * Math.sin(z * 0.12 + 0.5) +
              0.8 * Math.sin(x * 0.25 + 2.0) * Math.cos(z * 0.2 + 1.0) +
              0.4 * Math.sin(x * 0.4 + 3.0)
          positions.setY(i, height)
        }
        terrainGeo.computeVertexNormals()

        val terrainMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
          js.Dynamic.literal(color = "#1a5c1a", flatShading = false)
        )
        val terrain = js.Dynamic.newInstance(THREE.Mesh)(terrainGeo, terrainMat)
        terrain.receiveShadow = true
        scene.add(terrain)

        // ---- Path (winding dirt path) ----
        val pathGeo = js.Dynamic.newInstance(THREE.PlaneGeometry)(3, 80, 1, 40)
        pathGeo.rotateX(-Math.PI / 2)
        val pathPositions = pathGeo.attributes.position
        val pathCount = pathPositions.count.asInstanceOf[Int]
        for (i <- 0 until pathCount) {
          val z = pathPositions.getZ(i).asInstanceOf[Double]
          val x = pathPositions.getX(i).asInstanceOf[Double]
          val waveX = 4.0 * Math.sin(z * 0.1)
          val terrainY =
            2.5 * Math.sin((x + waveX) * 0.08) * Math.cos(z * 0.06) +
              1.5 * Math.sin((x + waveX) * 0.15 + 1.0) * Math.sin(z * 0.12 + 0.5) +
              0.8 * Math.sin((x + waveX) * 0.25 + 2.0) * Math.cos(z * 0.2 + 1.0)
          pathPositions.setX(i, x + waveX)
          pathPositions.setY(i, terrainY + 0.05)
        }
        pathGeo.computeVertexNormals()
        val pathMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
          js.Dynamic.literal(color = "#8B6914")
        )
        val path = js.Dynamic.newInstance(THREE.Mesh)(pathGeo, pathMat)
        scene.add(path)

        // ---- Water (small pond) ----
        val waterGeo = js.Dynamic.newInstance(THREE.CircleGeometry)(5, 32)
        waterGeo.rotateX(-Math.PI / 2)
        val waterMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
          js.Dynamic.literal(color = "#2255aa", transparent = true, opacity = 0.7)
        )
        val water = js.Dynamic.newInstance(THREE.Mesh)(waterGeo, waterMat)
        water.position.set(18, 0.1, 8)
        scene.add(water)

        // ---- Trees ----
        def makeTree(x: Double, z: Double, scale: Double, treeType: Int): Unit = {
          val terrainY =
            2.5 * Math.sin(x * 0.08) * Math.cos(z * 0.06) +
              1.5 * Math.sin(x * 0.15 + 1.0) * Math.sin(z * 0.12 + 0.5) +
              0.8 * Math.sin(x * 0.25 + 2.0) * Math.cos(z * 0.2 + 1.0)

          // Trunk
          val trunkGeo = js.Dynamic.newInstance(THREE.CylinderGeometry)(0.15 * scale, 0.25 * scale, 2.0 * scale, 8)
          val trunkMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
            js.Dynamic.literal(color = "#4a2800")
          )
          val trunk = js.Dynamic.newInstance(THREE.Mesh)(trunkGeo, trunkMat)
          trunk.position.set(x, terrainY + 1.0 * scale, z)
          trunk.castShadow = true
          scene.add(trunk)

          treeType match {
            case 0 => // Conifer (cone)
              val foliageGeo = js.Dynamic.newInstance(THREE.ConeGeometry)(1.2 * scale, 3.5 * scale, 8)
              val foliageMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
                js.Dynamic.literal(color = "#0d4d0d")
              )
              val foliage = js.Dynamic.newInstance(THREE.Mesh)(foliageGeo, foliageMat)
              foliage.position.set(x, terrainY + 3.5 * scale, z)
              foliage.castShadow = true
              scene.add(foliage)

            case 1 => // Rounded tree (sphere)
              val foliageGeo = js.Dynamic.newInstance(THREE.SphereGeometry)(1.5 * scale, 8, 6)
              val foliageMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
                js.Dynamic.literal(color = "#1a6b1a")
              )
              val foliage = js.Dynamic.newInstance(THREE.Mesh)(foliageGeo, foliageMat)
              foliage.position.set(x, terrainY + 3.0 * scale, z)
              foliage.castShadow = true
              scene.add(foliage)

            case _ => // Mushroom-shaped fairy-tale tree
              val foliageGeo = js.Dynamic.newInstance(THREE.SphereGeometry)(1.8 * scale, 10, 6)
              foliageGeo.scale(1.0, 0.6, 1.0)
              val foliageMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
                js.Dynamic.literal(color = "#2d8a2d")
              )
              val foliage = js.Dynamic.newInstance(THREE.Mesh)(foliageGeo, foliageMat)
              foliage.position.set(x, terrainY + 2.8 * scale, z)
              foliage.castShadow = true
              scene.add(foliage)
          }
        }

        // Place trees
        val treeRng = new scala.util.Random(42)
        for (_ <- 0 until 40) {
          val tx = treeRng.nextDouble() * 80 - 40
          val tz = treeRng.nextDouble() * 80 - 40
          // Avoid placing on the castle positions
          if (Math.abs(tx + 8) > 5 || Math.abs(tz + 5) > 5) {
            if (Math.abs(tx - 20) > 4 || Math.abs(tz - 15) > 4) {
              makeTree(tx, tz, 0.7 + treeRng.nextDouble() * 0.8, treeRng.nextInt(3))
            }
          }
        }

        // ---- Castle ----
        def makeCastle(cx: Double, cz: Double): Unit = {
          val terrainY =
            2.5 * Math.sin(cx * 0.08) * Math.cos(cz * 0.06) +
              1.5 * Math.sin(cx * 0.15 + 1.0) * Math.sin(cz * 0.12 + 0.5) +
              0.8 * Math.sin(cx * 0.25 + 2.0) * Math.cos(cz * 0.2 + 1.0)

          val stoneColor = "#8a7b6b"
          val roofColor = "#4a1a3a"

          // Main keep
          val keepGeo = js.Dynamic.newInstance(THREE.BoxGeometry)(4, 6, 4)
          val keepMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
            js.Dynamic.literal(color = stoneColor)
          )
          val keep = js.Dynamic.newInstance(THREE.Mesh)(keepGeo, keepMat)
          keep.position.set(cx, terrainY + 3, cz)
          keep.castShadow = true
          keep.receiveShadow = true
          scene.add(keep)

          // Roof
          val roofGeo = js.Dynamic.newInstance(THREE.ConeGeometry)(3.5, 3, 4)
          val roofMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
            js.Dynamic.literal(color = roofColor)
          )
          val roof = js.Dynamic.newInstance(THREE.Mesh)(roofGeo, roofMat)
          roof.position.set(cx, terrainY + 7.5, cz)
          roof.rotation.y = Math.PI / 4
          roof.castShadow = true
          scene.add(roof)

          // Towers (4 corners)
          val offsets = List((-2.5, -2.5), (2.5, -2.5), (-2.5, 2.5), (2.5, 2.5))
          offsets.foreach { case (ox, oz) =>
            val towerGeo = js.Dynamic.newInstance(THREE.CylinderGeometry)(0.8, 0.9, 7, 8)
            val towerMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
              js.Dynamic.literal(color = stoneColor)
            )
            val tower = js.Dynamic.newInstance(THREE.Mesh)(towerGeo, towerMat)
            tower.position.set(cx + ox, terrainY + 3.5, cz + oz)
            tower.castShadow = true
            scene.add(tower)

            // Tower cap
            val capGeo = js.Dynamic.newInstance(THREE.ConeGeometry)(1.1, 2, 8)
            val capMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
              js.Dynamic.literal(color = roofColor)
            )
            val cap = js.Dynamic.newInstance(THREE.Mesh)(capGeo, capMat)
            cap.position.set(cx + ox, terrainY + 8, cz + oz)
            cap.castShadow = true
            scene.add(cap)
          }

          // Castle windows (glowing)
          val windowGeo = js.Dynamic.newInstance(THREE.PlaneGeometry)(0.4, 0.6)
          val windowMat = js.Dynamic.newInstance(THREE.MeshBasicMaterial)(
            js.Dynamic.literal(color = "#ffdd66")
          )
          val windowPositions = List(
            (cx - 2.01, terrainY + 4.0, cz),
            (cx - 2.01, terrainY + 2.5, cz + 1),
            (cx - 2.01, terrainY + 2.5, cz - 1)
          )
          windowPositions.foreach { case (wx, wy, wz) =>
            val win = js.Dynamic.newInstance(THREE.Mesh)(windowGeo, windowMat)
            win.position.set(wx, wy, wz)
            win.rotation.y = Math.PI / 2
            scene.add(win)
          }
        }

        makeCastle(-8, -5)
        makeCastle(20, 15)

        // ---- Roaming figures (simple capsule-like shapes) ----
        val figureGroup = js.Dynamic.newInstance(THREE.Group)()
        scene.add(figureGroup)

        val figRng = new scala.util.Random(99)
        val figures = js.Array[js.Dynamic]()
        val figureSpeeds = js.Array[js.Dynamic]()
        for (_ <- 0 until 8) {
          val fx = figRng.nextDouble() * 40 - 20
          val fz = figRng.nextDouble() * 40 - 20
          val terrainY =
            2.5 * Math.sin(fx * 0.08) * Math.cos(fz * 0.06) +
              1.5 * Math.sin(fx * 0.15 + 1.0) * Math.sin(fz * 0.12 + 0.5) +
              0.8 * Math.sin(fx * 0.25 + 2.0) * Math.cos(fz * 0.2 + 1.0)

          // Body
          val bodyGeo = js.Dynamic.newInstance(THREE.CapsuleGeometry)(0.2, 0.6, 4, 8)
          val colors = js.Array("#cc3333", "#3333cc", "#33cc33", "#cc33cc", "#cccc33", "#33cccc", "#ff8833", "#8833ff")
          val bodyMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
            js.Dynamic.literal(color = colors(figRng.nextInt(colors.length)))
          )
          val body = js.Dynamic.newInstance(THREE.Mesh)(bodyGeo, bodyMat)
          body.position.set(fx, terrainY + 0.7, fz)
          body.castShadow = true
          figureGroup.add(body)
          figures.push(body)

          // Head
          val headGeo = js.Dynamic.newInstance(THREE.SphereGeometry)(0.2, 8, 6)
          val headMat = js.Dynamic.newInstance(THREE.MeshLambertMaterial)(
            js.Dynamic.literal(color = "#ffcc99")
          )
          val head = js.Dynamic.newInstance(THREE.Mesh)(headGeo, headMat)
          head.position.set(fx, terrainY + 1.3, fz)
          figureGroup.add(head)
          // Store speed info
          val speedX = (figRng.nextDouble() * 0.04 + 0.01) * (if (figRng.nextBoolean()) 1 else -1)
          val speedZ = (figRng.nextDouble() * 0.04 + 0.01) * (if (figRng.nextBoolean()) 1 else -1)
          figureSpeeds.push(
            js.Dynamic.literal(
              body = body,
              head = head,
              sx = speedX,
              sz = speedZ
            )
          )
        }

        // ---- Stars (particle system) ----
        val starCount = 200
        val starGeo = js.Dynamic.newInstance(THREE.BufferGeometry)()
        val starPositionsArr = new js.Array[Double]()
        val starRng = new scala.util.Random(7)
        for (_ <- 0 until starCount) {
          starPositionsArr.push(starRng.nextDouble() * 100 - 50)
          starPositionsArr.push(20 + starRng.nextDouble() * 30)
          starPositionsArr.push(starRng.nextDouble() * 100 - 50)
        }
        val starFloat32 = js.Dynamic.newInstance(g.Float32Array)(
          js.Array(starPositionsArr.toSeq: _*)
        )
        starGeo.setAttribute("position", js.Dynamic.newInstance(THREE.BufferAttribute)(starFloat32, 3))
        val starMat = js.Dynamic.newInstance(THREE.PointsMaterial)(
          js.Dynamic.literal(color = "#ffffff", size = 0.3, sizeAttenuation = true)
        )
        val stars = js.Dynamic.newInstance(THREE.Points)(starGeo, starMat)
        scene.add(stars)

        // ---- Moon ----
        val moonGeo = js.Dynamic.newInstance(THREE.SphereGeometry)(2.5, 16, 16)
        val moonMat = js.Dynamic.newInstance(THREE.MeshBasicMaterial)(
          js.Dynamic.literal(color = "#ffffdd")
        )
        val moon = js.Dynamic.newInstance(THREE.Mesh)(moonGeo, moonMat)
        moon.position.set(-25, 30, -30)
        scene.add(moon)

        // Moon glow
        val moonGlowGeo = js.Dynamic.newInstance(THREE.SphereGeometry)(3.5, 16, 16)
        val moonGlowMat = js.Dynamic.newInstance(THREE.MeshBasicMaterial)(
          js.Dynamic.literal(color = "#ffffcc", transparent = true, opacity = 0.2)
        )
        val moonGlow = js.Dynamic.newInstance(THREE.Mesh)(moonGlowGeo, moonGlowMat)
        moonGlow.position.set(-25, 30, -30)
        scene.add(moonGlow)

        // ---- Fireflies (small glowing particles near ground) ----
        val fireflyCount = 30
        val fireflyGeo = js.Dynamic.newInstance(THREE.BufferGeometry)()
        val fireflyPos = new js.Array[Double]()
        val ffRng = new scala.util.Random(555)
        for (_ <- 0 until fireflyCount) {
          fireflyPos.push(ffRng.nextDouble() * 50 - 25)
          fireflyPos.push(2 + ffRng.nextDouble() * 4)
          fireflyPos.push(ffRng.nextDouble() * 50 - 25)
        }
        val fireflyFloat32 = js.Dynamic.newInstance(g.Float32Array)(
          js.Array(fireflyPos.toSeq: _*)
        )
        fireflyGeo.setAttribute("position", js.Dynamic.newInstance(THREE.BufferAttribute)(fireflyFloat32, 3))
        val fireflyMat = js.Dynamic.newInstance(THREE.PointsMaterial)(
          js.Dynamic.literal(color = "#ffff44", size = 0.4, sizeAttenuation = true, transparent = true, opacity = 0.8)
        )
        val fireflies = js.Dynamic.newInstance(THREE.Points)(fireflyGeo, fireflyMat)
        scene.add(fireflies)

        // Return scene state for animation
        js.Dynamic.literal(
          renderer = renderer,
          scene = scene,
          camera = camera,
          container = container.asInstanceOf[js.Any],
          figures = figureSpeeds,
          fireflies = fireflies,
          stars = stars,
          water = water,
          tick = 0.asInstanceOf[js.Any]
        )
      }

      private def animate(state: js.Dynamic): F[Unit] = Sync[F].delay {
        val t = state.tick.asInstanceOf[Double] + 1
        state.tick = t.asInstanceOf[js.Any]

        // Move figures
        val figs = state.figures.asInstanceOf[js.Array[js.Dynamic]]
        for (i <- 0 until figs.length) {
          val info = figs(i)
          val body = info.body
          val head = info.head
          val sx = info.sx.asInstanceOf[Double]
          val sz = info.sz.asInstanceOf[Double]

          val nx = body.position.x.asInstanceOf[Double] + sx
          val nz = body.position.z.asInstanceOf[Double] + sz

          // Bounce at world edges
          val (finalX, finalSx) = if (nx > 30 || nx < -30) (-nx.sign * 29, -sx) else (nx, sx)
          val (finalZ, finalSz) = if (nz > 30 || nz < -30) (-nz.sign * 29, -sz) else (nz, sz)

          if (finalSx != sx) info.sx = finalSx.asInstanceOf[js.Any]
          if (finalSz != sz) info.sz = finalSz.asInstanceOf[js.Any]

          val terrainY =
            2.5 * Math.sin(finalX * 0.08) * Math.cos(finalZ * 0.06) +
              1.5 * Math.sin(finalX * 0.15 + 1.0) * Math.sin(finalZ * 0.12 + 0.5) +
              0.8 * Math.sin(finalX * 0.25 + 2.0) * Math.cos(finalZ * 0.2 + 1.0)

          body.position.set(finalX, terrainY + 0.7, finalZ)
          head.position.set(finalX, terrainY + 1.3, finalZ)
        }

        // Firefly flicker
        val ffOpacity = 0.4 + 0.5 * Math.sin(t * 0.08)
        state.fireflies.material.opacity = ffOpacity

        // Animate firefly positions slightly
        val ffPositions = state.fireflies.geometry.attributes.position
        val ffCount = ffPositions.count.asInstanceOf[Int]
        for (i <- 0 until ffCount) {
          val y = ffPositions.getY(i).asInstanceOf[Double]
          ffPositions.setY(i, y + 0.02 * Math.sin(t * 0.05 + i))
        }
        ffPositions.needsUpdate = true

        // Water shimmer
        val waterOpacity = 0.5 + 0.2 * Math.sin(t * 0.03)
        state.water.material.opacity = waterOpacity

        // Render
        state.renderer.render(state.scene, state.camera)
      }

      private def updateCamera(state: js.Dynamic, angle: Double, dist: Double): F[Unit] = Sync[F].delay {
        val camera = state.camera
        val x = Math.sin(angle) * dist
        val z = Math.cos(angle) * dist
        camera.position.set(x, 12, z)
        camera.lookAt(0, 3, 0)
      }

      private val tickerCallback: F[Unit] = for {
        nrOfSteps <- FixedStep.consumeSteps(stepperRef, animationSettings.step)
        _ <-
          if (nrOfSteps <= 0) Monad[F].unit
          else
            for {
              maybeScene <- sceneRef.get
              _ <- maybeScene.traverse_ { state =>
                for {
                  angle <- cameraAngleRef.get
                  dist <- cameraDistRef.get
                  rotSpeed <- rotSpeedRef.get
                  zoomSpeed <- zoomSpeedRef.get
                  newAngle = angle + rotSpeed * nrOfSteps
                  newDist = Math.max(15, Math.min(60, dist + zoomSpeed * nrOfSteps))
                  _ <- cameraAngleRef.set(newAngle)
                  _ <- cameraDistRef.set(newDist)
                  _ <- updateCamera(state, newAngle, newDist)
                  _ <- animate(state)
                } yield ()
              }
            } yield ()
      } yield ()

      override def start(): F[Unit] = for {
        state <- buildScene()
        _ <- sceneRef.set(Some(state))
        _ <- cameraAngleRef.set(0.0)
        _ <- cameraDistRef.set(35.0)
        _ <- rotSpeedRef.set(0.003) // Gentle auto-rotate
        _ <- zoomSpeedRef.set(0.0)
        _ <- FixedStep.reset(stepperRef)
        sub <- ticker.subscribe(tickerCallback)
        _ <- subRef.set(Some(sub))
        _ <- ticker.start
      } yield ()

      override def stop(): F[Unit] = for {
        maybeSub <- subRef.get
        _ <- maybeSub.traverse_(_.cancel)
        _ <- subRef.set(None)
        maybeScene <- sceneRef.get
        _ <- maybeScene.traverse_ { state =>
          Sync[F].delay {
            val container = state.container.asInstanceOf[HTMLElement]
            val rendererDom = state.renderer.domElement.asInstanceOf[dom.Node]
            state.renderer.dispose()
            if (container.contains(rendererDom)) {
              container.removeChild(rendererDom)
            }
          }
        }
        _ <- sceneRef.set(None)
      } yield ()

      override def handleInput(input: UserInput): F[Unit] = input match {
        case com.github.morotsman.lote.api.Character(c) if c == 'a' =>
          rotSpeedRef.set(-0.02)
        case com.github.morotsman.lote.api.Character(c) if c == 'd' =>
          rotSpeedRef.set(0.02)
        case com.github.morotsman.lote.api.Character(c) if c == 'w' =>
          zoomSpeedRef.set(-0.3)
        case com.github.morotsman.lote.api.Character(c) if c == 's' =>
          zoomSpeedRef.set(0.3)
        case com.github.morotsman.lote.api.Character(c) if c == 'q' =>
          rotSpeedRef.set(0.003) >> zoomSpeedRef.set(0.0) // Reset to gentle auto-rotate
        case _ =>
          Monad[F].unit
      }
    }
  }
}
