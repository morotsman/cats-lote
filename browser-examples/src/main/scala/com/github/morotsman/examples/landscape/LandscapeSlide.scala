package com.github.morotsman.examples.landscape

import cats.Monad
import cats.effect.{Ref, Temporal}
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

/** An interactive landscape slide with rolling hills, a castle, trees, and roaming figures.
  *
  * The scene scrolls horizontally. Figures wander on their own and the viewer can nudge the camera left/right with `a`
  * / `d`.
  */
object LandscapeSlide {

  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    ContextualF { ctx =>
      LandscapeAnimator
        .create[F](ctx.console, ctx.ticker, ctx.animationSettings)
        .map { animator =>
          new Slide[F] {
            override def content: F[ScreenAdjusted] =
              animator.lastFrame

            override def startShow: F[Unit] = animator.start()
            override def stopShow: F[Unit] = animator.stop()

            override def userInput(input: UserInput): F[Unit] =
              animator.handleInput(input)
          }
        }
    }
}

// ---- Landscape data model ----

private[landscape] case class Figure(
    worldX: Double, // position in world coordinates
    speed: Double, // cells per tick (negative = left)
    symbol: Char
)

private[landscape] case class LandscapeState(
    cameraX: Double, // left edge of viewport in world coordinates
    figures: Vector[Figure],
    tick: Long
)

// ---- Animator ----

private[landscape] trait LandscapeAnimator[F[_]] {
  def start(): F[Unit]
  def stop(): F[Unit]
  def handleInput(input: UserInput): F[Unit]
  def lastFrame: F[ScreenAdjusted]
}

private[landscape] object LandscapeAnimator {

  // World is wider than the screen — we scroll through it
  private val WorldWidth = 300

  // Terrain layers (generated once, indexed by world X)
  private val hillHeight: Array[Int] = {
    val arr = new Array[Int](WorldWidth)
    for (x <- arr.indices) {
      // Layered sine waves for natural rolling hills
      arr(x) = (
        3.0 * Math.sin(x * 0.03) +
          2.0 * Math.sin(x * 0.07 + 1.0) +
          1.5 * Math.sin(x * 0.13 + 2.5) +
          1.0 * Math.sin(x * 0.21 + 0.7)
      ).toInt
    }
    arr
  }

  // Static features placed in world coordinates
  private case class Tree(worldX: Int, style: Int)
  private case class Castle(worldX: Int)

  private val trees: Vector[Tree] = {
    val rng = new scala.util.Random(42)
    (0 until 35).map(_ => Tree(rng.nextInt(WorldWidth), rng.nextInt(3))).toVector
  }

  private val castles: Vector[Castle] = Vector(Castle(50), Castle(180))

  /** Safely set a character in the buffer if within bounds. */
  private def safeSet(buf: Array[Array[Char]], row: Int, col: Int, ch: Char, width: Int, height: Int): Unit = {
    if (row >= 0 && row < height && col >= 0 && col < width) {
      buf(row)(col) = ch
    }
  }

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: Clock[F]): F[LandscapeAnimator[F]] = {

    val rng = new scala.util.Random(123)
    val initialFigures = (0 until 12).map { i =>
      val symbols = "&#@%$"
      Figure(
        worldX = rng.nextInt(WorldWidth).toDouble,
        speed = (rng.nextDouble() * 0.6 + 0.1) * (if (rng.nextBoolean()) 1 else -1),
        symbol = symbols(i % symbols.length)
      )
    }.toVector

    for {
      stateRef <- Ref[F].of(Option.empty[LandscapeState])
      cameraSpeedRef <- Ref[F].of(0.0)
      subRef <- Ref[F].of(Option.empty[TickerSubscription[F]])
      stepperRef <- FixedStep.makeRef[F]
      lastFrameRef <- Ref[F].of(ScreenAdjusted(""))
    } yield new LandscapeAnimator[F] {

      private def renderWorld(state: LandscapeState, screenWidth: Int, screenHeight: Int): ScreenAdjusted = {
        if (screenWidth <= 0 || screenHeight <= 0) return ScreenAdjusted("")

        val camX = state.cameraX.toInt
        val groundRow = screenHeight - 6
        val w = screenWidth
        val h = screenHeight

        // Use a 2D char array for safe random-access writes
        val buf = Array.fill(h)(Array.fill(w)(' '))

        // Stars in the sky
        val starRng = new scala.util.Random(7)
        for (_ <- 0 until 40) {
          val sx = starRng.nextInt(WorldWidth)
          val sy = starRng.nextInt(Math.max(1, groundRow - 8))
          val screenX = ((sx - camX) % WorldWidth + WorldWidth) % WorldWidth
          safeSet(buf, sy, screenX, if (starRng.nextBoolean()) '.' else '.', w, h)
        }

        // Moon
        val moonX = ((20 - camX) % WorldWidth + WorldWidth) % WorldWidth
        safeSet(buf, 2, moonX, '(', w, h)
        safeSet(buf, 2, moonX + 1, ')', w, h)

        // Draw hills and ground
        for (screenX <- 0 until w) {
          val worldXi = ((camX + screenX) % WorldWidth + WorldWidth) % WorldWidth
          val hv = hillHeight(worldXi)
          val hillTop = groundRow - hv - 4

          for (row <- Math.max(0, hillTop) until h) {
            val ch =
              if (row == hillTop) '^'
              else if (row < groundRow - 2) ':'
              else if (row == groundRow - 2) '#'
              else '#'
            safeSet(buf, row, screenX, ch, w, h)
          }
        }

        // Draw trees
        trees.foreach { tree =>
          val screenX = ((tree.worldX - camX) % WorldWidth + WorldWidth) % WorldWidth
          if (screenX >= 2 && screenX < w - 2) {
            val worldXi = ((tree.worldX) % WorldWidth + WorldWidth) % WorldWidth
            val hv = hillHeight(worldXi)
            val treeBase = groundRow - hv - 4

            tree.style match {
              case 0 => // Tall pine
                safeSet(buf, treeBase, screenX, '|', w, h)
                safeSet(buf, treeBase - 1, screenX - 1, '/', w, h)
                safeSet(buf, treeBase - 1, screenX, '*', w, h)
                safeSet(buf, treeBase - 1, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 2, screenX - 1, '/', w, h)
                safeSet(buf, treeBase - 2, screenX, '*', w, h)
                safeSet(buf, treeBase - 2, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 3, screenX, '/', w, h)
                safeSet(buf, treeBase - 3, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 4, screenX, 'A', w, h)
              case 1 => // Wide pine
                safeSet(buf, treeBase, screenX, '|', w, h)
                safeSet(buf, treeBase - 1, screenX - 2, '/', w, h)
                safeSet(buf, treeBase - 1, screenX - 1, '*', w, h)
                safeSet(buf, treeBase - 1, screenX, '*', w, h)
                safeSet(buf, treeBase - 1, screenX + 1, '*', w, h)
                safeSet(buf, treeBase - 1, screenX + 2, '\\', w, h)
                safeSet(buf, treeBase - 2, screenX - 1, '/', w, h)
                safeSet(buf, treeBase - 2, screenX, '*', w, h)
                safeSet(buf, treeBase - 2, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 3, screenX, '/', w, h)
                safeSet(buf, treeBase - 3, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 4, screenX, '^', w, h)
              case _ => // Small spruce
                safeSet(buf, treeBase, screenX, '|', w, h)
                safeSet(buf, treeBase - 1, screenX - 1, '/', w, h)
                safeSet(buf, treeBase - 1, screenX, 'x', w, h)
                safeSet(buf, treeBase - 1, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 2, screenX, '/', w, h)
                safeSet(buf, treeBase - 2, screenX + 1, '\\', w, h)
                safeSet(buf, treeBase - 3, screenX, '^', w, h)
            }
          }
        }

        // Draw castles
        castles.foreach { castle =>
          val screenX = ((castle.worldX - camX) % WorldWidth + WorldWidth) % WorldWidth
          if (screenX >= 2 && screenX + 12 < w) {
            val worldXi = ((castle.worldX) % WorldWidth + WorldWidth) % WorldWidth
            val hv = hillHeight(worldXi)
            val base = groundRow - hv - 4

            // Bigger castle with towers, battlements, and gate
            val art = Vector(
              "  ^   /\\   ^  ",
              "  |   ||   |  ",
              " .|. [==] .|. ",
              " |=| |  | |=| ",
              " |=| |  | |=| ",
              " |=| |..| |=| ",
              " |=|_|__|_|=| ",
              " |  ======  | ",
              " | |######| | ",
              " |_|##  ##|_| ",
              "   |##  ##|   ",
              "   |##..##|   "
            )
            art.reverse.zipWithIndex.foreach { case (line, i) =>
              val row = base - i
              line.zipWithIndex.foreach { case (ch, j) =>
                if (ch != ' ') {
                  safeSet(buf, row, screenX + j, ch, w, h)
                }
              }
            }
          }
        }

        // Draw figures
        state.figures.foreach { fig =>
          val screenX = ((fig.worldX.toInt - camX) % WorldWidth + WorldWidth) % WorldWidth
          if (screenX >= 0 && screenX < w) {
            val worldXi = ((fig.worldX.toInt) % WorldWidth + WorldWidth) % WorldWidth
            val hv = hillHeight(worldXi)
            val figRow = groundRow - hv - 5
            safeSet(buf, figRow, screenX, fig.symbol, w, h)
          }
        }

        // Info bar at bottom
        val info = s" [A/D] scroll  |  Camera: ${state.cameraX.toInt}  |  Tick: ${state.tick} "
        if (h > 0) {
          val infoChars = info.toCharArray
          for (i <- infoChars.indices if i < w) {
            buf(h - 1)(i) = infoChars(i)
          }
        }

        ScreenAdjusted(buf.map(row => new String(row)).mkString("\n"))
      }

      private def advanceState(state: LandscapeState, cameraSpeed: Double): LandscapeState = {
        val newCameraX = (state.cameraX + cameraSpeed + WorldWidth) % WorldWidth
        val newFigures = state.figures.map { fig =>
          val newX = (fig.worldX + fig.speed + WorldWidth) % WorldWidth
          fig.copy(worldX = newX)
        }
        state.copy(
          cameraX = newCameraX,
          figures = newFigures,
          tick = state.tick + 1
        )
      }

      private def onTick(nrOfSteps: Int): F[Unit] =
        if (nrOfSteps <= 0) Monad[F].unit
        else
          for {
            maybeState <- stateRef.get
            _ <- maybeState.traverse_ { s =>
              for {
                screen <- console.context
                camSpeed <- cameraSpeedRef.get
                updated = (0 until nrOfSteps).foldLeft(s)((st, _) => advanceState(st, camSpeed))
                _ <- stateRef.set(Some(updated))
                frame = renderWorld(updated, screen.screenWidth, screen.screenHeight)
                _ <- lastFrameRef.set(frame)
                _ <- console.writeString(frame)
              } yield ()
            }
          } yield ()

      private val tickerCallback: F[Unit] =
        FixedStep.consumeSteps(stepperRef, animationSettings.step).flatMap(onTick)

      override def start(): F[Unit] = for {
        _ <- stateRef.set(
          Some(
            LandscapeState(
              cameraX = 0.0,
              figures = initialFigures,
              tick = 0
            )
          )
        )
        _ <- cameraSpeedRef.set(0.0)
        _ <- FixedStep.reset(stepperRef)
        sub <- ticker.subscribe(tickerCallback)
        _ <- subRef.set(Some(sub))
        _ <- ticker.start
      } yield ()

      override def stop(): F[Unit] = for {
        maybeSub <- subRef.get
        _ <- maybeSub.traverse_(_.cancel)
        _ <- subRef.set(None)
        _ <- stateRef.set(None)
      } yield ()

      override def handleInput(input: UserInput): F[Unit] = input match {
        case com.github.morotsman.lote.api.Character(c) if c == 'a' =>
          cameraSpeedRef.set(-1.0)
        case com.github.morotsman.lote.api.Character(c) if c == 'd' =>
          cameraSpeedRef.set(1.0)
        case com.github.morotsman.lote.api.Character(c) if c == 's' =>
          cameraSpeedRef.set(0.0)
        case _ =>
          Monad[F].unit
      }

      override def lastFrame: F[ScreenAdjusted] = lastFrameRef.get
    }
  }
}
