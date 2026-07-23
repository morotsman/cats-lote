package com.github.morotsman.examples.landscape

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{AnimationClock, SmoothChar, TickedSlide}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

/** An interactive landscape slide with rolling hills, a castle, trees, and roaming figures.
  *
  * The scene scrolls horizontally. Figures wander on their own and the viewer can nudge the camera left/right with `a`
  * / `d`.
  */
object LandscapeSlide {

  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      create[F](builder.console, builder.ticker, builder.animationSettings)
    }

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): F[Slide[F]] = {

    val rng = new scala.util.Random(123)
    val initialFigures = (0 until 12).map { i =>
      val symbols = "&#@%$"
      Figure(
        worldX = rng.nextInt(WorldWidth).toDouble,
        speed = (rng.nextDouble() * 0.6 + 0.1) * (if (rng.nextBoolean()) 1 else -1),
        symbol = symbols(i % symbols.length)
      )
    }.toVector

    val initialState = LandscapeState(cameraX = 0.0, figures = initialFigures, tick = 0)

    for {
      stateRef <- Ref[F].of(Option.empty[LandscapeState])
      cameraSpeedRef <- Ref[F].of(0.0)
      lastFrameRef <- Ref[F].of(ScreenAdjusted(""))
      slide <- TickedSlide[F](console, ticker, animationSettings)
        .withGlideLayer(wrapThreshold = 20)
        .buildWithGlideProgress(
          onTick = { (nrOfSteps, progress, glide) =>
            for {
              maybeState <- stateRef.get
              _ <- maybeState.traverse_ { s =>
                for {
                  screen <- console.context
                  camSpeed <- cameraSpeedRef.get
                  updated = (0 until nrOfSteps).foldLeft(s)((st, _) => advanceState(st, camSpeed))
                  _ <- stateRef.set(Some(updated))
                  interpCameraX = (updated.cameraX + camSpeed * progress + WorldWidth) % WorldWidth
                  intCameraX = Math.floor(interpCameraX).toInt
                  fracOffset = interpCameraX - intCameraX
                  gridState = updated.copy(cameraX = intCameraX.toDouble)
                  frame = renderWorld(gridState, screen.screenWidth, screen.screenHeight)
                  _ <- lastFrameRef.set(frame)
                  figureChars = figuresToSmoothChars(updated, intCameraX, screen.screenWidth, screen.screenHeight)
                  infoChars = infoBarToSmoothChars(updated, screen.screenWidth, screen.screenHeight)
                  _ <- glide.renderOntoScrolled(
                    frame,
                    figureChars ++ infoChars,
                    scrollX = -fracOffset,
                    fixedRows = Set(screen.screenHeight - 1, screen.screenHeight - 2)
                  )
                } yield ()
              }
            } yield ()
          },
          onInput = {
            case com.github.morotsman.lote.api.Character(c) if c == 'a' => cameraSpeedRef.set(-1.0)
            case com.github.morotsman.lote.api.Character(c) if c == 'd' => cameraSpeedRef.set(1.0)
            case com.github.morotsman.lote.api.Character(c) if c == 's' => cameraSpeedRef.set(0.0)
            case _                                                      => Monad[F].unit
          },
          onStart = stateRef.set(Some(initialState)) >> cameraSpeedRef.set(0.0),
          contentF = Some(lastFrameRef.get.map(Some(_)))
        )
    } yield slide
  }

  // ── World constants ──────────────────────────────────────────────

  private val WorldWidth = 300

  private val hillHeight: Array[Int] = {
    val arr = new Array[Int](WorldWidth)
    for (x <- arr.indices) {
      arr(x) = (
        3.0 * Math.sin(x * 0.03) +
          2.0 * Math.sin(x * 0.07 + 1.0) +
          1.5 * Math.sin(x * 0.13 + 2.5) +
          1.0 * Math.sin(x * 0.21 + 0.7)
      ).toInt
    }
    arr
  }

  private case class Tree(worldX: Int, style: Int)
  private case class Castle(worldX: Int)

  private val trees: Vector[Tree] = {
    val rng = new scala.util.Random(42)
    (0 until 35).map(_ => Tree(rng.nextInt(WorldWidth), rng.nextInt(3))).toVector
  }

  private val castles: Vector[Castle] = Vector(Castle(50), Castle(180))

  private def safeSet(buf: Array[Array[Char]], row: Int, col: Int, ch: Char, width: Int, height: Int): Unit = {
    if (row >= 0 && row < height && col >= 0 && col < width) {
      buf(row)(col) = ch
    }
  }

  // ── State model ──────────────────────────────────────────────────

  private def advanceState(state: LandscapeState, cameraSpeed: Double): LandscapeState = {
    val newCameraX = (state.cameraX + cameraSpeed + WorldWidth) % WorldWidth
    val newFigures = state.figures.map { fig =>
      val newX = (fig.worldX + fig.speed + WorldWidth) % WorldWidth
      fig.copy(worldX = newX)
    }
    state.copy(cameraX = newCameraX, figures = newFigures, tick = state.tick + 1)
  }

  // ── Rendering ────────────────────────────────────────────────────

  private def renderWorld(
      state: LandscapeState,
      screenWidth: Int,
      screenHeight: Int
  ): ScreenAdjusted = {
    if (screenWidth <= 0 || screenHeight <= 0) return ScreenAdjusted("")

    val camX = state.cameraX.toInt
    val groundRow = screenHeight - 6
    val w = screenWidth
    val h = screenHeight

    val buf = Array.fill(h)(Array.fill(w)(' '))

    // Stars
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

    // Hills and ground
    for (screenX <- 0 until w) {
      val worldXi = ((camX + screenX) % WorldWidth + WorldWidth) % WorldWidth
      val hv = hillHeight(worldXi)
      val hillTop = groundRow - hv - 4
      for (row <- Math.max(0, hillTop) until h) {
        val ch =
          if (row == hillTop) '^'
          else if (row < groundRow - 2) ':'
          else '#'
        safeSet(buf, row, screenX, ch, w, h)
      }
    }

    // Trees
    trees.foreach { tree =>
      val screenX = ((tree.worldX - camX) % WorldWidth + WorldWidth) % WorldWidth
      if (screenX >= 2 && screenX < w - 2) {
        val worldXi = ((tree.worldX) % WorldWidth + WorldWidth) % WorldWidth
        val hv = hillHeight(worldXi)
        val treeBase = groundRow - hv - 4
        tree.style match {
          case 0 =>
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
          case 1 =>
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
          case _ =>
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

    // Castles
    castles.foreach { castle =>
      val screenX = ((castle.worldX - camX) % WorldWidth + WorldWidth) % WorldWidth
      if (screenX >= 2 && screenX + 12 < w) {
        val worldXi = ((castle.worldX) % WorldWidth + WorldWidth) % WorldWidth
        val hv = hillHeight(worldXi)
        val base = groundRow - hv - 4
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
            if (ch != ' ') safeSet(buf, row, screenX + j, ch, w, h)
          }
        }
      }
    }

    // Clear bottom row for info bar
    if (h > 0) for (i <- 0 until w) buf(h - 1)(i) = ' '

    ScreenAdjusted(buf.map(row => new String(row)).mkString("\n"))
  }

  private def figuresToSmoothChars(
      state: LandscapeState,
      camX: Int,
      screenWidth: Int,
      screenHeight: Int
  ): Vector[SmoothChar] = {
    val groundRow = screenHeight - 6
    state.figures.zipWithIndex.flatMap { case (fig, idx) =>
      val screenX = ((fig.worldX.toInt - camX) % WorldWidth + WorldWidth) % WorldWidth
      if (screenX >= 0 && screenX < screenWidth) {
        val worldXi = ((fig.worldX.toInt) % WorldWidth + WorldWidth) % WorldWidth
        val hv = hillHeight(worldXi)
        val figRow = groundRow - hv - 5
        if (figRow >= 0 && figRow < screenHeight)
          Some(SmoothChar(fig.symbol, screenX, figRow, "#ffffff", key = idx))
        else None
      } else None
    }
  }

  private def infoBarToSmoothChars(
      state: LandscapeState,
      screenWidth: Int,
      screenHeight: Int
  ): Vector[SmoothChar] = {
    if (screenHeight <= 0) return Vector.empty
    val info = s" [A/D] scroll  |  Camera: ${state.cameraX.toInt}  |  Tick: ${state.tick} "
    val row = screenHeight - 1
    info.zipWithIndex.collect {
      case (ch, col) if col < screenWidth && ch != ' ' =>
        SmoothChar(ch, col, row, "#ffffff", key = 1000 + col)
    }.toVector
  }
}

// ---- Landscape data model ----

private[landscape] case class Figure(
    worldX: Double,
    speed: Double,
    symbol: Char
)

private[landscape] case class LandscapeState(
    cameraX: Double,
    figures: Vector[Figure],
    tick: Long
)
