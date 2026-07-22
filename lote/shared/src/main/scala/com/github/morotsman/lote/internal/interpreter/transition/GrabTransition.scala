package com.github.morotsman.lote.internal.interpreter.transition

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, ScreenAdjusted}
import com.github.morotsman.lote.api.support.{AnimationClock, GlideLayer, SmoothChar, TickedTransition}
import com.github.morotsman.lote.api.spi.{NConsole, Ticker, Transition}

import scala.util.Random

/** A transition where a multi-line ASCII snake crawls in from the right at a random row, navigates to the end of the
  * longest text line, opens its mouth to grab, then drags the entire content off to the right.
  */
private[lote] object GrabTransition {

  // Multi-line snake facing left (head on left, body trailing right)
  // Mouth closed, crawling frame 1
  private val snakeCrawlLeft1: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """\/  /~   \_/  \        """,
    """\___|__________\       """,
    """     \_______   \      """,
    """          \      \     """,
    """           |      |    """,
    """          /      /     """,
    """         /      /      """
  )

  // Mouth closed, crawling frame 2 (body wiggles)
  private val snakeCrawlLeft2: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """\/  /~   \_/  \        """,
    """\___|__________\       """,
    """     \_______    \     """,
    """           \      \    """,
    """          /      /     """,
    """         /      /      """,
    """        /      /       """
  )

  // Mouth open (about to bite)
  private val snakeMouthOpenArt: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """    /        /         """,
    """   |          \        """,
    """    |__________\       """,
    """     \_______   \      """,
    """          \      \     """,
    """           |      |    """,
    """          /      /     """
  )

  // Mouth wide open
  private val snakeMouthWideArt: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """    /         /        """,
    """                \      """,
    """   |             \     """,
    """    |_____________\    """,
    """     \_______      \   """,
    """          \         \  """,
    """           |         | """
  )

  // Mouth closed after bite (grabbed)
  private val snakeBiteArt: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """    /    \_/  \        """,
    """    |__________\       """,
    """     \_______   \      """,
    """          \      \     """,
    """           |      |    """,
    """          /      /     """,
    """         /      /      """
  )

  // Crawling backwards (no tongue while dragging)
  private val snakeCrawlRight1: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """    /    \_/  \        """,
    """    |__________\       """,
    """     \_______   \      """,
    """          \      \     """,
    """           |      |    """,
    """          /      /     """,
    """         /      /      """
  )
  private val snakeCrawlRight2: Vector[String] = Vector(
    """        /^\/^\         """,
    """     _|__|  0|         """,
    """    /    \_/  \        """,
    """    |__________\       """,
    """     \_______    \     """,
    """           \      \    """,
    """          /      /     """,
    """         /      /      """,
    """        /      /       """
  )

  private val snakeHeight = snakeCrawlLeft1.length
  private val snakeWidth = snakeCrawlLeft1.map(_.length).max

  sealed trait GrabPhase
  case class CrawlIn(
      step: Int,
      totalSteps: Int,
      spawnRow: Int,
      targetRow: Int,
      targetCol: Int
  ) extends GrabPhase
  case class Bite(frame: Int) extends GrabPhase
  case class DragOut(step: Int, totalSteps: Int, grabberRow: Int, startCol: Int) extends GrabPhase
  case object Done extends GrabPhase

  private case class SnakeRenderInfo(
      snakeArt: Vector[String],
      snakeRow: Int,
      snakeCol: Int,
      dragOffset: Int
  )

  private case class GrabStepResult(
      renderedFrame: Option[ScreenAdjusted],
      nextPhase: GrabPhase,
      completed: Boolean = false,
      snakeInfo: Option[SnakeRenderInfo] = None
  )

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      stepSize: Int = 2
  )(implicit animationSettings: AnimationSettings): Transition[F] =
    create(stepSize, NConsole[F], Ticker[F], animationSettings)

  def create[F[_]: Temporal: Ref.Make](
      stepSize: Int = 2,
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): Transition[F] = {

    def snakeToSmoothChars(info: SnakeRenderInfo): Vector[SmoothChar] =
      info.snakeArt.zipWithIndex.flatMap { case (line, rowOffset) =>
        val trimmed = line.stripTrailing()
        val row = info.snakeRow + rowOffset
        trimmed.zipWithIndex.collect {
          case (ch, colOffset) if ch != ' ' =>
            SmoothChar(ch, info.snakeCol + colOffset, row)
        }
      }

    def renderWithOverlay(
        info: SnakeRenderInfo,
        lines: Vector[String],
        screenWidth: Int,
        screenHeight: Int,
        gridLayer: GlideLayer.Overlay[F],
        scrollFrac: Double = 0.0
    ): F[Unit] = {
      val overlayChars = snakeToSmoothChars(info)
      val bgContent = renderScene(lines, screenWidth, screenHeight, 0, 0, Vector.empty, info.dragOffset)
      val bgFrame = ScreenAdjusted(bgContent)
      val scrollX = if (info.dragOffset > 0) scrollFrac else 0.0
      console.clear() *> gridLayer.renderOntoScrolled(bgFrame, overlayChars, scrollX = scrollX)
    }

    def findLongestLineEnd(lines: Vector[String]): (Int, Int) = {
      var maxRow = 0
      var maxCol = 0
      lines.zipWithIndex.foreach { case (line, row) =>
        val lastNonSpace = line.lastIndexWhere(_ != ' ')
        if (lastNonSpace > maxCol) {
          maxCol = lastNonSpace
          maxRow = row
        }
      }
      (maxRow, maxCol + 1)
    }

    def renderScene(
        lines: Vector[String],
        screenWidth: Int,
        screenHeight: Int,
        snakeTopRow: Int,
        snakeCol: Int,
        snakeArt: Vector[String],
        dragOffset: Int = 0
    ): String = {
      lines.zipWithIndex
        .map { case (line, row) =>
          val paddedLine =
            if (line.length < screenWidth)
              line + " " * (screenWidth - line.length)
            else line.take(screenWidth)

          val shifted = if (dragOffset > 0) {
            (" " * Math.min(dragOffset, screenWidth) + paddedLine).take(screenWidth)
          } else {
            paddedLine
          }

          val snakeLineIndex = row - snakeTopRow
          if (snakeLineIndex >= 0 && snakeLineIndex < snakeArt.length) {
            val snakeLine = snakeArt(snakeLineIndex)
            val trimmedSnakeLine = snakeLine.stripTrailing()
            if (trimmedSnakeLine.nonEmpty && snakeCol >= 0 && snakeCol < screenWidth) {
              val c = Math.max(0, snakeCol)
              val availableWidth = screenWidth - c
              val snakePart = trimmedSnakeLine.take(availableWidth)
              if (c + snakePart.length <= shifted.length) {
                shifted.take(c) + snakePart + shifted.drop(c + snakePart.length)
              } else {
                (shifted.take(c) + snakePart).take(screenWidth)
              }
            } else {
              shifted
            }
          } else {
            shifted
          }
        }
        .take(screenHeight)
        .mkString("\n")
    }

    TickedTransition(console, ticker, animationSettings)
      .buildWithSetup { (slide1Content, _slide2Content, complete) =>
        for {
          gridLayer <- GlideLayer.make[F](console, animationSettings.step, wrapThreshold = stepSize)
          screen <- console.context
          lines1 = slide1Content.content.split("\n", -1).toVector
          maxSpawnRow = Math.max(1, Math.min(lines1.length, screen.screenHeight) - snakeHeight)
          spawnRow = Random.nextInt(maxSpawnRow)
          longestLineEnd = findLongestLineEnd(lines1)
          (targetRow, targetCol) = longestLineEnd
          snakeTargetRow = Math.max(0, targetRow - snakeHeight / 2)
          startCol = screen.screenWidth
          horizontalDistance = startCol - targetCol
          crawlTotalSteps = Math.max(1, horizontalDistance / stepSize)
          initialPhase: GrabPhase = CrawlIn(0, crawlTotalSteps, spawnRow, snakeTargetRow, targetCol)
          phaseRef <- Ref[F].of(initialPhase)
          snakeInfoRef <- Ref[F].of(Option.empty[SnakeRenderInfo])
          holdFrames = Vector(4, 5, 3)
          cumulativeBiteFrames = holdFrames.scanLeft(0)(_ + _).tail
          totalBiteFrames = cumulativeBiteFrames.last
          dragOutTotalSteps = Math.max(1, (screen.screenWidth + snakeWidth) / stepSize)
          advancePhase = { (phase: GrabPhase) =>
            phase match {
              case CrawlIn(step, totalSteps, spawnRow, tRow, tCol) =>
                val crawlFrames = Vector(snakeCrawlLeft1, snakeCrawlLeft2)
                val frameIndex = step % crawlFrames.length
                val snakeArt = crawlFrames(frameIndex)
                val col = startCol - (step * stepSize)
                val progress = step.toDouble / totalSteps.toDouble
                val row = (spawnRow + ((tRow - spawnRow) * progress)).toInt
                val content = renderScene(lines1, screen.screenWidth, screen.screenHeight, row, col, snakeArt)
                GrabStepResult(
                  renderedFrame = Some(ScreenAdjusted(content)),
                  nextPhase = if (step >= totalSteps) Bite(0) else CrawlIn(step + 1, totalSteps, spawnRow, tRow, tCol),
                  snakeInfo = Some(SnakeRenderInfo(snakeArt, row, col, 0))
                )
              case Bite(frame) =>
                val scenes = Vector(snakeMouthOpenArt, snakeMouthWideArt, snakeBiteArt)
                if (frame >= totalBiteFrames) {
                  GrabStepResult(
                    renderedFrame = None,
                    nextPhase = DragOut(0, dragOutTotalSteps, snakeTargetRow, targetCol),
                    snakeInfo = Some(SnakeRenderInfo(snakeBiteArt, snakeTargetRow, targetCol, 0))
                  )
                } else {
                  val sceneIdx = cumulativeBiteFrames.indexWhere(_ > frame)
                  val snakeArt = scenes(sceneIdx)
                  val content =
                    renderScene(lines1, screen.screenWidth, screen.screenHeight, snakeTargetRow, targetCol, snakeArt)
                  GrabStepResult(
                    renderedFrame = Some(ScreenAdjusted(content)),
                    nextPhase = Bite(frame + 1),
                    snakeInfo = Some(SnakeRenderInfo(snakeArt, snakeTargetRow, targetCol, 0))
                  )
                }
              case DragOut(step, totalSteps, grabberRow, sCol) =>
                val crawlFrames = Vector(snakeCrawlRight1, snakeCrawlRight2)
                val frameIndex = step % crawlFrames.length
                val snakeArt = crawlFrames(frameIndex)
                val snakeCol = sCol + (step * stepSize)
                val dragOffset = step * stepSize
                val content = renderScene(
                  lines1,
                  screen.screenWidth,
                  screen.screenHeight,
                  grabberRow,
                  snakeCol,
                  snakeArt,
                  dragOffset
                )
                GrabStepResult(
                  renderedFrame = Some(ScreenAdjusted(content)),
                  nextPhase = if (step >= totalSteps) Done else DragOut(step + 1, totalSteps, grabberRow, sCol),
                  completed = step >= totalSteps,
                  snakeInfo = Some(SnakeRenderInfo(snakeArt, grabberRow, snakeCol, dragOffset))
                )
              case Done =>
                GrabStepResult(renderedFrame = None, nextPhase = Done, completed = true)
            }
          }
        } yield TickedTransition.TickHandler(
          onTick = (nrOfSteps: Int, progress: Double) => {
            val scrollFrac = progress * stepSize
            if (nrOfSteps <= 0) {
              snakeInfoRef.get.flatMap {
                case Some(info) =>
                  renderWithOverlay(info, lines1, screen.screenWidth, screen.screenHeight, gridLayer, scrollFrac)
                case None => Monad[F].unit
              }
            } else {
              for {
                phase <- phaseRef.get
                result =
                  (0 until nrOfSteps).foldLeft(
                    (phase, Option.empty[ScreenAdjusted], false, Option.empty[SnakeRenderInfo])
                  ) { case ((currentPhase, renderedFrame, completed, _snakeInfo), _) =>
                    if (completed) {
                      (currentPhase, renderedFrame, true, _snakeInfo)
                    } else {
                      val stepResult = advancePhase(currentPhase)
                      (
                        stepResult.nextPhase,
                        stepResult.renderedFrame.orElse(renderedFrame),
                        stepResult.completed,
                        stepResult.snakeInfo.orElse(_snakeInfo)
                      )
                    }
                  }
                (nextPhase, renderedFrame, completed, latestSnakeInfo) = result
                _ <- phaseRef.set(nextPhase)
                _ <- latestSnakeInfo.traverse_(info => snakeInfoRef.set(Some(info)))
                _ <- latestSnakeInfo match {
                  case Some(info) =>
                    renderWithOverlay(info, lines1, screen.screenWidth, screen.screenHeight, gridLayer, scrollFrac)
                  case None =>
                    renderedFrame.traverse_(frame => console.clear() *> console.writeString(frame))
                }
                _ <- if (completed) complete else Monad[F].unit
              } yield ()
            }
          },
          cleanup = gridLayer.clear()
        )
      }
  }
}
