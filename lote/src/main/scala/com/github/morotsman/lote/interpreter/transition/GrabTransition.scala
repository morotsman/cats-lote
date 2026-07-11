package com.github.morotsman.lote.interpreter.transition

import cats.Monad
import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.implicits._
import cats.implicits._
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Slide, Ticker, Transition}
import com.github.morotsman.lote.interpreter.animation.FixedStep
import com.github.morotsman.lote.model.{ScreenAdjusted, UserInput}

import scala.util.Random

/** A transition where a multi-line ASCII snake crawls in from the right at a random row, navigates to the end of the
  * longest text line, opens its mouth to grab, then drags the entire content off to the right.
  */
object GrabTransition {

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

  private case class GrabStepResult(
      renderedFrame: Option[ScreenAdjusted],
      nextPhase: GrabPhase,
      completed: Boolean = false
  )

  def apply[F[_]: Temporal: Ref.Make: NConsole: Ticker](
      stepSize: Int = 2
  )(implicit animationSettings: AnimationSettings): Transition[F] = new Transition[F] {

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      for {
        screen <- NConsole[F].context
        slide1 <- from.content
        slide2 <- to.content
        lines1 = slide1.content.split("\n", -1).toVector
        maxSpawnRow = Math.max(
          1,
          Math.min(lines1.length, screen.screenHeight) - snakeHeight
        )
        spawnRow = Random.nextInt(maxSpawnRow)
        longestLineEnd = findLongestLineEnd(lines1)
        (targetRow, targetCol) = longestLineEnd
        snakeTargetRow = Math.max(0, targetRow - snakeHeight / 2)
        startCol = screen.screenWidth
        horizontalDistance = startCol - targetCol
        crawlTotalSteps = Math.max(1, horizontalDistance / stepSize)
        initialPhase: GrabPhase = CrawlIn(
          0,
          crawlTotalSteps,
          spawnRow,
          snakeTargetRow,
          targetCol
        )
        phaseRef <- Ref[F].of(initialPhase)
        stepperRef <- FixedStep.makeRef[F]
        done <- Deferred[F, Unit]
        holdFrames = Vector(4, 5, 3)
        cumulativeBiteFrames = holdFrames.scanLeft(0)(_ + _).tail
        totalBiteFrames = cumulativeBiteFrames.last
        dragOutTotalSteps =
          Math.max(1, (screen.screenWidth + snakeWidth) / stepSize)
        advancePhase = { (phase: GrabPhase) =>
          phase match {
            case CrawlIn(step, totalSteps, spawnRow, tRow, tCol) =>
              val crawlFrames = Vector(snakeCrawlLeft1, snakeCrawlLeft2)
              val frameIndex = step % crawlFrames.length
              val snakeArt = crawlFrames(frameIndex)
              val col = startCol - (step * stepSize)
              val progress = step.toDouble / totalSteps.toDouble
              val row = (spawnRow + ((tRow - spawnRow) * progress)).toInt
              val content = renderScene(
                lines1,
                screen.screenWidth,
                screen.screenHeight,
                row,
                col,
                snakeArt
              )

              GrabStepResult(
                renderedFrame = Some(ScreenAdjusted(content)),
                nextPhase =
                  if (step >= totalSteps) Bite(0)
                  else CrawlIn(step + 1, totalSteps, spawnRow, tRow, tCol)
              )
            case Bite(frame) =>
              val scenes =
                Vector(snakeMouthOpenArt, snakeMouthWideArt, snakeBiteArt)

              if (frame >= totalBiteFrames) {
                GrabStepResult(
                  renderedFrame = None,
                  nextPhase =
                    DragOut(0, dragOutTotalSteps, snakeTargetRow, targetCol)
                )
              } else {
                val sceneIdx = cumulativeBiteFrames.indexWhere(_ > frame)
                val snakeArt = scenes(sceneIdx)
                val content = renderScene(
                  lines1,
                  screen.screenWidth,
                  screen.screenHeight,
                  snakeTargetRow,
                  targetCol,
                  snakeArt
                )
                GrabStepResult(
                  renderedFrame = Some(ScreenAdjusted(content)),
                  nextPhase = Bite(frame + 1)
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
                completed = step >= totalSteps
              )
            case Done =>
              GrabStepResult(renderedFrame = None, nextPhase = Done, completed = true)
          }
        }
        onTick = { (nrOfSteps: Int) =>
          if (nrOfSteps <= 0) Monad[F].unit
          else {
            for {
              phase <- phaseRef.get
              result =
                (0 until nrOfSteps).foldLeft(
                  (phase, Option.empty[ScreenAdjusted], false)
                ) { case ((currentPhase, renderedFrame, completed), _) =>
                  if (completed) {
                    (currentPhase, renderedFrame, true)
                  } else {
                    val stepResult = advancePhase(currentPhase)
                    (
                      stepResult.nextPhase,
                      stepResult.renderedFrame.orElse(renderedFrame),
                      stepResult.completed
                    )
                  }
                }
              (nextPhase, renderedFrame, completed) = result
              _ <- phaseRef.set(nextPhase)
              _ <- renderedFrame.traverse_(frame =>
                NConsole[F].clear() *> NConsole[F].writeString(frame)
              )
              _ <- if (completed) done.complete(()).attempt.void else Monad[F].unit
            } yield ()
          }
        }
        tickerCallback = FixedStep.consumeSteps(stepperRef).flatMap(onTick)
        sub <- Ticker[F].subscribe(tickerCallback)
        _ <- Ticker[F].start
        _ <- done.get.guarantee(sub.cancel)
        _ <- NConsole[F].clear()
        _ <- NConsole[F].writeString(slide2)
      } yield ()
    }

    private def findLongestLineEnd(lines: Vector[String]): (Int, Int) = {
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

    private def renderScene(
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
            (" " * Math.min(dragOffset, screenWidth) + paddedLine).take(
              screenWidth
            )
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

    override def userInput(input: UserInput): F[Unit] = Monad[F].unit
  }
}
