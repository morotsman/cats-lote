package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.model.{ScreenAdjusted, UserInput}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

/**
 * A transition where a multi-line ASCII snake crawls in from the right at a random row,
 * navigates to the end of the longest text line, opens its mouth to grab,
 * then drags the entire content off to the right.
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

  def apply[F[_] : Temporal : NConsole](
                                         speed: FiniteDuration = 60.milli,
                                         stepSize: Int = 2
                                       ): Transition[F] = new Transition[F] {

    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      for {
        screen <- NConsole[F].context
        slide1 <- from.content
        slide2 <- to.content
        lines1 = slide1.content.split("\n", -1).toVector
        maxSpawnRow = Math.max(1, Math.min(lines1.length, screen.screenHeight) - snakeHeight)
        spawnRow = Random.nextInt(maxSpawnRow)
        (targetRow, targetCol) = findLongestLineEnd(lines1)
        // Center the snake vertically on the target row
        snakeTargetRow = Math.max(0, targetRow - snakeHeight / 2)
        _ <- animatePath(lines1, screen.screenWidth, screen.screenHeight, spawnRow, snakeTargetRow, targetCol)
        _ <- animateBite(lines1, screen.screenWidth, screen.screenHeight, snakeTargetRow, targetCol)
        _ <- animateDragOut(lines1, screen.screenWidth, screen.screenHeight, snakeTargetRow, targetCol)
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
      lines.zipWithIndex.map { case (line, row) =>
        val paddedLine = if (line.length < screenWidth) line + " " * (screenWidth - line.length)
        else line.take(screenWidth)

        // Shift content right by dragOffset
        val shifted = if (dragOffset > 0) {
          (" " * Math.min(dragOffset, screenWidth) + paddedLine).take(screenWidth)
        } else {
          paddedLine
        }

        // Check if this row is part of the snake
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
      }.take(screenHeight).mkString("\n")
    }

    private def animatePath(
                             lines: Vector[String],
                             screenWidth: Int,
                             screenHeight: Int,
                             spawnRow: Int,
                             targetRow: Int,
                             targetCol: Int
                           ): F[Unit] = {

      val startCol = screenWidth
      val horizontalDistance = startCol - targetCol
      val verticalDistance = targetRow - spawnRow
      val totalSteps = Math.max(1, horizontalDistance / stepSize)

      val crawlFrames = Vector(snakeCrawlLeft1, snakeCrawlLeft2)

      (0 to totalSteps).toList.traverse_ { step =>
        val frameIndex = step % crawlFrames.length
        val snakeArt = crawlFrames(frameIndex)
        val col = startCol - (step * stepSize)
        val progress = step.toDouble / totalSteps.toDouble
        val row = (spawnRow + (verticalDistance * progress)).toInt

        val content = renderScene(lines, screenWidth, screenHeight, row, col, snakeArt)
        NConsole[F].clear() >> NConsole[F].writeString(ScreenAdjusted(content)) >> Temporal[F].sleep(speed)
      }
    }

    private def animateBite(
                             lines: Vector[String],
                             screenWidth: Int,
                             screenHeight: Int,
                             targetRow: Int,
                             targetCol: Int
                           ): F[Unit] = {
      // Open mouth gradually, then snap shut
      val scene1 = renderScene(lines, screenWidth, screenHeight, targetRow, targetCol, snakeMouthOpenArt)
      val scene2 = renderScene(lines, screenWidth, screenHeight, targetRow, targetCol, snakeMouthWideArt)
      val scene3 = renderScene(lines, screenWidth, screenHeight, targetRow, targetCol, snakeBiteArt)

      NConsole[F].clear() >> NConsole[F].writeString(ScreenAdjusted(scene1)) >> Temporal[F].sleep(speed * 4) >>
        NConsole[F].clear() >> NConsole[F].writeString(ScreenAdjusted(scene2)) >> Temporal[F].sleep(speed * 5) >>
        NConsole[F].clear() >> NConsole[F].writeString(ScreenAdjusted(scene3)) >> Temporal[F].sleep(speed * 3)
    }

    private def animateDragOut(
                                lines: Vector[String],
                                screenWidth: Int,
                                screenHeight: Int,
                                grabberRow: Int,
                                startCol: Int
                              ): F[Unit] = {

      val stepsNeeded = Math.max(1, (screenWidth + snakeWidth) / stepSize)
      val crawlFrames = Vector(snakeCrawlRight1, snakeCrawlRight2)

      (0 to stepsNeeded).toList.traverse_ { step =>
        val frameIndex = step % crawlFrames.length
        val snakeArt = crawlFrames(frameIndex)
        val snakeCol = startCol + (step * stepSize)
        val dragOffset = step * stepSize

        val content = renderScene(lines, screenWidth, screenHeight, grabberRow, snakeCol, snakeArt, dragOffset)
        NConsole[F].clear() >> NConsole[F].writeString(ScreenAdjusted(content)) >> Temporal[F].sleep(speed)
      }
    }

    override def userInput(input: UserInput): F[Unit] = Temporal[F].unit
  }
}
