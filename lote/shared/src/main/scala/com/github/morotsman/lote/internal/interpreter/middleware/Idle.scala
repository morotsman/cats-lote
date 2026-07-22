package com.github.morotsman.lote.internal.interpreter.middleware

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted, UserInput}
import com.github.morotsman.lote.internal.algebra.IdleDetector
import com.github.morotsman.lote.api.spi.{NConsole, Overlay}
import com.github.morotsman.lote.api.support.{Clock => LoteClock, FixedStep, GlideLayer, SmoothChar}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

case class Bug(
    x: Int,
    y: Int,
    char: String,
    carrying: Option[String] = None,
    targetEdge: Int = -1
)

case class StolenWord(row: Int, startCol: Int, length: Int)

private[middleware] object IdleAnsiSupport {

  private case class VisibleCell(prefix: String, char: Char)
  private case class ParsedLine(cells: Vector[VisibleCell], trailingAnsi: String) {
    def visibleText: String = cells.map(_.char).mkString
  }

  def stripAnsi(line: String): String =
    parseLine(line).visibleText

  def hasVisibleText(line: String): Boolean =
    parseLine(line).cells.exists(_.char != ' ')

  def findWordAt(
      lines: Array[String],
      x: Int,
      y: Int
  ): Option[(String, Int, Int)] = {
    if (y < 0 || y >= lines.length) return None

    val parsed = parseLine(lines(y))
    if (x < 0 || x >= parsed.cells.length || parsed.cells(x).char == ' ') return None

    var start = x
    while (start > 0 && parsed.cells(start - 1).char != ' ') start -= 1
    var end = x
    while (end < parsed.cells.length - 1 && parsed.cells(end + 1).char != ' ') end += 1

    val word = parsed.cells.slice(start, end + 1).map(_.char).mkString
    if (word.trim.nonEmpty) Some((word, start, end + 1)) else None
  }

  def textPositions(lines: Array[String]): Array[(Int, Int)] =
    lines.zipWithIndex.flatMap { case (line, y) =>
      parseLine(line).cells.zipWithIndex.collect {
        case (cell, x) if cell.char != ' ' => (x, y)
      }
    }

  def applyStolen(
      lines: Array[String],
      stolenWords: List[StolenWord]
  ): Array[String] = {
    val result = lines.clone()
    val stolenByRow = stolenWords.groupBy(_.row)

    result.indices.foreach { row =>
      stolenByRow.get(row).foreach { rowStolenWords =>
        val parsed = parseLine(result(row))
        val cells = parsed.cells.toArray

        rowStolenWords.foreach { sw =>
          val start = Math.max(0, sw.startCol)
          val end = Math.min(cells.length, sw.startCol + sw.length)
          var idx = start
          while (idx < end) {
            cells(idx) = cells(idx).copy(char = ' ')
            idx += 1
          }
        }

        result(row) = rebuildLine(cells.toVector, parsed.trailingAnsi)
      }
    }

    result
  }

  def overlayText(
      line: String,
      startCol: Int,
      text: String,
      screenWidth: Int
  ): String = {
    if (screenWidth <= 0) return ""

    val parsed = parseLine(line)
    val baseCells =
      if (parsed.cells.length < screenWidth)
        parsed.cells ++ Vector.fill(screenWidth - parsed.cells.length)(VisibleCell("", ' '))
      else parsed.cells.take(screenWidth)

    if (startCol < 0 || startCol >= baseCells.length || text.isEmpty)
      rebuildLine(baseCells, parsed.trailingAnsi)
    else {
      val maxWrite = Math.min(text.length, screenWidth - startCol)
      val updatedCells = baseCells.zipWithIndex.map { case (cell, idx) =>
        val relative = idx - startCol
        if (relative >= 0 && relative < maxWrite) cell.copy(char = text.charAt(relative))
        else cell
      }
      rebuildLine(updatedCells, parsed.trailingAnsi)
    }
  }

  private def parseLine(line: String): ParsedLine = {
    val cells = Vector.newBuilder[VisibleCell]
    val pendingAnsi = new StringBuilder
    var index = 0

    while (index < line.length) {
      if (
        line.charAt(index) == '\u001b' &&
        index + 1 < line.length &&
        line.charAt(index + 1) == '['
      ) {
        val start = index
        index += 2
        while (index < line.length && line.charAt(index) != 'm') {
          index += 1
        }
        if (index < line.length) {
          index += 1
        }
        pendingAnsi.append(line.substring(start, index))
      } else {
        cells += VisibleCell(pendingAnsi.toString, line.charAt(index))
        pendingAnsi.clear()
        index += 1
      }
    }

    ParsedLine(cells.result(), pendingAnsi.toString)
  }

  private def rebuildLine(cells: Vector[VisibleCell], trailingAnsi: String): String =
    cells.map(cell => s"${cell.prefix}${cell.char}").mkString + trailingAnsi
}

private[lote] case class IdleOverlayConfig(
    firstBugDelay: FiniteDuration = 0.seconds,
    spawnInterval: FiniteDuration = 3.seconds,
    bugsPerSpawn: Int = 6,
    maxBugs: Int = 100,
    bugChars: List[String] = List("@", "o", "*", "~", "#"),
    bugSpeed: Int = 1,
    // Time display wobble settings
    timeWobbleApproachChance: Double = 0.1,
    timeWobbleAtTargetXChance: Double = 0.05,
    timeWobbleAtTargetYChance: Double = 0.05,
    timeWobbleMaxOffset: Int = 1
)

private case class IdleOverlayState(
    idleStartTime: Long = 0,
    bugs: List[Bug] = List.empty,
    stolenWords: List[StolenWord] = List.empty,
    lastSpawnTime: Long = 0,
    spawnCount: Int = 0,
    wasIdle: Boolean = false,
    random: Random = new Random()
)

private[lote] trait Idle[F[_]] extends Overlay[F]

private[lote] object Idle {

  /** The simulation step duration for the idle bug animation. Bug movement advances once per step, independent of the
    * render/ticker frequency.
    */
  private val IdleStep: FiniteDuration = 70.millis

  def make[F[_]: Temporal: Ref.Make](
      idleDetector: IdleDetector[F],
      console: NConsole[F],
      config: IdleOverlayConfig = IdleOverlayConfig()
  ): F[Idle[F]] = {
    implicit val loteClock: LoteClock[F] = LoteClock.fromTemporal[F]
    for {
      state <- Ref[F].of(IdleOverlayState())
      stepperRef <- FixedStep.makeRef[F]
      glideLayer <- GlideLayer.make[F](console, IdleStep, wrapThreshold = 2)
    } yield new Idle[F] {

      /** Convert bugs to SmoothChars for overlay rendering. */
      private def bugsToSmoothChars(bugs: List[Bug]): Vector[SmoothChar] =
        bugs.zipWithIndex.flatMap { case (bug, bugIdx) =>
          val displayStr = bug.carrying match {
            case Some(word) => bug.char + word
            case None       => bug.char
          }
          displayStr.zipWithIndex.map { case (ch, charOffset) =>
            SmoothChar(ch, bug.x + charOffset, bug.y, key = bugIdx * 100 + charOffset)
          }
        }.toVector

      private def spawnBugAtEdge(context: Screen, rng: Random): Bug = {
        val edge = rng.nextInt(4)
        val char = config.bugChars(rng.nextInt(config.bugChars.length))
        edge match {
          case 0 => Bug(rng.nextInt(context.screenWidth), 0, char)
          case 1 =>
            Bug(rng.nextInt(context.screenWidth), context.screenHeight - 1, char)
          case 2 => Bug(0, rng.nextInt(context.screenHeight), char)
          case _ =>
            Bug(context.screenWidth - 1, rng.nextInt(context.screenHeight), char)
        }
      }

      private def moveBug(
          bug: Bug,
          targetX: Int,
          targetY: Int,
          rng: Random
      ): Bug = {
        val moveTowardsTarget = rng.nextDouble() < 0.9

        val (dx, dy) = if (moveTowardsTarget) {
          val rawDx = if (targetX > bug.x) 1 else if (targetX < bug.x) -1 else 0
          val rawDy = if (targetY > bug.y) 1 else if (targetY < bug.y) -1 else 0
          if (rng.nextBoolean()) (rawDx, 0) else (0, rawDy)
        } else {
          val dirs = List(
            (-1, 0),
            (1, 0),
            (0, -1),
            (0, 1),
            (-1, -1),
            (1, 1),
            (-1, 1),
            (1, -1)
          )
          dirs(rng.nextInt(dirs.length))
        }

        bug.copy(x = bug.x + dx, y = bug.y + dy)
      }

      private def moveBugTowardsEdge(
          bug: Bug,
          context: Screen,
          rng: Random
      ): Bug = {
        val (targetX, targetY) = bug.targetEdge match {
          case 0 => (bug.x, -1)
          case 1 => (bug.x, context.screenHeight)
          case 2 => (-1, bug.y)
          case _ => (context.screenWidth, bug.y)
        }

        val moveTowards = rng.nextDouble() < 0.9
        if (moveTowards) {
          val dx = if (targetX > bug.x) 1 else if (targetX < bug.x) -1 else 0
          val dy = if (targetY > bug.y) 1 else if (targetY < bug.y) -1 else 0
          bug.copy(x = bug.x + dx, y = bug.y + dy)
        } else {
          val dirs = List(
            (-1, 0),
            (1, 0),
            (0, -1),
            (0, 1),
            (-1, -1),
            (1, 1),
            (-1, 1),
            (1, -1)
          )
          val (dx, dy) = dirs(rng.nextInt(dirs.length))
          bug.copy(x = bug.x + dx, y = bug.y + dy)
        }
      }

      private def findWordAt(
          lines: Array[String],
          x: Int,
          y: Int
      ): Option[(String, Int, Int)] =
        IdleAnsiSupport.findWordAt(lines, x, y)

      private def updateBugs(
          context: Screen,
          s: IdleOverlayState,
          now: Long,
          screenContent: String
      ): IdleOverlayState = {
        val rng = s.random

        val lines = screenContent.split("\n", -1)
        val linesWithStolen = applyStolen(lines, s.stolenWords)

        val textPositions: Array[(Int, Int)] =
          IdleAnsiSupport.textPositions(linesWithStolen)

        var newStolenWords = s.stolenWords

        val movedBugs = s.bugs.map { bug =>
          (0 until config.bugSpeed).foldLeft(bug) { (b, _) =>
            if (b.carrying.isDefined) {
              moveBugTowardsEdge(b, context, rng)
            } else {
              val wordHere = findWordAt(linesWithStolen, b.x, b.y)
              wordHere match {
                case Some((word, startCol, _)) =>
                  newStolenWords = StolenWord(b.y, startCol, word.length) :: newStolenWords
                  linesWithStolen.indices.find(_ == b.y).foreach { row =>
                    linesWithStolen(row) = IdleAnsiSupport
                      .applyStolen(
                        Array(linesWithStolen(row)),
                        List(StolenWord(0, startCol, word.length))
                      )
                      .head
                  }
                  b.copy(carrying = Some(word), targetEdge = rng.nextInt(4))
                case None =>
                  val (targetX, targetY) = if (textPositions.nonEmpty) {
                    textPositions(rng.nextInt(textPositions.length))
                  } else {
                    (context.screenWidth / 2, context.screenHeight / 2)
                  }
                  moveBug(b, targetX, targetY, rng)
              }
            }
          }
        }

        val aliveBugs =
          movedBugs.filter(bug =>
            bug.x >= 0 && bug.x < context.screenWidth && bug.y >= 0 && bug.y < context.screenHeight
          )

        val allGone = allTextStolen(lines, newStolenWords)

        if (allGone) {
          val targets = timeDisplayPositions(context, rng)
          val neededBugs = targets.length
          val currentBugs = aliveBugs.map(_.copy(carrying = None))
          val bugsForTime = if (currentBugs.length < neededBugs) {
            val extra = (0 until (neededBugs - currentBugs.length))
              .map(_ => spawnBugAtEdge(context, rng))
              .toList
            currentBugs ++ extra
          } else {
            currentBugs.take(neededBugs)
          }

          val movedToTime =
            bugsForTime.zip(targets).map { case (bug, (tx, ty, bChar)) =>
              val wobbleRange = config.timeWobbleMaxOffset * 2 + 1
              val wobbleX =
                if (rng.nextDouble() < config.timeWobbleApproachChance)
                  rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset
                else 0
              val wobbleY =
                if (rng.nextDouble() < config.timeWobbleApproachChance)
                  rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset
                else 0
              val dx = if (tx > bug.x) 1 else if (tx < bug.x) -1 else 0
              val dy = if (ty > bug.y) 1 else if (ty < bug.y) -1 else 0
              val atTarget =
                Math.abs(bug.x - tx) <= 1 && Math.abs(bug.y - ty) <= 1
              if (atTarget) {
                val wx =
                  tx + (if (rng.nextDouble() < config.timeWobbleAtTargetXChance)
                          rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset
                        else 0)
                val wy =
                  ty + (if (rng.nextDouble() < config.timeWobbleAtTargetYChance)
                          rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset
                        else 0)
                bug.copy(x = wx, y = wy, char = bChar)
              } else {
                bug.copy(
                  x = bug.x + dx + wobbleX,
                  y = bug.y + dy + wobbleY,
                  char = bChar
                )
              }
            }

          val leftover = if (currentBugs.length > neededBugs) {
            currentBugs
              .drop(neededBugs)
              .map(b =>
                moveBugTowardsEdge(
                  b.copy(targetEdge = if (b.targetEdge < 0) rng.nextInt(4) else b.targetEdge),
                  context,
                  rng
                )
              )
          } else List.empty

          val finalBugs = movedToTime ++ leftover
          s.copy(
            bugs = finalBugs,
            stolenWords = newStolenWords,
            lastSpawnTime = now,
            spawnCount = s.spawnCount
          )
        } else {
          val timeSinceIdleStart = now - s.idleStartTime
          val timeSinceLastSpawn = now - s.lastSpawnTime
          val shouldSpawnFirst =
            s.spawnCount == 0 && timeSinceIdleStart >= config.firstBugDelay.toMillis
          val shouldSpawnMore =
            s.spawnCount > 0 && timeSinceLastSpawn >= config.spawnInterval.toMillis

          val (finalBugs, newSpawnTime, newSpawnCount) =
            if ((shouldSpawnFirst || shouldSpawnMore) && aliveBugs.length < config.maxBugs) {
              val count = if (s.spawnCount == 0) 1 else config.bugsPerSpawn
              val spawned =
                (0 until count).map(_ => spawnBugAtEdge(context, rng)).toList
              (aliveBugs ++ spawned, now, s.spawnCount + 1)
            } else {
              (aliveBugs, s.lastSpawnTime, s.spawnCount)
            }

          s.copy(
            bugs = finalBugs,
            stolenWords = newStolenWords,
            lastSpawnTime = newSpawnTime,
            spawnCount = newSpawnCount
          )
        }
      }

      private val digitPatterns: Map[Char, Array[String]] = Map(
        '0' -> Array(
          "###",
          "# #",
          "# #",
          "# #",
          "###"
        ),
        '1' -> Array(" # ", "## ", " # ", " # ", "###"),
        '2' -> Array("###", "  #", "###", "#  ", "###"),
        '3' -> Array("###", "  #", "###", "  #", "###"),
        '4' -> Array("# #", "# #", "###", "  #", "  #"),
        '5' -> Array("###", "#  ", "###", "  #", "###"),
        '6' -> Array("###", "#  ", "###", "# #", "###"),
        '7' -> Array("###", "  #", " # ", " # ", " # "),
        '8' -> Array("###", "# #", "###", "# #", "###"),
        '9' -> Array("###", "# #", "###", "  #", "###"),
        ':' -> Array("   ", " # ", "   ", " # ", "   ")
      )

      private def timeDisplayPositions(
          context: Screen,
          rng: Random
      ): List[(Int, Int, String)] = {
        val time = {
          // Cross-platform time formatting (no java.time on Scala.js)
          val millis = System.currentTimeMillis()
          val totalSeconds = (millis / 1000) % 86400 // seconds since midnight UTC
          val hours = (totalSeconds / 3600) % 24
          val minutes = (totalSeconds % 3600) / 60
          val seconds = totalSeconds % 60
          f"$hours%02d:$minutes%02d:$seconds%02d"
        }
        val scale = 4
        val charWidth = (3 + 1) * scale
        val totalWidth = time.length * charWidth - scale
        val totalHeight = 5 * scale
        val startX = (context.screenWidth - totalWidth) / 2
        val startY = (context.screenHeight - totalHeight) / 2

        time.zipWithIndex.flatMap { case (ch, charIdx) =>
          val pattern = digitPatterns.getOrElse(ch, digitPatterns('0'))
          pattern.zipWithIndex.flatMap { case (row, rowIdx) =>
            row.zipWithIndex.collect {
              case (pixel, colIdx) if pixel != ' ' =>
                (0 until scale).flatMap { sy =>
                  (0 until scale).map { sx =>
                    val x = startX + charIdx * charWidth + colIdx * scale + sx
                    val y = startY + rowIdx * scale + sy
                    val bugChar =
                      config.bugChars(rng.nextInt(config.bugChars.length))
                    (x, y, bugChar)
                  }
                }
            }.flatten
          }
        }.toList
      }

      private def allTextStolen(
          lines: Array[String],
          stolenWords: List[StolenWord]
      ): Boolean = {
        val visible = applyStolen(lines, stolenWords)
        visible.forall(line => !IdleAnsiSupport.hasVisibleText(line))
      }

      private def applyStolen(
          lines: Array[String],
          stolenWords: List[StolenWord]
      ): Array[String] =
        IdleAnsiSupport.applyStolen(lines, stolenWords)

      private def normalizeLines(
          content: String,
          context: Screen
      ): Vector[String] = {
        val lines = content.split("\n", -1).toVector
        if (lines.length < context.screenHeight - 1) {
          lines ++ Vector.fill(context.screenHeight - 1 - lines.length)(
            " " * context.screenWidth
          )
        } else if (lines.length > context.screenHeight - 1) {
          lines.take(context.screenHeight - 1)
        } else {
          lines
        }
      }

      /** Apply stolen words to the screen content (bugs are rendered via GlideLayer). */
      private def applyIdleOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          stolenWords: List[StolenWord]
      ): ScreenAdjusted = {
        val lines = screenAdjusted.content.split("\n", -1)
        val blankedLines = applyStolen(lines, stolenWords)
        val padded = normalizeLines(blankedLines.mkString("\n"), context)
        screenAdjusted.copy(content = padded.mkString("\n"))
      }

      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): F[ScreenAdjusted] = for {
        _ <- idleDetector.onContentChange(originalContent.content)
        now <- Temporal[F].realTime.map(_.toMillis)
        idle <- idleDetector.isIdle
        result <-
          if (idle) {
            for {
              idleStart <- idleDetector.idleStartTime
              stepsAndProgress <- FixedStep.consumeSteps(stepperRef, IdleStep)
              (nrOfSteps, _) = stepsAndProgress
              currentState <- state.get
              updatedState = {
                val withIdleStart: IdleOverlayState = if (!currentState.wasIdle) {
                  currentState.copy(
                    wasIdle = true,
                    idleStartTime = idleStart.getOrElse(now),
                    lastSpawnTime = now
                  )
                } else currentState
                // Only advance the simulation on discrete steps (consistent timing)
                if (nrOfSteps > 0) {
                  (0 until nrOfSteps).foldLeft(withIdleStart) { (s, _) =>
                    updateBugs(context, s, now, screenAdjusted.content)
                  }
                } else withIdleStart
              }
              _ <- state.set(updatedState)
              bg = applyIdleOverlay(
                context,
                screenAdjusted,
                updatedState.stolenWords
              )
              // GlideLayer handles platform branching: smooth overlay on WebGL, grid composite on terminal
              result <-
                if (updatedState.bugs.nonEmpty)
                  glideLayer.renderOnto(bg, bugsToSmoothChars(updatedState.bugs))
                else
                  glideLayer.clear().as(bg)
            } yield result
          } else {
            for {
              s <- state.get
              _ <- state.update(s => if (s.wasIdle) IdleOverlayState() else s)
              _ <-
                if (s.wasIdle) {
                  glideLayer.clear() *> FixedStep.reset(stepperRef)
                } else Monad[F].unit
            } yield screenAdjusted
          }
      } yield result

      override def onUserInput(userInput: UserInput)(implicit F: cats.Applicative[F]): F[Unit] = {
        val _ = userInput
        F.unit
      }
    }
  }

}
