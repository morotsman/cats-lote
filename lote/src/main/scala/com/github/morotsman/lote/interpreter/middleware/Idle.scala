package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.{Clock, Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{IdleDetector, Overlay}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted}

import java.time.LocalTime
import java.time.format.DateTimeFormatter
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

case class IdleOverlayConfig(
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

trait Idle[F[_]] extends Overlay[F]

object Idle {

  def make[F[_]: Temporal: Ref.Make](
      idleDetector: IdleDetector[F],
      config: IdleOverlayConfig = IdleOverlayConfig()
  ): F[Idle[F]] = for {
    state <- Ref[F].of(IdleOverlayState())
  } yield new Idle[F] {

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
    ): Option[(String, Int, Int)] = {
      if (y < 0 || y >= lines.length) return None
      val line = lines(y)
      if (x < 0 || x >= line.length || line.charAt(x) == ' ') return None

      var start = x
      while (start > 0 && line.charAt(start - 1) != ' ') start -= 1
      var end = x
      while (end < line.length - 1 && line.charAt(end + 1) != ' ') end += 1

      val word = line.substring(start, end + 1)
      if (word.trim.nonEmpty) Some((word, start, end + 1)) else None
    }

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
        linesWithStolen.zipWithIndex.flatMap { case (line, y) =>
          line.zipWithIndex.collect { case (ch, x) if ch != ' ' => (x, y) }
        }

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
                if (b.y < linesWithStolen.length) {
                  linesWithStolen(b.y) = linesWithStolen(b.y).take(
                    startCol
                  ) + (" " * word.length) + linesWithStolen(b.y).drop(
                    startCol + word.length
                  )
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
        movedBugs.filter(bug => bug.x >= 0 && bug.x < context.screenWidth && bug.y >= 0 && bug.y < context.screenHeight)

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
      '0' -> Array("###", "# #", "# #", "# #", "###"),
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
      val time = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"))
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
      visible.forall(_.trim.isEmpty)
    }

    private def applyStolen(
        lines: Array[String],
        stolenWords: List[StolenWord]
    ): Array[String] = {
      val result = lines.clone()
      stolenWords.foreach { sw =>
        if (sw.row >= 0 && sw.row < result.length) {
          val line = result(sw.row)
          if (sw.startCol < line.length) {
            val end = Math.min(sw.startCol + sw.length, line.length)
            result(sw.row) = line.take(sw.startCol) + (" " * (end - sw.startCol)) + line.drop(
              end
            )
          }
        }
      }
      result
    }

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

    private def applyIdleOverlay(
        context: Screen,
        screenAdjusted: ScreenAdjusted,
        bugs: List[Bug],
        stolenWords: List[StolenWord]
    ): ScreenAdjusted = {
      val lines = screenAdjusted.content.split("\n", -1)
      val blankedLines = applyStolen(lines, stolenWords)
      val padded = normalizeLines(blankedLines.mkString("\n"), context)

      val withBugs = bugs.foldLeft(padded) { (acc, bug) =>
        if (bug.y >= 0 && bug.y < acc.length && bug.x >= 0) {
          val line = acc(bug.y)
          val displayStr = bug.carrying match {
            case Some(word) => bug.char + word
            case None       => bug.char
          }
          if (bug.x < line.length) {
            val updatedLine = line.take(bug.x) + displayStr + line.drop(
              bug.x + displayStr.length
            )
            val trimmed =
              if (updatedLine.length > context.screenWidth)
                updatedLine.take(context.screenWidth)
              else updatedLine
            acc.updated(bug.y, trimmed)
          } else acc
        } else acc
      }

      screenAdjusted.copy(content = withBugs.mkString("\n"))
    }

    override def applyOverlay(
        context: Screen,
        screenAdjusted: ScreenAdjusted,
        originalContent: ScreenAdjusted
    ): F[ScreenAdjusted] = for {
      // TODO should we not be idle if there is an animated slide going on? Think about it
      _ <- idleDetector.onContentChange(originalContent.content)
      now <- Clock[F].realTime.map(_.toMillis)
      idle <- idleDetector.isIdle
      result <-
        if (idle) {
          for {
            idleStart <- idleDetector.idleStartTime
            currentState <- state.get
            updatedState = {
              val withIdleStart: IdleOverlayState = if (!currentState.wasIdle) {
                currentState.copy(
                  wasIdle = true,
                  idleStartTime = idleStart.getOrElse(now),
                  lastSpawnTime = now
                )
              } else currentState
              updateBugs(context, withIdleStart, now, screenAdjusted.content)
            }
            _ <- state.set(updatedState)
          } yield applyIdleOverlay(
            context,
            screenAdjusted,
            updatedState.bugs,
            updatedState.stolenWords
          )
        } else {
          state.update(s => if (s.wasIdle) IdleOverlayState() else s) *>
            Monad[F].pure(screenAdjusted)
        }
    } yield result
  }

}
