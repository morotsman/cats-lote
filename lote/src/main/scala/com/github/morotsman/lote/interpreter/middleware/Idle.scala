package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.{Clock, Ref, Temporal}
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Overlay}
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted, UserInput}

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

case class Bug(x: Int, y: Int, char: String, carrying: Option[String] = None, targetEdge: Int = -1)

case class StolenWord(row: Int, startCol: Int, length: Int)

case class IdleConfig(
                       idleTimeout: FiniteDuration = 2.minutes,
                       firstBugDelay: FiniteDuration = 0.seconds,
                       spawnInterval: FiniteDuration = 3.seconds,
                       bugsPerSpawn: Int = 4,
                       maxBugs: Int = 20,
                       bugChars: List[String] = List("@", "o", "*", "~", "#"),
                       bugSpeed: Int = 1,
                       // Time display wobble settings
                       timeWobbleApproachChance: Double = 0.1,   // chance of random wobble while approaching target
                       timeWobbleAtTargetXChance: Double = 0.05, // chance of X wobble once at target position
                       timeWobbleAtTargetYChance: Double = 0.05,  // chance of Y wobble once at target position
                       timeWobbleMaxOffset: Int = 1              // max offset in pixels for wobble (wobble range: -offset to +offset)
                     )

case class IdleState[F[_]](
                            lastSlideChangeTime: Long,
                            lastContentChangeTime: Long,
                            lastRawContent: Option[String] = None,
                            isIdle: Boolean = false,
                            idleStartTime: Long = 0,
                            bugs: List[Bug] = List.empty,
                            stolenWords: List[StolenWord] = List.empty,
                            lastSpawnTime: Long = 0,
                            spawnCount: Int = 0,
                            random: Random = new Random()
                          )

trait Idle[F[_]] extends Overlay[F] {
  def notifyActivity(): F[Unit]
}

object Idle {

  def make[F[_] : Monad : Clock : Temporal : Ref.Make : NConsole](
                                                                    config: IdleConfig = IdleConfig()
                                                                  ): F[Idle[F]] = for {
    now <- Clock[F].realTime.map(_.toMillis)
    state <- Ref[F].of(IdleState[F](lastSlideChangeTime = now, lastContentChangeTime = now))
  } yield new Idle[F] {

    override def notifyActivity(): F[Unit] = for {
      now <- Clock[F].realTime.map(_.toMillis)
      _ <- state.update(_.copy(
        lastSlideChangeTime = now,
        lastContentChangeTime = now,
        isIdle = false,
        bugs = List.empty,
        stolenWords = List.empty,
        spawnCount = 0
      ))
    } yield ()

    override def onKeyPress(input: UserInput): F[Unit] = notifyActivity()

    override def onContentChange(content: String): F[Unit] = for {
      s <- state.get
      now <- Clock[F].realTime.map(_.toMillis)
      contentChanged = s.lastRawContent.exists(_ != content)
      _ <- if (contentChanged) {
        state.update(_.copy(lastContentChangeTime = now, lastRawContent = Some(content), isIdle = false, bugs = List.empty, stolenWords = List.empty, spawnCount = 0))
      } else {
        state.update(_.copy(lastRawContent = Some(content)))
      }
    } yield ()

    private def checkIdle: F[Boolean] = for {
      s <- state.get
      now <- Clock[F].realTime.map(_.toMillis)
      lastActivity = Math.max(s.lastSlideChangeTime, s.lastContentChangeTime)
      elapsed = FiniteDuration(now - lastActivity, TimeUnit.MILLISECONDS)
    } yield elapsed >= config.idleTimeout

    private def spawnBugAtEdge(context: Screen, rng: Random): Bug = {
      val edge = rng.nextInt(4) // 0=top, 1=bottom, 2=left, 3=right
      val char = config.bugChars(rng.nextInt(config.bugChars.length))
      edge match {
        case 0 => Bug(rng.nextInt(context.screenWidth), 0, char)
        case 1 => Bug(rng.nextInt(context.screenWidth), context.screenHeight - 1, char)
        case 2 => Bug(0, rng.nextInt(context.screenHeight), char)
        case _ => Bug(context.screenWidth - 1, rng.nextInt(context.screenHeight), char)
      }
    }

    private def moveBug(bug: Bug, targetX: Int, targetY: Int, rng: Random): Bug = {
      // 70% chance to move towards target, 30% chance to move in a random direction
      val moveTowardsTarget = rng.nextDouble() < 0.8

      val (dx, dy) = if (moveTowardsTarget) {
        val rawDx = if (targetX > bug.x) 1 else if (targetX < bug.x) -1 else 0
        val rawDy = if (targetY > bug.y) 1 else if (targetY < bug.y) -1 else 0
        // Sometimes only move in one axis
        if (rng.nextBoolean()) (rawDx, 0) else (0, rawDy)
      } else {
        // Random direction
        val dirs = List((-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, 1), (-1, 1), (1, -1))
        dirs(rng.nextInt(dirs.length))
      }

      bug.copy(x = bug.x + dx, y = bug.y + dy)
    }

    private def moveBugTowardsEdge(bug: Bug, context: Screen, rng: Random): Bug = {
      // Use the stored target edge direction
      val (targetX, targetY) = bug.targetEdge match {
        case 0 => (bug.x, -1)              // top
        case 1 => (bug.x, context.screenHeight) // bottom
        case 2 => (-1, bug.y)              // left
        case _ => (context.screenWidth, bug.y) // right
      }

      // 90% towards target edge, 10% random wobble
      val moveTowards = rng.nextDouble() < 0.9
      if (moveTowards) {
        val dx = if (targetX > bug.x) 1 else if (targetX < bug.x) -1 else 0
        val dy = if (targetY > bug.y) 1 else if (targetY < bug.y) -1 else 0
        bug.copy(x = bug.x + dx, y = bug.y + dy)
      } else {
        val dirs = List((-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, 1), (-1, 1), (1, -1))
        val (dx, dy) = dirs(rng.nextInt(dirs.length))
        bug.copy(x = bug.x + dx, y = bug.y + dy)
      }
    }

    /** Find the word at the given position in the lines */
    private def findWordAt(lines: Array[String], x: Int, y: Int): Option[(String, Int, Int)] = {
      if (y < 0 || y >= lines.length) return None
      val line = lines(y)
      if (x < 0 || x >= line.length || line.charAt(x) == ' ') return None

      // Expand left and right to find word boundaries
      var start = x
      while (start > 0 && line.charAt(start - 1) != ' ') start -= 1
      var end = x
      while (end < line.length - 1 && line.charAt(end + 1) != ' ') end += 1

      val word = line.substring(start, end + 1)
      if (word.trim.nonEmpty) Some((word, start, end + 1)) else None
    }

    private def updateBugs(context: Screen, s: IdleState[F], now: Long, screenContent: String): IdleState[F] = {
      val rng = s.random

      // Apply stolen words to get current visible content
      val lines = screenContent.split("\n", -1)
      val linesWithStolen = applyStolen(lines, s.stolenWords)

      // Find all non-space character positions in the modified content as targets
      val textPositions: Array[(Int, Int)] = linesWithStolen.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.collect { case (ch, x) if ch != ' ' => (x, y) }
      }

      var newStolenWords = s.stolenWords

      // Move existing bugs (multiple steps per tick based on bugSpeed)
      val movedBugs = s.bugs.map { bug =>
        (0 until config.bugSpeed).foldLeft(bug) { (b, _) =>
          if (b.carrying.isDefined) {
            // Bug is carrying a word, move towards nearest edge
            moveBugTowardsEdge(b, context, rng)
          } else {
            // Check if bug is on a word it can steal
            val wordHere = findWordAt(linesWithStolen, b.x, b.y)
            wordHere match {
              case Some((word, startCol, _)) =>
                // Steal the word!
                newStolenWords = StolenWord(b.y, startCol, word.length) :: newStolenWords
                // Blank out the word in linesWithStolen so other bugs don't target it
                if (b.y < linesWithStolen.length) {
                  linesWithStolen(b.y) = linesWithStolen(b.y).take(startCol) + (" " * word.length) + linesWithStolen(b.y).drop(startCol + word.length)
                }
                b.copy(carrying = Some(word), targetEdge = rng.nextInt(4))
              case None =>
                // Move towards text
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

      // Remove bugs that have left the screen (they've dragged their word off)
      val aliveBugs = movedBugs.filter(bug =>
        bug.x >= 0 && bug.x < context.screenWidth && bug.y >= 0 && bug.y < context.screenHeight
      )

      // Check if all text has been stolen - switch to time display mode
      val allGone = allTextStolen(lines, newStolenWords)

      if (allGone) {
        // Generate target positions for the current time
        val targets = timeDisplayPositions(context, rng)

        // Ensure we have enough bugs to display the time
        val neededBugs = targets.length
        val currentBugs = aliveBugs.map(_.copy(carrying = None)) // drop carried words
        val bugsForTime = if (currentBugs.length < neededBugs) {
          // Spawn additional bugs at random edges
          val extra = (0 until (neededBugs - currentBugs.length)).map(_ => spawnBugAtEdge(context, rng)).toList
          currentBugs ++ extra
        } else {
          currentBugs.take(neededBugs)
        }

        // Move each bug towards its assigned time-position with wobble
        val movedToTime = bugsForTime.zip(targets).map { case (bug, (tx, ty, bChar)) =>
          val wobbleRange = config.timeWobbleMaxOffset * 2 + 1
          val wobbleX = if (rng.nextDouble() < config.timeWobbleApproachChance) rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset else 0
          val wobbleY = if (rng.nextDouble() < config.timeWobbleApproachChance) rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset else 0
          val dx = if (tx > bug.x) 1 else if (tx < bug.x) -1 else 0
          val dy = if (ty > bug.y) 1 else if (ty < bug.y) -1 else 0
          // Once near target, add continuous wobble
          val atTarget = Math.abs(bug.x - tx) <= 1 && Math.abs(bug.y - ty) <= 1
          if (atTarget) {
            val wx = tx + (if (rng.nextDouble() < config.timeWobbleAtTargetXChance) rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset else 0)
            val wy = ty + (if (rng.nextDouble() < config.timeWobbleAtTargetYChance) rng.nextInt(wobbleRange) - config.timeWobbleMaxOffset else 0)
            bug.copy(x = wx, y = wy, char = bChar)
          } else {
            bug.copy(x = bug.x + dx + wobbleX, y = bug.y + dy + wobbleY, char = bChar)
          }
        }

        // Keep any leftover bugs wandering off-screen
        val leftover = if (currentBugs.length > neededBugs) {
          currentBugs.drop(neededBugs).map(b => moveBugTowardsEdge(b.copy(targetEdge = if (b.targetEdge < 0) rng.nextInt(4) else b.targetEdge), context, rng))
        } else List.empty

        val finalBugs = movedToTime ++ leftover
        s.copy(bugs = finalBugs, stolenWords = newStolenWords, lastSpawnTime = now, spawnCount = s.spawnCount)
      } else {
        // Normal mode: keep stealing text
        // Check if we should spawn new bugs
        val timeSinceIdleStart = now - s.idleStartTime
        val timeSinceLastSpawn = now - s.lastSpawnTime
        val shouldSpawnFirst = s.spawnCount == 0 && timeSinceIdleStart >= config.firstBugDelay.toMillis
        val shouldSpawnMore = s.spawnCount > 0 && timeSinceLastSpawn >= config.spawnInterval.toMillis

        val (finalBugs, newSpawnTime, newSpawnCount) = if ((shouldSpawnFirst || shouldSpawnMore) && aliveBugs.length < config.maxBugs) {
          val count = if (s.spawnCount == 0) 1 else config.bugsPerSpawn
          val spawned = (0 until count).map(_ => spawnBugAtEdge(context, rng)).toList
          (aliveBugs ++ spawned, now, s.spawnCount + 1)
        } else {
          (aliveBugs, s.lastSpawnTime, s.spawnCount)
        }

        s.copy(bugs = finalBugs, stolenWords = newStolenWords, lastSpawnTime = newSpawnTime, spawnCount = newSpawnCount)
      }
    }

    /** Simple 3x5 pixel font for digits and colon */
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

    /** Generate target positions for bugs to form the current time display */
    private def timeDisplayPositions(context: Screen, rng: Random): List[(Int, Int, String)] = {
      val time = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"))
      val scale = 2
      val charWidth = (3 + 1) * scale // (3 pixels + 1 gap) * scale
      val totalWidth = time.length * charWidth - scale
      val totalHeight = 5 * scale
      val startX = (context.screenWidth - totalWidth) / 2
      val startY = (context.screenHeight - totalHeight) / 2

      time.zipWithIndex.flatMap { case (ch, charIdx) =>
        val pattern = digitPatterns.getOrElse(ch, digitPatterns('0'))
        pattern.zipWithIndex.flatMap { case (row, rowIdx) =>
          row.zipWithIndex.collect { case (pixel, colIdx) if pixel != ' ' =>
            // Each pixel becomes a scale x scale block
            (0 until scale).flatMap { sy =>
              (0 until scale).map { sx =>
                val x = startX + charIdx * charWidth + colIdx * scale + sx
                val y = startY + rowIdx * scale + sy
                val bugChar = config.bugChars(rng.nextInt(config.bugChars.length))
                (x, y, bugChar)
              }
            }
          }.flatten
        }
      }.toList
    }

    /** Check if all visible text has been stolen */
    private def allTextStolen(lines: Array[String], stolenWords: List[StolenWord]): Boolean = {
      val visible = applyStolen(lines, stolenWords)
      visible.forall(_.trim.isEmpty)
    }

    private def applyStolen(lines: Array[String], stolenWords: List[StolenWord]): Array[String] = {
      val result = lines.clone()
      stolenWords.foreach { sw =>
        if (sw.row >= 0 && sw.row < result.length) {
          val line = result(sw.row)
          if (sw.startCol < line.length) {
            val end = Math.min(sw.startCol + sw.length, line.length)
            result(sw.row) = line.take(sw.startCol) + (" " * (end - sw.startCol)) + line.drop(end)
          }
        }
      }
      result
    }

    private def normalizeLines(content: String, context: Screen): Vector[String] = {
      val lines = content.split("\n", -1).toVector
      if (lines.length < context.screenHeight - 1) {
        lines ++ Vector.fill(context.screenHeight - 1 - lines.length)(" " * context.screenWidth)
      } else if (lines.length > context.screenHeight - 1) {
        lines.take(context.screenHeight - 1)
      } else {
        lines
      }
    }

    private def applyIdleOverlay(context: Screen, screenAdjusted: ScreenAdjusted, bugs: List[Bug], stolenWords: List[StolenWord]): ScreenAdjusted = {
      // First blank out stolen words from the content
      val lines = screenAdjusted.content.split("\n", -1)
      val blankedLines = applyStolen(lines, stolenWords)
      val padded = normalizeLines(blankedLines.mkString("\n"), context)

      // Render bugs (with their carried words shown next to them)
      val withBugs = bugs.foldLeft(padded) { (acc, bug) =>
        if (bug.y >= 0 && bug.y < acc.length && bug.x >= 0) {
          val line = acc(bug.y)
          val displayStr = bug.carrying match {
            case Some(word) => bug.char + word
            case None => bug.char
          }
          if (bug.x < line.length) {
            val updatedLine = line.take(bug.x) + displayStr + line.drop(bug.x + displayStr.length)
            // Ensure line doesn't exceed screen width
            val trimmed = if (updatedLine.length > context.screenWidth) updatedLine.take(context.screenWidth) else updatedLine
            acc.updated(bug.y, trimmed)
          } else acc
        } else acc
      }

      screenAdjusted.copy(content = withBugs.mkString("\n"))
    }

    override def applyOverlay(context: Screen, screenAdjusted: ScreenAdjusted): F[ScreenAdjusted] = for {
      now <- Clock[F].realTime.map(_.toMillis)
      idle <- checkIdle
      result <- if (idle) {
        for {
          currentState <- state.get
          updatedState = {
            val withIdleStart: IdleState[F] = if (!currentState.isIdle) {
              currentState.copy(isIdle = true, idleStartTime = now, lastSpawnTime = now)
            } else currentState
            updateBugs(context, withIdleStart, now, screenAdjusted.content)
          }
          _ <- state.set(updatedState)
        } yield applyIdleOverlay(context, screenAdjusted, updatedState.bugs, updatedState.stolenWords)
      } else {
        Monad[F].pure(screenAdjusted)
      }
    } yield result
  }

}
