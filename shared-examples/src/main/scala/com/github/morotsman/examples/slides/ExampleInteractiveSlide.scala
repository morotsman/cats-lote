package com.github.morotsman.examples.slides

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Character, ScreenAdjusted}
import com.github.morotsman.lote.api.builders.ContextualF
import com.github.morotsman.lote.api.support.{AnimationClock, SmoothChar, TickedSlide}
import com.github.morotsman.lote.api.spi.{NConsole, Slide, Ticker}

import scala.util.Random

object ExampleInteractiveSlide {

  /** Example custom `Slide[F]` using `TickedSlide` to eliminate ticker/FixedStep/GlideLayer boilerplate.
    *
    * Game state, the input queue, and rendering logic are captured as closures in the `TickedSlide` callbacks.
    */

  def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
    TickedSlide.contextual[F] { builder =>
      create[F](builder.console, builder.ticker, builder.animationSettings)
    }

  def create[F[_]: Temporal: Ref.Make](
      console: NConsole[F],
      ticker: Ticker[F],
      animationSettings: AnimationSettings
  )(implicit clock: AnimationClock[F]): F[Slide[F]] =
    for {
      queue <- Queue.unbounded[F, Direction]
      stateRef <- Ref[F].of(Option.empty[AnimatorState])
      slide <- TickedSlide[F](console, ticker, animationSettings)
        .withGlideLayer()
        .buildWithGlide(
          onTick = { (nrOfSteps, glide) =>
            for {
              maybeState <- stateRef.get
              _ <- maybeState.traverse_ { s =>
                if (!s.running) Monad[F].unit
                else if (hasSelfCollision(s.worm) || hasBlockCollision(s.worm, s.blockIndexes)) {
                  for {
                    screen <- console.context
                    _ <- glide.clear()
                    _ <- console.writeString(renderDeathScreen(screen.screenWidth, screen.screenHeight, s.score))
                    _ <- stateRef.update(_.map(_.copy(running = false)))
                  } yield ()
                } else if (nrOfSteps <= 0) {
                  glide.render(wormToSmoothChars(s.worm, s.screenWidth))
                } else {
                  for {
                    screen <- console.context
                    updatedState <- (0 until nrOfSteps).toList.foldLeftM(s) {
                      case (currentState, _) if !currentState.running =>
                        Monad[F].pure(currentState)
                      case (currentState, _) if hasSelfCollision(currentState.worm) || hasBlockCollision(currentState.worm, currentState.blockIndexes) =>
                        Monad[F].pure(currentState.copy(running = false))
                      case (currentState, _) =>
                        queue.tryTake.map { maybeUserInput =>
                          updateWormState(currentState, maybeUserInput, screen.screenWidth)
                        }
                    }
                    _ <-
                      if (updatedState.running) {
                        val bg = renderBackground(updatedState, screen.screenWidth)
                        glide
                          .renderOnto(bg, wormToSmoothChars(updatedState.worm, screen.screenWidth))
                          .flatMap(console.writeString)
                      } else {
                        for {
                          _ <- glide.clear()
                          _ <- console.writeString(renderDeathScreen(screen.screenWidth, screen.screenHeight, updatedState.score))
                        } yield ()
                      }
                    _ <- stateRef.set(Some(updatedState))
                  } yield ()
                }
              }
            } yield ()
          },
          onInput = {
            case Character(c) if c == 'a' => queue.offer(DirectionLeft())
            case Character(c) if c == 'w' => queue.offer(DirectionUp())
            case Character(c) if c == 's' => queue.offer(DirectionDown())
            case Character(c) if c == 'd' => queue.offer(DirectionRight())
            case _                        => Monad[F].unit
          },
          onStart = for {
            screen <- console.context
            screenSize = screen.screenWidth * (screen.screenHeight - 1)
            emptyScreen = Vector.fill(screenSize)(' ')
            heartIndexes = List.fill(500)(Random.nextInt(screenSize)).toSet
            blockIndexes = List.fill(50)(Random.nextInt(screenSize)).toSet -- heartIndexes
            message = "WASD is your friend"
            initialWorm = Worm(message.zipWithIndex.map { case (c, index) =>
              WormSegment(screenSize / 2 + index, DirectionLeft(), c)
            }.toVector)
            wormPositions = initialWorm.segments.map(_.index).toSet
            safeBlockIndexes = blockIndexes -- wormPositions
            _ <- stateRef.set(
              Some(
                AnimatorState(
                  emptyScreen,
                  initialWorm,
                  heartIndexes,
                  safeBlockIndexes,
                  screen.screenWidth,
                  score = 0,
                  running = true
                )
              )
            )
          } yield ()
        )
    } yield slide

  // ── Game logic (pure) ────────────────────────────────────────────

  private def growWorm(
      worm: Worm,
      collisions: Set[Int],
      screenWidth: Int
  ): Worm =
    if (collisions.nonEmpty) {
      val last = worm.segments.last
      Worm(segments =
        worm.segments :+ last.copy(
          index = last.direction match {
            case DirectionLeft()  => last.index + 1
            case DirectionRight() => last.index - 1
            case DirectionUp()    => last.index + screenWidth
            case DirectionDown()  => last.index - screenWidth
            case NoDirection()    => last.index
          },
          symbol = '♥'
        )
      )
    } else {
      worm
    }

  private def moveWorm(
      worm: Worm,
      newHeadDirection: Direction,
      screenWidth: Int,
      screenLength: Int
  ): Worm =
    worm.copy(segments =
      worm.segments
        .foldLeft((newHeadDirection, Vector[WormSegment]())) { case ((newDirection, acc), oldSegment) =>
          val newSegment = oldSegment.copy(
            direction = newDirection,
            index = oldSegment.direction match {
              case DirectionLeft() =>
                if (oldSegment.index % screenWidth == 0) {
                  oldSegment.index + screenWidth - 1
                } else {
                  oldSegment.index - 1
                }
              case DirectionRight() =>
                if (oldSegment.index % screenWidth == screenWidth - 1) {
                  oldSegment.index - screenWidth + 1
                } else {
                  oldSegment.index + 1
                }
              case DirectionUp() =>
                if (oldSegment.index - screenWidth > -1) {
                  oldSegment.index - screenWidth
                } else {
                  screenLength + (oldSegment.index - screenWidth)
                }
              case DirectionDown() =>
                if (oldSegment.index + screenWidth < screenLength) {
                  oldSegment.index + screenWidth
                } else {
                  screenWidth + oldSegment.index - screenLength
                }
              case NoDirection() =>
                oldSegment.index
            }
          )
          (oldSegment.direction, newSegment +: acc)
        }
        ._2
        .reverse
    )

  private def updateWormState(
      s: AnimatorState,
      maybeUserInput: Option[Direction],
      screenWidth: Int
  ): AnimatorState = {
    val collisions =
      s.worm.segments.map(_.index).toSet.intersect(s.heartIndexes)
    val updatedHeartIndexes = s.heartIndexes.removedAll(collisions)
    val updatedScore = s.score + collisions.size
    val wormWithHearts = growWorm(s.worm, collisions, screenWidth)
    val headDirection = maybeUserInput.orElse(
      wormWithHearts.segments.headOption.map(_.direction)
    )
    val updatedWorm = headDirection.fold(wormWithHearts) { dir =>
      moveWorm(wormWithHearts, dir, screenWidth, s.emptyScreen.length)
    }
    val hitBlock = updatedWorm.segments.headOption.exists(seg => s.blockIndexes.contains(seg.index))
    if (hitBlock)
      s.copy(worm = updatedWorm, heartIndexes = updatedHeartIndexes, score = updatedScore, running = false)
    else
      s.copy(worm = updatedWorm, heartIndexes = updatedHeartIndexes, score = updatedScore)
  }

  private def hasSelfCollision(worm: Worm): Boolean =
    worm.segments.map(_.index).size != worm.segments.map(_.index).toSet.size

  private def hasBlockCollision(worm: Worm, blockIndexes: Set[Int]): Boolean =
    worm.segments.headOption.exists(seg => blockIndexes.contains(seg.index))

  // ── Rendering ────────────────────────────────────────────────────

  private val youDiedArt: String =
    """
      |
      |
      |▓██   ██▓ ▒█████   █    ██    ▓█████▄  ██▓▓█████ ▓█████▄
      | ▒██  ██▒▒██▒  ██▒ ██  ▓██▒   ▒██▀ ██▌▓██▒▓█   ▀ ▒██▀ ██▌
      |  ▒██ ██░▒██░  ██▒▓██  ▒██░   ░██   █▌▒██▒▒███   ░██   █▌
      |  ░ ▐██▓░▒██   ██░▓▓█  ░██░   ░▓█▄   ▌░██░▒▓█  ▄ ░▓█▄   ▌
      |  ░ ██▒▓░░ ████▓▒░▒▒█████▓    ░▒████▓ ░██░░▒████▒ ░▒████▓
      |   ██▒▒▒ ░ ▒░▒░▒░ ░▒▓▒ ▒ ▒    ▒▒▓  ▒ ░▓  ░░ ▒░ ░  ▒▒▓  ▒
      | ▓██ ░▒░   ░ ▒ ▒░ ░░▒░ ░ ░    ░ ▒  ▒  ▒ ░ ░ ░  ░  ░ ▒  ▒
      | ▒ ▒ ░░  ░ ░ ░ ▒   ░░░ ░ ░    ░ ░  ░  ▒ ░   ░     ░ ░  ░
      | ░ ░       ░ ░       ░            ░  ░       ░  ░      ░
      | ░ ░                            ░                    ░
      |
      |""".stripMargin

  private def renderDeathScreen(
      screenWidth: Int,
      screenHeight: Int,
      score: Int
  ): ScreenAdjusted = {
    val scoreText = s"Score: $score"
    val artWithScore = youDiedArt + "\n" + scoreText
    val artLines = artWithScore.split("\n", -1).toVector
    val artHeight = artLines.length
    val artWidth = artLines.map(_.length).maxOption.getOrElse(0)
    val topPad = math.max(0, (screenHeight - artHeight) / 2)
    val leftPad = math.max(0, (screenWidth - artWidth) / 2)

    def fitLine(line: String): String = {
      val padded = (" " * leftPad) + line
      val truncated = padded.take(screenWidth)
      truncated + (" " * math.max(0, screenWidth - truncated.length))
    }

    val emptyLine = " " * screenWidth
    val lines = Vector.fill(topPad)(emptyLine) ++
      artLines.map(fitLine) ++
      Vector.fill(math.max(0, screenHeight - topPad - artHeight))(emptyLine)

    ScreenAdjusted(lines.take(screenHeight).mkString("\n"))
  }

  private val AnsiRed = "\u001b[31m"
  private val AnsiReset = "\u001b[0m"

  private def renderBackground(
      s: AnimatorState,
      screenWidth: Int
  ): ScreenAdjusted = {
    val scoreLabel = s"Score: ${s.score}"
    val scoreLabelIndexes = scoreLabel.indices.toSet

    val rendered = s.emptyScreen.indices.map { index =>
      if (scoreLabelIndexes.contains(index)) scoreLabel(index).toString
      else if (s.blockIndexes.contains(index)) s"${AnsiRed}█${AnsiReset}"
      else if (s.heartIndexes.contains(index)) "♥"
      else " "
    }

    ScreenAdjusted(
      rendered.grouped(screenWidth).map(_.mkString).mkString("\n")
    )
  }

  private def wormToSmoothChars(worm: Worm, screenWidth: Int): Vector[SmoothChar] =
    worm.segments.map { seg =>
      SmoothChar(seg.symbol, seg.index % screenWidth, seg.index / screenWidth)
    }
}

case class WormSegment(index: Int, direction: Direction, symbol: Char)

case class Worm(segments: Vector[WormSegment])

case class AnimatorState(
    emptyScreen: Vector[Char],
    worm: Worm,
    heartIndexes: Set[Int],
    blockIndexes: Set[Int],
    screenWidth: Int,
    score: Int,
    running: Boolean
)
