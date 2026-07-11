package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted}
import com.github.morotsman.lote.util.Colors

case class Milestone(label: String, slideIndex: Int)

trait ProgressBar[F[_]] extends Overlay[F] {
  def setCurrentSlide(index: Int): F[Unit]
}

object ProgressBar {

  private def maxRenderableColumn(screenWidth: Int): Int =
    if (screenWidth <= 1) 0 else screenWidth - 2

  private def maxMilestoneStart(screenWidth: Int, labelLength: Int): Int =
    if (screenWidth <= labelLength) 0
    else Math.max(0, screenWidth - labelLength - 1)

  private def anchorPositions(
      screenWidth: Int,
      totalSlides: Int
  ): Vector[Int] =
    if (totalSlides <= 0 || screenWidth <= 0) Vector.empty
    else if (totalSlides == 1) Vector(Math.min(screenWidth / 2, maxRenderableColumn(screenWidth)))
    else
      (0 until totalSlides).toVector.map { index =>
        Math.round(index.toDouble * maxRenderableColumn(screenWidth).toDouble / (totalSlides - 1).toDouble).toInt
      }

  private def milestoneStartPositions(
      screenWidth: Int,
      totalSlides: Int,
      milestones: List[Milestone]
  ): Vector[Int] = {
    val labels = milestones.map(_.label)
    val lengths = labels.map(_.length).toVector
    val anchors = anchorPositions(screenWidth, totalSlides)

    val idealStarts = milestones.map { m =>
      val anchor = anchors.lift(m.slideIndex).getOrElse(0)
      val preferred = anchor - m.label.length / 2
      Math.max(0, Math.min(maxMilestoneStart(screenWidth, m.label.length), preferred))
    }.toVector

    val leftAdjusted = idealStarts.indices.foldLeft(Vector.empty[Int]) { (acc, idx) =>
      val start = idealStarts(idx)
      val minStart =
        acc.lastOption.map(previous => previous + lengths(idx - 1) + 1).getOrElse(0)
      acc :+ Math.max(start, minStart)
    }

    leftAdjusted.indices.reverse.foldLeft(leftAdjusted) { (positions, idx) =>
      val maxStart =
        if (idx == positions.length - 1) maxMilestoneStart(screenWidth, lengths(idx))
        else Math.max(0, positions(idx + 1) - lengths(idx) - 1)
      positions.updated(idx, Math.min(positions(idx), maxStart))
    }
  }

  def make[F[_]: Monad: Ref.Make](
      totalSlides: Int,
      milestones: List[Milestone] = List.empty
  ): F[ProgressBar[F]] = Ref[F].of(0).map { currentSlideRef =>
    new ProgressBar[F] {

      override def setCurrentSlide(index: Int): F[Unit] =
        currentSlideRef.set(index)

      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): F[ScreenAdjusted] = for {
        currentIndex <- currentSlideRef.get
      } yield {
        // ANSI color codes
        val anchors = anchorPositions(context.screenWidth, totalSlides)
        val centered = {
          val cells = Array.fill(context.screenWidth)(" ")
          anchors.zipWithIndex.dropRight(1).foreach { case (leftPos, i) =>
            val rightPos = anchors(i + 1)
            val line =
              if (i < currentIndex) Colors.bright + "━" + Colors.reset
              else Colors.gray + "─" + Colors.reset
            ((leftPos + 1) until rightPos).foreach { pos =>
              if (pos >= 0 && pos < cells.length) cells(pos) = line
            }
          }
          anchors.zipWithIndex.foreach { case (pos, i) =>
            if (pos >= 0 && pos < cells.length) {
              cells(pos) =
                if (i < currentIndex) Colors.bright + "●" + Colors.reset
                else if (i == currentIndex) Colors.bold + Colors.bright + "◉" + Colors.reset
                else Colors.gray + "○" + Colors.reset
            }
          }
          cells.mkString
        }

        // Determine which milestone is active based on current slide
        val sortedMilestones = milestones.sortBy(_.slideIndex)

        def isActiveMilestone(m: Milestone): Boolean = {
          val mIdx = sortedMilestones.indexOf(m)
          val start = m.slideIndex
          val end =
            if (mIdx < sortedMilestones.length - 1)
              sortedMilestones(mIdx + 1).slideIndex
            else totalSlides
          currentIndex >= start && currentIndex < end
        }

        // Build milestone row with colors
        val milestoneRow = if (milestones.nonEmpty) {
          val positions = milestoneStartPositions(
            context.screenWidth,
            totalSlides,
            sortedMilestones
          )
          val parts = sortedMilestones.zip(positions).map { case (m, pos) =>
            val color = if (isActiveMilestone(m)) Colors.bright else Colors.gray
            (pos, color + m.label + Colors.reset)
          }
          // Build the row by inserting colored labels at correct positions
          val sb = new StringBuilder
          var currentPos = 0
          parts.foreach { case (pos, coloredLabel) =>
            if (pos > currentPos) {
              sb.append(" " * (pos - currentPos))
              currentPos = pos
            }
            sb.append(coloredLabel)
            // Advance currentPos by the visible label length (without ANSI codes)
            val visibleLen =
              coloredLabel.replaceAll("\u001b\\[[0-9;]*m", "").length
            currentPos += visibleLen
          }
          if (currentPos < context.screenWidth) {
            sb.append(" " * (context.screenWidth - currentPos))
          }
          sb.toString()
        } else {
          " " * context.screenWidth
        }

        val lines = screenAdjusted.content.split("\n", -1).toVector
        val progressRowIndex = context.screenHeight - 3
        val milestoneRowIndex = progressRowIndex - 1

        val padded = if (lines.length <= progressRowIndex) {
          lines ++ Vector.fill(progressRowIndex - lines.length + 1)(
            " " * context.screenWidth
          )
        } else {
          lines
        }


        val updatedLines = padded
          .updated(progressRowIndex, centered)
          .updated(milestoneRowIndex, milestoneRow)

        screenAdjusted.copy(content = updatedLines.mkString("\n"))
      }
    }
  }

}
