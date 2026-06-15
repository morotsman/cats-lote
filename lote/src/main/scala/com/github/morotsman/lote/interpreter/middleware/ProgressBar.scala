package com.github.morotsman.lote.interpreter.middleware

import cats.Monad
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.algebra.Overlay
import com.github.morotsman.lote.model.{Screen, ScreenAdjusted}

case class Milestone(label: String, slideIndex: Int)

trait ProgressBar[F[_]] extends Overlay[F] {
  def setCurrentSlide(index: Int): F[Unit]
}

object ProgressBar {

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
        val indicators = (0 until totalSlides)
          .map { i =>
            if (i < currentIndex) "#"
            else if (i == currentIndex) "0"
            else "-"
          }
          .mkString(" ")

        val progressBarPadding =
          Math.max(0, (context.screenWidth - indicators.length) / 2)

        // ANSI color codes
        val gray = "\u001b[90m"
        val bright = "\u001b[97m"
        val reset = "\u001b[0m"

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
          val parts = sortedMilestones.map { m =>
            val pos = progressBarPadding + m.slideIndex * 2
            val color = if (isActiveMilestone(m)) bright else gray
            (pos, color + m.label + reset)
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

        val centered = {
          " " * progressBarPadding + indicators + " " * Math.max(
            0,
            context.screenWidth - progressBarPadding - indicators.length
          )
        }

        val updatedLines = padded
          .updated(progressRowIndex, centered)
          .updated(milestoneRowIndex, milestoneRow)

        screenAdjusted.copy(content = updatedLines.mkString("\n"))
      }
    }
  }

}
