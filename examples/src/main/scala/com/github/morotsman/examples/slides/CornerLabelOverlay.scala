package com.github.morotsman.examples.slides

import cats.Applicative
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted}
import com.github.morotsman.lote.api.spi.Overlay

object CornerLabelOverlay {

  /** Example custom `Overlay[F]` that adds a static label to the top-right corner.
    *
    * This shows the smallest useful overlay shape:
    *   - inspect the screen context to learn the available width and height
    *   - rewrite the rendered `ScreenAdjusted` content in `applyOverlay(...)`
    *   - ignore `onUserInput(...)` entirely when no interactive behavior is needed
    */

  def apply[F[_]: Applicative](label: String): Overlay[F] =
    new Overlay[F] {
      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): F[ScreenAdjusted] = {
        val _ = originalContent
        val width = context.screenWidth - 1
        val height = context.screenHeight
        val badge = s"[$label]".take(width)

        def normalizeLine(line: String): String = {
          val truncated = line.take(width)
          truncated + (" " * (width - truncated.length))
        }

        val lines = screenAdjusted.content.split("\n", -1).toVector
        val paddedLines =
          if (lines.length >= height) lines.take(height)
          else lines ++ Vector.fill(height - lines.length)(" " * width)

        val topLine = normalizeLine(paddedLines.headOption.getOrElse(""))
        val badgeStart = math.max(0, width - badge.length)
        val updatedTopLine =
          topLine.take(badgeStart) + badge + topLine.drop(math.min(width, badgeStart + badge.length))

        Applicative[F].pure(
          screenAdjusted.copy(content = paddedLines.updated(0, updatedTopLine).mkString("\n"))
        )
      }
    }
}
