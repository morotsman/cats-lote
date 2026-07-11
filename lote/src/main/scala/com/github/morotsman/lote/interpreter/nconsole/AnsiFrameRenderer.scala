package com.github.morotsman.lote.interpreter.nconsole

private[nconsole] object AnsiFrameRenderer {

  private val AnsiReset = "\u001b[0m"

  private def truncateVisibleWidth(line: String, width: Int): String = {
    if (width <= 0) ""
    else {
      val builder = new StringBuilder
      var index = 0
      var visibleChars = 0
      var sawAnsi = false

      while (index < line.length && visibleChars < width) {
        if (line.charAt(index) == '\u001b' && index + 1 < line.length && line.charAt(index + 1) == '[') {
          val endIndex = line.indexOf('m', index + 2)
          if (endIndex >= 0) {
            builder.append(line.substring(index, endIndex + 1))
            index = endIndex + 1
            sawAnsi = true
          } else {
            builder.append(line.charAt(index))
            index += 1
            visibleChars += 1
          }
        } else {
          builder.append(line.charAt(index))
          index += 1
          visibleChars += 1
        }
      }

      val result = builder.result()
      if (sawAnsi && !result.endsWith(AnsiReset)) result + AnsiReset
      else result
    }
  }

  def render(
      previousFrame: Vector[String],
      content: String,
      width: Int,
      height: Int
  ): (Vector[String], String) = {
    val nextFrame = normalize(content, width, height)
    val commands = new StringBuilder
    val maxRows = Math.max(previousFrame.length, nextFrame.length)

    (0 until maxRows).foreach { rowIndex =>
      val previousLine = previousFrame.lift(rowIndex).getOrElse("")
      val nextLine = nextFrame.lift(rowIndex).getOrElse("")

      if (previousLine != nextLine) {
        commands.append(s"\u001b[${rowIndex + 1};1H")
        commands.append(nextLine)
        commands.append("\u001b[K")
      }
    }

    (nextFrame, commands.result())
  }

  private[nconsole] def normalize(
      content: String,
      width: Int,
      height: Int
  ): Vector[String] =
    content
      .split("\n", -1)
      .iterator
      .take(Math.max(height, 0))
      .map(truncateVisibleWidth(_, Math.max(width, 0)))
      .toVector
}

