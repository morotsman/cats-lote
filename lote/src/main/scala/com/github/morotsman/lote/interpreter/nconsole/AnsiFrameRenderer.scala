package com.github.morotsman.lote.interpreter.nconsole

private[nconsole] object AnsiFrameRenderer {

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
      .map(_.take(Math.max(width, 0)))
      .toVector
}

