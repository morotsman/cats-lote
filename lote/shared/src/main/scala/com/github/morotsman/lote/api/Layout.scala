package com.github.morotsman.lote.api

/** Layout generators that compute slide positions for common spatial arrangements.
  *
  * A `Layout` produces positions for a given number of slides. Use with
  * `SessionBuilder.addLayoutSection` to apply positions automatically:
  *
  * {{{
  * SessionBuilder[F]()
  *   .addLayoutSection(Layout.grid(cols = 3, spacingX = 1800, spacingY = 1200)) { section =>
  *     section
  *       .addTextSlide { _.content("Slide 1") }
  *       .addTextSlide { _.content("Slide 2") }
  *       .addTextSlide { _.content("Slide 3") }
  *   }
  * }}}
  *
  * Or call `.positions(count)` directly to get a `List[SlidePosition]` for manual use.
  */
sealed trait Layout {

  /** Generates `count` positions for this layout arrangement. */
  def positions(count: Int): List[SlidePosition]
}

object Layout {

  /** Arranges slides in a row-major grid (left-to-right, then top-to-bottom).
    *
    * @param cols     number of columns per row
    * @param spacingX horizontal spacing between columns (default 1800)
    * @param spacingY vertical spacing between rows (default 1200)
    * @param origin   position of the first slide (top-left corner of the grid)
    */
  def grid(
      cols: Int,
      spacingX: Double = 1800.0,
      spacingY: Double = 1200.0,
      origin: SlidePosition = SlidePosition()
  ): Layout = {
    require(cols > 0, "cols must be > 0")
    new Layout {
      override def positions(count: Int): List[SlidePosition] =
        (0 until count).map { i =>
          val col = i % cols
          val row = i / cols
          origin.copy(
            x = origin.x + col * spacingX,
            y = origin.y + row * spacingY
          )
        }.toList
    }
  }

  /** Arranges slides evenly spaced around a circle.
    *
    * @param radius     distance from center to each slide position
    * @param centerX    X coordinate of the circle center
    * @param centerY    Y coordinate of the circle center
    * @param centerZ    Z coordinate of the circle center (default 0)
    * @param startAngle starting angle in degrees (0 = right, 90 = down)
    */
  def circle(
      radius: Double,
      centerX: Double = 0.0,
      centerY: Double = 0.0,
      centerZ: Double = 0.0,
      startAngle: Double = 0.0
  ): Layout = {
    require(radius > 0, "radius must be > 0")
    new Layout {
      override def positions(count: Int): List[SlidePosition] = {
        require(count > 0, "count must be > 0")
        val angleStep = 2 * math.Pi / count
        val startRad = math.toRadians(startAngle)
        (0 until count).map { i =>
          val angle = startRad + i * angleStep
          SlidePosition(
            x = centerX + radius * math.cos(angle),
            y = centerY + radius * math.sin(angle),
            z = centerZ
          )
        }.toList
      }
    }
  }

  /** Arranges slides along a spiral (helix) path with increasing radius.
    *
    * @param startRadius initial distance from center for the first slide
    * @param growth      radius increase per slide
    * @param angleStep   angle between consecutive slides in degrees (default 60)
    * @param zStep       Z-axis change per slide (negative = away from camera)
    * @param centerX     X coordinate of the spiral center
    * @param centerY     Y coordinate of the spiral center
    * @param centerZ     starting Z coordinate
    */
  def spiral(
      startRadius: Double = 800.0,
      growth: Double = 400.0,
      angleStep: Double = 60.0,
      zStep: Double = 0.0,
      centerX: Double = 0.0,
      centerY: Double = 0.0,
      centerZ: Double = 0.0
  ): Layout = new Layout {
    override def positions(count: Int): List[SlidePosition] = {
      val angleStepRad = math.toRadians(angleStep)
      (0 until count).map { i =>
        val r = startRadius + i * growth
        val angle = i * angleStepRad
        SlidePosition(
          x = centerX + r * math.cos(angle),
          y = centerY + r * math.sin(angle),
          z = centerZ + i * zStep
        )
      }.toList
    }
  }

  /** Arranges slides along a straight line in any direction.
    *
    * The direction vector is normalized internally, so only its direction matters, not its magnitude.
    *
    * @param spacing distance between consecutive slides along the line
    * @param dirX    X component of the line direction (default 1 = horizontal)
    * @param dirY    Y component of the line direction (default 0)
    * @param dirZ    Z component of the line direction (default 0)
    * @param origin  starting position of the first slide
    */
  def line(
      spacing: Double = 1800.0,
      dirX: Double = 1.0,
      dirY: Double = 0.0,
      dirZ: Double = 0.0,
      origin: SlidePosition = SlidePosition()
  ): Layout = {
    val len = math.sqrt(dirX * dirX + dirY * dirY + dirZ * dirZ)
    require(len > 0, "direction vector must be non-zero")
    val nx = dirX / len
    val ny = dirY / len
    val nz = dirZ / len
    new Layout {
      override def positions(count: Int): List[SlidePosition] =
        (0 until count).map { i =>
          origin.copy(
            x = origin.x + i * spacing * nx,
            y = origin.y + i * spacing * ny,
            z = origin.z + i * spacing * nz
          )
        }.toList
    }
  }
}

