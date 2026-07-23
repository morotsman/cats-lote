package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Layout, LoteApp, SlidePosition, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder

/** Demonstrates spatial layout of slides in 3D space (WebGL).
  *
  * This example focuses exclusively on slide positioning — no overlays, no transitions, just the spatial arrangement of
  * slides.
  *
  * Part 1: Layout helpers — grid, circle, spiral — that compute positions automatically via `addLayoutSection`. Part 2:
  * Relative positioning — `.right()`, `.down()`, etc.
  *
  * To run:
  * {{{
  * // In build.sbt, set:
  * Compile / mainClass := Some("com.github.morotsman.examples.SpatialLayoutExample")
  *
  * sbt browserExamples/fastLinkJS
  * cd browser-examples
  * python3 -m http.server 8080
  * }}}
  * Open http://127.0.0.1:8080/spatial-layouts.html
  */
object SpatialLayoutExample extends LoteApp {

  def presentation = slides[IO]()

  def slides[F[_]: Async: Ref.Make](): SessionBuilder[F] =
    SessionBuilder[F]()
      .withFrameRate(60)
      .withAnimationFrameRate(30)
      .withQuickNavigation()
      // ═══════════════════════════════════════════════
      //  INTRO
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Spatial Layout Helpers
          |
          |This example demonstrates the layout helpers
          |and relative positioning API for arranging
          |slides in 3D space.
          |
          |Features shown:
          |
          |  1. Layout.grid     — rows and columns
          |  2. Layout.circle   — slides in a ring
          |  3. Layout.spiral   — slides along a helix
          |  4. Relative positioning — .right(), .down()
          |
          |Use `-` / `+` to zoom out and see the layout.
          |Press → to begin.""".stripMargin
        ).title("Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 0, 0)
      }
      // ═══════════════════════════════════════════════
      //  GRID LAYOUT via Layout.grid
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |  ┌─────────────────────────────────────┐
          |  │       GRID LAYOUT (automatic)        │
          |  └─────────────────────────────────────┘
          |
          |  The next 6 slides are arranged with:
          |
          |    Layout.grid(
          |      cols = 3,
          |      spacingX = 1800,
          |      spacingY = 1200,
          |      origin = SlidePosition(0, 3600, 0)
          |    )
          |
          |  No manual coordinates needed!
          |  Zoom out with `-` to see the grid.""".stripMargin
        ).title("Grid: Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 2400, 0)
      }
      .addLayoutSection(Layout.grid(cols = 3, spacingX = 1800, spacingY = 1200, origin = SlidePosition(0, 3600, 0))) {
        section =>
          section
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (0,0)  │
            |  └──────────────┘
            |
            |  Row 0, Column 0
            |  Auto-positioned""".stripMargin
              ).title("Grid (0,0)")
            }
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (0,1)  │
            |  └──────────────┘
            |
            |  Row 0, Column 1
            |  Auto-positioned""".stripMargin
              ).title("Grid (0,1)")
            }
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (0,2)  │
            |  └──────────────┘
            |
            |  Row 0, Column 2
            |  Auto-positioned""".stripMargin
              ).title("Grid (0,2)")
            }
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (1,0)  │
            |  └──────────────┘
            |
            |  Row 1, Column 0
            |  Auto-positioned""".stripMargin
              ).title("Grid (1,0)")
            }
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (1,1)  │
            |  └──────────────┘
            |
            |  Row 1, Column 1
            |  Auto-positioned""".stripMargin
              ).title("Grid (1,1)")
            }
            .addTextSlide {
              _.content(
                """
            |  ┌──────────────┐
            |  │  Grid (1,2)  │
            |  └──────────────┘
            |
            |  Row 1, Column 2
            |  Auto-positioned""".stripMargin
              ).title("Grid (1,2)")
            }
      }
      // ═══════════════════════════════════════════════
      //  CIRCLE LAYOUT via Layout.circle
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |  ┌─────────────────────────────────────┐
          |  │      CIRCLE LAYOUT (automatic)       │
          |  └─────────────────────────────────────┘
          |
          |  The next 6 slides are arranged with:
          |
          |    Layout.circle(
          |      radius = 2400,
          |      centerX = 1800,
          |      centerY = 12000
          |    )
          |
          |  Zoom out with `-` to see the ring.""".stripMargin
        ).title("Circle: Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 7200, 0)
      }
      .addLayoutSection(Layout.circle(radius = 2400, centerX = 1800, centerY = 12000)) { section =>
        section
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  0°    │
            |  └────────────────┘
            |
            |  Auto-positioned at 0°""".stripMargin
            ).title("Circle 0°")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  60°   │
            |  └────────────────┘
            |
            |  Auto-positioned at 60°""".stripMargin
            ).title("Circle 60°")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  120°  │
            |  └────────────────┘
            |
            |  Auto-positioned at 120°""".stripMargin
            ).title("Circle 120°")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  180°  │
            |  └────────────────┘
            |
            |  Auto-positioned at 180°""".stripMargin
            ).title("Circle 180°")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  240°  │
            |  └────────────────┘
            |
            |  Auto-positioned at 240°""".stripMargin
            ).title("Circle 240°")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Circle  300°  │
            |  └────────────────┘
            |
            |  Auto-positioned at 300°""".stripMargin
            ).title("Circle 300°")
          }
      }
      // ═══════════════════════════════════════════════
      //  SPIRAL LAYOUT via Layout.spiral
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |  ┌─────────────────────────────────────┐
          |  │      SPIRAL LAYOUT (automatic)       │
          |  └─────────────────────────────────────┘
          |
          |  The next 6 slides are arranged with:
          |
          |    Layout.spiral(
          |      startRadius = 800, growth = 400,
          |      angleStep = 60, zStep = -500,
          |      centerX = 1800, centerY = 19200
          |    )
          |
          |  Zoom out with `-` to see the helix.""".stripMargin
        ).title("Spiral: Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 16800, 0)
      }
      .addLayoutSection(
        Layout.spiral(startRadius = 800, growth = 400, angleStep = 60, zStep = -500, centerX = 1800, centerY = 19200)
      ) { section =>
        section
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #0    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=800, 0°)""".stripMargin
            ).title("Spiral #0")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #1    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=1200, 60°)""".stripMargin
            ).title("Spiral #1")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #2    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=1600, 120°)""".stripMargin
            ).title("Spiral #2")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #3    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=2000, 180°)""".stripMargin
            ).title("Spiral #3")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #4    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=2400, 240°)""".stripMargin
            ).title("Spiral #4")
          }
          .addTextSlide {
            _.content(
              """
            |  ┌────────────────┐
            |  │  Spiral  #5    │
            |  └────────────────┘
            |
            |  Auto-positioned (r=2800, 300°)""".stripMargin
            ).title("Spiral #5")
          }
      }
      // ═══════════════════════════════════════════════
      //  RELATIVE POSITIONING
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |  ┌─────────────────────────────────────┐
          |  │    RELATIVE POSITIONING              │
          |  └─────────────────────────────────────┘
          |
          |  The next slides use relative positioning:
          |
          |    .right(1800)    — 1800 units right
          |    .down(1200)     — 1200 units below
          |    .offset(dx,dy,dz) — general offset
          |
          |  No absolute coordinates needed!""".stripMargin
        ).title("Relative: Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 24000, 0)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌────────────────────┐
          |  │  .right(1800)      │
          |  └────────────────────┘
          |
          |  This slide used .right(1800)
          |  relative to the intro slide.""".stripMargin
        ).title("Right")
          .right(1800)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌────────────────────┐
          |  │  .right(1800)      │
          |  └────────────────────┘
          |
          |  Another .right(1800)
          |  — now two steps right.""".stripMargin
        ).title("Right Again")
          .right(1800)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌────────────────────┐
          |  │  .down(1200)       │
          |  └────────────────────┘
          |
          |  This slide used .down(1200)
          |  — same column, one row down.""".stripMargin
        ).title("Down")
          .down(1200)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌──────────────────────────┐
          |  │  .left(1800).down(1200)  │
          |  └──────────────────────────┘
          |
          |  Offsets accumulate!
          |  .left(1800).down(1200)""".stripMargin
        ).title("Left + Down")
          .left(1800)
          .down(1200)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌──────────────────────────────┐
          |  │  .right(1800).back(2000)     │
          |  └──────────────────────────────┘
          |
          |  Relative works in 3D too!
          |  .right(1800).back(2000)
          |  — moves right AND away from camera.""".stripMargin
        ).title("Right + Back")
          .right(1800)
          .back(2000)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌──────────────────────────────┐
          |  │  .right(1800).rotateY(15)    │
          |  └──────────────────────────────┘
          |
          |  Relative rotation!
          |  .right(1800).rotateY(15)
          |  — tilts 15° more than the previous slide.""".stripMargin
        ).title("Right + RotateY")
          .right(1800)
          .rotateY(15)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌──────────────────────────────┐
          |  │  .right(1800).rotateY(15)    │
          |  └──────────────────────────────┘
          |
          |  Another .rotateY(15) accumulates!
          |  Now rotated 30° total from the
          |  "Relative Intro" slide.""".stripMargin
        ).title("RotateY Again")
          .right(1800)
          .rotateY(15)
      }
      .addTextSlide {
        _.content(
          """
          |  ┌──────────────────────────────────┐
          |  │  .down(1200).rotateX(10)         │
          |  │           .rotateZ(-5)           │
          |  └──────────────────────────────────┘
          |
          |  Multiple rotation axes at once:
          |  .down(1200).rotateX(10).rotateZ(-5)""".stripMargin
        ).title("Multi-axis Rotation")
          .down(1200)
          .rotateX(10)
          .rotateZ(-5)
      }
      // ═══════════════════════════════════════════════
      //  SUMMARY
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Summary
          |
          |Layout helpers eliminate manual coordinate math:
          |
          |  Layout.grid(cols, spacingX, spacingY, origin)
          |  Layout.circle(radius, centerX, centerY)
          |  Layout.spiral(startRadius, growth, angleStep, zStep)
          |  Layout.line(spacing, dirX, dirY, dirZ, origin)
          |
          |Relative positioning for sequential flows:
          |
          |  .right(d)   .left(d)    .offset(dx, dy, dz)
          |  .down(d)    .up(d)
          |  .forward(d) .back(d)
          |
          |Relative rotation (accumulates with previous):
          |
          |  .rotateX(deg)  .rotateY(deg)  .rotateZ(deg)
          |
          |All compose freely with manual .at() and .rotatedBy().""".stripMargin
        ).title("Summary")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 30000, 0)
      }
}
