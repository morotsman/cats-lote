package com.github.morotsman.examples

import cats.effect._
import cats.implicits._
import com.github.morotsman.examples.landscape.LandscapeSlide
import com.github.morotsman.examples.slides.{Bye, ExampleInteractiveSlide, SweepRightTransition}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Milestone, TransitionType, VerticalAlignment}
import com.github.morotsman.lote.api.builders.{ContextualF, SessionBuilder}
import com.github.morotsman.lote.api.spi.Slide

import scala.concurrent.duration.DurationInt

/** Shared presentation that can run both in a terminal and in the browser.
  *
  * Platform-specific code (e.g. terminal creation, 3D slides) lives in the platform modules. This object builds the
  * common session and accepts an optional 3D landscape slide. When running in a terminal (no 3D slide provided), the
  * presentation falls back to showing the ASCII landscape.
  */
object SharedAdvancedPresentation {

  /** Builds the full advanced presentation.
    *
    * @param landscape3DSlide
    *   An optional 3D landscape slide (requires WebGL / browser). When `None`, the "Landscape (3D)" slot falls back to
    *   the ASCII version with a note explaining that 3D requires a browser.
    */
  def build[F[_]: Async: Ref.Make](
      landscape3DSlide: Option[ContextualF[F, Slide[F]]] = None
  ): SessionBuilder[F] =
    SessionBuilder[F]()
      .withTimer(30.minutes)
      .withProgressBar(
        List(
          Milestone("Start", 0),
          Milestone("Agenda", 1),
          Milestone("Slides", 2),
          Milestone("Overlays", 4),
          Milestone("Titles", 9),
          Milestone("3D Space", 10),
          Milestone("Transitions", 19),
          Milestone("Custom", 26),
          Milestone("Interactive", 28),
          Milestone("Landscapes", 30),
          Milestone("Summary", 33),
          Milestone("Bye", 34)
        )
      )
      .withQuickNavigation()
      .withIdleAnimation(idleTimeout = 10.seconds)
      .withFrameRate(60)
      .withAnimationFrameRate(30)
      // ═══════════════════════════════════════════════
      //  START
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content("""
                  |_________     _________________________         .____    ______________________________
                  |\_   ___ \   /  _  \__    ___/   _____/         |    |   \_____  \__    ___/\_   _____/
                  |/    \  \/  /  /_\  \|    |  \_____  \   ______ |    |    /   |   \|    |    |    __)_
                  |\     \____/    |    \    |  /        \ /_____/ |    |___/    |    \    |    |        \
                  | \______  /\____|__  /____| /_______  /         |_______ \_______  /____|   /_______  /
                  |        \/         \/               \/                  \/       \/                 \/
                  |
                  |
                  |
                  |
                  |""".stripMargin)
          .title("Start")
          .at(0, 0, 0)
      }
      // ═══════════════════════════════════════════════
      //  AGENDA
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Agenda:
          |
          |- text slides and staged reveals
          |- overlays for timing, navigation, and progress
          |- 3D slide placement (x, y, z and rotation)
          |- transitions (grab, morph, replace, falling)
          |- custom overlays and custom transitions
          |- interactive slides and landscapes
          |
          |In other words, we are about to spend several slides proving that a presentation can absolutely develop a personality if left unsupervised.""".stripMargin
        ).title("Agenda")
      }
      // ═══════════════════════════════════════════════
      //  TEXT SLIDES
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Text slides are the simplest way to build a slide in lote.
          |
          |Use them when you want to focus on:
          |- the text itself
          |- alignment
          |- titles
          |- transitions
          |
          |Most of this walkthrough uses text slides,
          |because sometimes a string in the right place
          |is enough software engineering for one day.""".stripMargin
        ).title("Text Slides")
      }
      .addTextSlide {
        _.content(
          """
          |A text slide can also reveal more content one step at a time.
          |
          |That is useful when you want to control pacing
          |without turning one idea into three nearly identical slides and pretending that was the plan all along.""".stripMargin
        )
          .separator("\n\n")
          .step("Step 1: Start with the smallest useful message.")
          .step("Step 2: Add more detail only when the audience is ready for it.")
          .step("Step 3: Keep the same slide, but reveal it gradually so your outline does not breed in public.")
          .title("Step By Step")
      }
      // ═══════════════════════════════════════════════
      //  OVERLAYS
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |An overlay adds information around the slide content
          |without changing the slide itself.
          |
          |Good for things like:
          |- timing
          |- navigation
          |- progress indicators
          |- decorations that refuse to wait their turn
          |
          |This deck uses several built-in overlays,
          |and the next few slides explain them one by one,
          |because even text can benefit from accessories.""".stripMargin
        ).title("Overlays")
      }
      .addTextSlide {
        _.content(
          """
          |The timer overlay shows how much presentation time is left.
          |
          |Useful when:
          |- you have a fixed speaking slot
          |- you want a steady pace without checking a separate clock
          |
          |In this deck, the timer is enabled once on the session
          |and stays visible across all slides,
          |quietly reminding the audience that their life is ticking away one second at a time.""".stripMargin
        ).title("Timer Overlay")
      }
      .addTextSlide {
        _.content(
          """
          |The progress bar overlay shows where you are in the deck.
          |
          |Milestones add section labels to the bar,
          |so the audience can tell both:
          |- how far through the presentation you are
          |- which section you're currently in
          |
          |Gives the audience hope that the presentation will eventually end.""".stripMargin
        ).title("Progress Bar Overlay")
      }
      .addTextSlide {
        _.content(
          """
          |Quick navigation gives you a menu of slides
          |so you can jump directly to another part of the deck.
          |
          |Press `N` to open it.
          |Use `↑` / `↓` to move, `Enter` to jump,
          |and `N` again to close it.
          |
          |It becomes much more useful once your slides have titles,
          |which is the next feature in this walkthrough,
          |because unnamed slides are a bold lifestyle choice.""".stripMargin
        ).title("Quick Navigation Overlay")
      }
      .addTextSlide {
        _.content(
          """
          |Titles give each slide a stable name.
          |Especially useful with quick navigation,
          |because the menu becomes much easier to scan.
          |
          |Press `N` to open quick navigation.
          |Use `↑` / `↓` to move, `Enter` to jump,
          |and `N` again to close the menu
          |before it learns too much about your slides.""".stripMargin
        ).title("Titles")
      }
      .addTextSlide {
        _.content(
          """
            |Idle animation adds motion when the presentation sits still
            |for a while without input.
            |
            |Keeps a paused deck from looking frozen,
            |which is useful during demos, questions, or breaks.
            |
            |In this deck, it starts after 5 minutes of inactivity,
            |once the terminal has had enough of your silence.""".stripMargin
        ).title("Idle Animation Overlay")
      }
      // ═══════════════════════════════════════════════
      //  3D SPACE — X/Y movement, Z depth, rotations
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Slides live in a 3D world.
          |
          |Each slide can be placed with `.at(x, y, z)`.
          |The camera flies between positions with ease-in-out.
          |Slides without a position stay in place — content swaps.
          |
          |The next few slides demonstrate spatial navigation:
          |- horizontal and vertical movement
          |- z-axis depth (zooming in and out)
          |- rotations
          |
          |Use `-` and `+` to zoom the debug camera
          |and see all slides laid out in space.
          |
          |⚠ WebGL only: 3D positioning and camera flight
          |are only visible in the browser. In a terminal,
          |slides simply swap in place.""".stripMargin
        ).title("3D Space")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 4800, 0)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║       Horizontal movement         ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is at (1800, 4800, 0).
          |The camera flew right from (0, 4800, 0).
          |
          |Neighbouring slides are visible during the flight,
          |because all slides exist as textured planes
          |in the same Three.js scene.
          |
          |⚠ WebGL only: camera flight visible in browser.""".stripMargin
        ).title("Horizontal Movement")
          .at(1800, 4800, 0)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║          Diagonal + Depth         ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is at (3600, 3600, 0).
          |The camera flew diagonally — right and up.
          |
          |Diagonal movement shows that the camera
          |follows a straight line through 3D space,
          |not a staircase of horizontal-then-vertical steps.
          |
          |⚠ WebGL only: camera flight visible in browser.""".stripMargin
        ).title("Diagonal Movement")
          .at(3600, 3600, 0)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║         Z-axis: coming closer     ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is at (3600, 2400, 500).
          |
          |Notice the z = 500: this slide is closer to the camera
          |than the others. When zoomed out with `-` you can see
          |it sitting in front of its neighbours.
          |
          |The z-axis adds depth to the layout.
          |
          |⚠ WebGL only: depth visible in browser.""".stripMargin
        ).title("Z-Axis: Closer")
          .at(3600, 2400, 500)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║       Z-axis: far behind          ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is at (3600, 2400, -3000).
          |
          |It sits far behind the previous slide.
          |The camera flew through the "Closer" slide
          |to reach this one in the back.
          |
          |Use `-` to zoom out and see both slides
          |stacked along the z-axis — one in front,
          |one far behind, like looking down a hallway.
          |
          |⚠ WebGL only: depth visible in browser.""".stripMargin
        ).title("Z-Axis: Far Behind")
          .at(3600, 2400, -3000)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║     Rotation: tilted slide        ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is rotated 15° around the Y axis.
          |
          |`.rotatedBy(0, 15, 0)` tilts the slide surface
          |so it faces slightly to the left.
          |
          |The camera still flies to the correct position,
          |and the slide is perfectly readable — just angled,
          |like a book propped up on a shelf.
          |
          |⚠ WebGL only: rotation visible in browser.""".stripMargin
        ).title("Y-Axis Rotation")
          .at(5400, 2400, 0)
          .rotatedBy(0, 15, 0)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║   Rotation: X-axis tilt           ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is rotated 10° around the X axis.
          |
          |`.rotatedBy(10, 0, 0)` tilts the slide backward,
          |like a monitor angled away from you.
          |
          |Combined with z-axis depth, rotations let you
          |build layouts that feel genuinely three-dimensional
          |rather than flat grids with extra steps.
          |
          |⚠ WebGL only: rotation visible in browser.""".stripMargin
        ).title("X-Axis Rotation")
          .at(5400, 3600, 200)
          .rotatedBy(10, 0, 0)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║   Rotation: Z-axis spin           ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide is rotated 8° around the Z axis.
          |
          |`.rotatedBy(0, 0, 8)` spins the slide in the plane,
          |like a Post-it note stuck on at an angle.
          |
          |Z-rotation is fun for emphasis or stylistic flair,
          |and thoroughly alarming in large quantities.
          |
          |⚠ WebGL only: rotation visible in browser.""".stripMargin
        ).title("Z-Axis Rotation")
          .at(5400, 4800, 0)
          .rotatedBy(0, 0, 8)
      }
      .addTextSlide {
        _.content(
          """
          |         ╔═══════════════════════════════════╗
          |         ║   Combined: depth + rotation      ║
          |         ╚═══════════════════════════════════╝
          |
          |This slide combines everything:
          |  position  (7200, 3600, 3000)
          |  rotation  (50°, -120°, 30°)
          |
          |The camera navigates to this position and orientation
          |in one smooth flight. Zoom out with `-` to see
          |how all these slides sit in the scene together.
          |
          |That is the full spatial toolkit:
          |x, y, z placement and rx, ry, rz rotation.
          |
          |⚠ WebGL only: 3D positioning visible in browser.""".stripMargin
        ).title("Combined Depth + Rotation")
          .at(7200, 3600, 3000)
          .rotatedBy(50, -120, 30)
      }
      // ═══════════════════════════════════════════════
      //  TRANSITIONS
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Transitions animate the content change between slides.
          |
          |Available transitions:
          |- `grabTransition` — a snake crawls in and drags the content away
          |- `morphTransition` — characters morph in place
          |- `replaceTransition` — characters are replaced one by one
          |- `fallingCharactersTransition` — text falls off the screen
          |- `smokeTransition` — characters dissolve into smoke particles
          |- `dissolveTransition` — characters fade out with randomized timing
          |
          |The camera navigation itself provides the spatial movement.
          |Transitions add character-level animation on top.
          |
          |This slide leaves with `grabTransition`.""".stripMargin
        ).title("Transitions")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 7200, 0)
          .grabTransition(1)
      }
      .addTextSlide {
        _.content(
          """
          |The transition that brought you here was `GrabTransition`.
          |
          |A multi-line ASCII snake crawled in from the right,
          |found the longest line of text, bit down,
          |and dragged everything off screen.
          |
          |This slide leaves with `morphTransition`.""".stripMargin
        ).title("Grab Transition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .morphTransition()
      }
      .addTextSlide {
        _.content(
          """
          |The transition that brought you here was `MorphTransition`.
          |
          |Each character morphed in place through a sequence
          |of intermediate symbols before settling on the new content.
          |
          |This slide leaves with `fallingCharactersTransition`.""".stripMargin
        ).title("Morph Transition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .fallingCharactersTransition()
      }
      .addTextSlide {
        _.content(
          """
          |The transition that brought you here was `FallingCharactersTransition`.
          |
          |Each character fell off the screen with a bit of gravity,
          |clearing the way for the next slide's content.
          |
          |This slide leaves with `smokeTransition`.""".stripMargin
        ).title("Falling Characters Transition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .smokeTransition()
      }
      .addTextSlide {
        _.content(
          """
           |The transition that brought you here was `SmokeTransition`.
           |
           |Each character broke apart into smoke particles
           |that drifted, rotated, and faded away,
           |leaving only empty space and a vague sense of loss.
           |
           |On terminal backends, smoke is not available,
           |so a default fallback (falling characters) runs instead.
           |You can override that default with `.withFallback`.
           |
           |This slide leaves with `dissolveTransition`.""".stripMargin
        ).title("Smoke Transition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .dissolveTransition()
      }
      .addTextSlide {
        _.content(
          """
           |The transition that brought you here was `DissolveTransition`.
           |
           |Each character faded out independently,
           |with per-cell randomized timing so the text
           |disintegrates unevenly across the screen.
           |
           |On WebGL backends the dissolve is GPU-accelerated.
           |On terminal backends it falls back to falling characters,
           |because graceful disappearance is harder than it looks
           |when your pixels are made of monospace.
           |
           |This slide leaves with `smokeTransition` + `withFallback`.""".stripMargin
        ).title("Dissolve Transition")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .smokeTransition()
          .withFallback(TransitionType.Grab())
      }
      .addTextSlide {
        _.content(
          """
           |The transition that brought you here used `.withFallback`.
           |
           |  .smokeTransition()
           |  .withFallback(TransitionType.Grab())
           |
           |On WebGL, the smoke transition ran as usual.
           |On a terminal, the grab transition was used instead
           |of the default falling-characters fallback.
           |
           |`TransitionType` is a sealed trait that describes
           |any built-in transition without exposing internals:
           |  Grab, Morph, Replace, FallingCharacters, Smoke, Dissolve
           |
           |`.withFallback` also accepts a `Transition[F]` or a
           |`Contextual[F, Transition[F]]` for custom transitions.""".stripMargin
        ).title("withFallback")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  CUSTOM OVERLAYS & TRANSITIONS
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |You can also create custom overlays:
          |
          |- `CustomOverlayExample` shows a simple overlay
          |  that stamps the same decoration on every slide
          |
          |- `EffectfulOverlayExample` shows a stateful overlay
          |  that updates while the slide stays on screen
          |
          |Because once a corner label works,
          |the natural next step is giving it opinions.""".stripMargin
        ).title("Custom Overlays")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(
          """
          |You can also create your own transitions.
          |
          |This slide leaves with a custom left-to-right wipe,
          |showing that your own transitions plug in
          |just like the built-in ones.
          |
          |If you want to study it in isolation, run `CustomTransitionExample`,
          |where the wipe receives the attention it clearly feels it deserves.""".stripMargin
        ).title("Custom Transition")
          .transition(SweepRightTransition.contextual[F]())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  INTERACTIVE SLIDES
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |The next slide is different from the text and step-by-step slides.
          |
          |It's a custom slide with its own state,
          |its own input handling, and its own rendering behavior.
          |
          |In this demo:
          |- use w, a, s, and d to move
          |- collect the ? characters
          |- avoid running into yourself
          |
          |If you want this in isolation, run `InteractiveSlideExample`,
          |where a slide finally gets hobbies.""".stripMargin
        ).title("Introducing Interactive Slides")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addSlideF {
        _.addSlideF(ExampleInteractiveSlide.contextual[F]())
          .map(
            _.title("Interactive")
          )
      }
      // ═══════════════════════════════════════════════
      //  LANDSCAPES
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |The next slides show a landscape scene.
          |
          |ASCII landscape controls:
          |  A — scroll left
          |  D — scroll right
          |  S — stop scrolling
          |
          |Figures roam on their own,
          |because even ASCII deserves a countryside.""".stripMargin
        ).title("Introducing Landscapes")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 12000, 0)
      }
      .addSlideF {
        _.addSlideF(LandscapeSlide.contextual[F]())
          .map(_.title("Landscape (ASCII)").transparentBackground().at(3600, 12000, 0))
      }
      .addSlideF { builder =>
        landscape3DSlide match {
          case Some(slide3D) =>
            builder
              .addSlideF(slide3D)
              .map(_.title("Landscape (3D)").at(3600, 12000, -3000).transparentBackground())
          case None =>
            builder
              .addSlideF(LandscapeSlide.contextual[F]())
              .map(_.title("Landscape (3D fallback — ASCII)").at(3600, 12000, -3000))
        }
      }
      // ═══════════════════════════════════════════════
      //  SUMMARY & BYE
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Summary:
          |
          |- text slides cover the common case
          |- staged reveals help with pacing
          |- overlays add deck-level information
          |- 3D placement and camera navigation shape the spatial feel
          |- transitions add character-level animation
          |- custom slides exist for the moment plain text starts wanting side quests
          |
          |The overall model is fairly small,
          |right up until you decide the terminal should also be entertaining.""".stripMargin
        ).title("Summary")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .at(0, 14400, 0)
      }
      .addTextSlide {
        _.content(
          Bye() +
            """
            |
            |Try the smaller examples when you want one concept at a time.
            |Come back to AdvancedExample when you want the full overview
            |and a terminal that is trying a little too hard.
            |""".stripMargin
        ).title("Bye")
          .fallingCharactersTransition()
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
          .at(1800, 14400, 0)
      }

}
