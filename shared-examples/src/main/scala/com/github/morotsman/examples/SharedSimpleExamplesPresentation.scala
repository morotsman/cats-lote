package com.github.morotsman.examples

import cats.effect._
import cats.implicits._
import com.github.morotsman.examples.slides.{SimpleCounterSlide, SimpleGlideSlide, SimpleScrollSlide, SimpleStaticMarkerScrollSlide, SimpleEasedWipeTransition, SimpleSweepGlideTransition, SimpleSweepTransition, SimpleTypewriterTransition, SimpleWipeTransition}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Milestone, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder

/** A small, standalone presentation that demonstrates the core animation
  * concepts one at a time.
  *
  * Run this when you want to study the building blocks without the overhead of
  * the full `SharedAdvancedPresentation`. Each slide introduces exactly one new
  * concept:
  *
  *   1. `SimpleCounterSlide` — the simplest custom `Slide[F]` (Ref + Ticker + userInput)
  *   2. `SimpleGlideSlide` — adds `GlideLayer` and `FixedStep` (smooth overlay rendering)
  *   3. `SimpleScrollSlide` — adds `renderOntoScrolled` (sub-pixel grid scrolling + fixed rows)
  *   4. `SimpleWipeTransition` — the simplest custom `Transition[F]` (FixedStep + Deferred)
  *
  * The presentation is intentionally lightweight (few slides, no 3D positioning)
  * so it starts instantly and runs smoothly on any backend.
  */
object SharedSimpleExamplesPresentation {

  def build[F[_]: Async: Ref.Make](): SessionBuilder[F] =
    SessionBuilder[F]()
      .withProgressBar(
        List(
          Milestone("Intro", 0),
          Milestone("Counter", 1),
          Milestone("Glide", 3),
          Milestone("Scroll", 5),
          Milestone("Transition", 7),
          Milestone("Done", 13)
        )
      )
      .withQuickNavigation()
      .withFrameRate(60)
      .withAnimationFrameRate(30)
      // ═══════════════════════════════════════════════
      //  INTRO
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Simple Examples — Core Animation Concepts
          |
          |This presentation introduces four building blocks,
          |each adding one concept on top of the previous:
          |
          |  1. SimpleCounterSlide
          |     The minimum viable custom Slide[F].
          |     Just a Ref, a Ticker subscription, and userInput.
          |
          |  2. SimpleGlideSlide
          |     Adds FixedStep (fixed-rate simulation) and
          |     GlideLayer.renderOnto (smooth overlay rendering).
          |
          |  3. SimpleScrollSlide
          |     Adds renderOntoScrolled (sub-pixel grid scrolling,
          |     fixed rows, and overlay in a single call).
          |
          |  4. SimpleWipeTransition
          |     The minimum viable custom Transition[F].
          |     FixedStep + Deferred for timed completion.
          |
          |Press → to begin.""".stripMargin
        ).title("Intro")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  1. SIMPLE COUNTER
      // ═══════════════════════════════════════════════
      .addSlideF {
        _.addSlideF(SimpleCounterSlide.contextual[F]())
          .map(_.title("Simple Counter"))
      }
      .addTextSlide {
        _.content(
          """
          |SimpleCounterSlide — Recap
          |
          |That was the simplest custom Slide[F]. It used:
          |
          |  • Ref[F]            — holds the counter value
          |  • Ticker.subscribe  — re-renders on every tick
          |  • userInput         — handles W/S key presses
          |
          |No GlideLayer, no FixedStep, no SmoothChar.
          |Just the minimum required to make a slide interactive.
          |
          |Source: slides/SimpleCounterSlide.scala
          |
           |Next: SimpleGlideSlide adds smooth overlay rendering.""".stripMargin
        ).title("Counter — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  2. SIMPLE GLIDE
      // ═══════════════════════════════════════════════
      .addSlideF {
        _.addSlideF(SimpleGlideSlide.contextual[F]())
          .map(_.title("Simple Glide"))
      }
      .addTextSlide {
        _.content(
          """
          |SimpleGlideSlide — Recap
          |
          |On top of SimpleCounterSlide, this added:
          |
          |  • FixedStep           — decouples simulation rate from frame rate.
          |                          Each step moves the ball by one cell.
          |
          |  • GlideLayer.renderOnto — renders the ball as a SmoothChar.
          |     - WebGL: sub-pixel interpolation on a floating overlay
          |     - Terminal: integer compositing onto the grid
          |
          |The slide never branches on the platform — GlideLayer
          |abstracts that away.
          |
          |Source: slides/SimpleGlideSlide.scala
          |
          |Next: SimpleScrollSlide adds sub-pixel grid scrolling.""".stripMargin
        ).title("Glide — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  3a. SIMPLE SCROLL (static marker)
      // ═══════════════════════════════════════════════
      .addSlideF {
        _.addSlideF(SimpleStaticMarkerScrollSlide.contextual[F]())
          .map(_.title("Static Marker Scroll"))
      }
      // ═══════════════════════════════════════════════
      //  3b. SIMPLE SCROLL (orbiting marker)
      // ═══════════════════════════════════════════════
      .addSlideF {
        _.addSlideF(SimpleScrollSlide.contextual[F]())
          .map(_.title("Simple Scroll"))
      }
      .addTextSlide {
        _.content(
          """
          |SimpleScrollSlide — Recap
          |
          |On top of SimpleGlideSlide, this added:
          |
          |  • renderOntoScrolled — one call that combines:
          |     - scrollX: sub-pixel grid offset (the fractional camera)
          |     - chars: overlay characters (the marker)
          |     - fixedRows: rows that don't scroll (the status bar)
          |
          |  On WebGL this gives buttery-smooth world scrolling.
          |  On terminal it renders at integer positions (same logic).
          |
          |This is the same pattern used by LandscapeSlide,
          |just stripped to the essentials.
          |
          |Source: slides/SimpleScrollSlide.scala
          |
          |Next: SimpleWipeTransition (watch the transition!)""".stripMargin
        ).title("Scroll — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SimpleWipeTransition.contextual[F]())
      }
      // ═══════════════════════════════════════════════
      //  4. SIMPLE WIPE TRANSITION (the transition itself IS the demo)
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |SimpleWipeTransition — Recap
          |
          |The transition that just played was SimpleWipeTransition.
          |It is the simplest custom Transition[F]:
          |
          |  • FixedStep.consumeSteps — fixed-rate row clearing
          |  • Ref[F]                 — tracks how many rows are cleared
          |  • Deferred[F, Unit]      — signals completion
          |  • Ticker.subscribe       — drives the animation loop
          |
          |The transition reads from.content and to.content,
          |then wipes top-to-bottom, revealing the new slide.
          |No GlideLayer, no SmoothChar — just the essentials.
          |
           |Source: slides/SimpleWipeTransition.scala
           |
           |Next: SimpleSweepTransition (watch the transition!)""".stripMargin
        ).title("Wipe Transition — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SimpleSweepTransition.contextual[F]())
      }
      // ═══════════════════════════════════════════════
      //  5. SIMPLE SWEEP TRANSITION
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |SimpleSweepTransition — Recap
          |
          |The transition that just played was SimpleSweepTransition.
          |It reveals the next slide left-to-right, column by column.
          |
          |Like SimpleWipeTransition, it uses TickedTransition.buildProgress
          |with a ProgressContext that provides:
          |
          |  • ctx.progress      — animation progress (0.0 → 1.0)
          |  • ctx.screenWidth   — actual terminal width in characters
          |  • ctx.screenHeight  — actual terminal height in characters
          |
          |This means no hardcoded "estimatedWidth = 80" —
          |the sweep adapts to whatever screen size you have.
          |
           |Source: slides/SimpleSweepTransition.scala
           |
           |Next: withEasing (watch the eased wipe!)""".stripMargin
        ).title("Sweep Transition — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SimpleEasedWipeTransition.contextual[F]())
      }
      // ═══════════════════════════════════════════════
      //  6. EASED WIPE TRANSITION (withEasing demo)
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |SimpleEasedWipeTransition — Recap
          |
          |The transition that just played was the same top-to-bottom
          |wipe, but with an easing function applied:
          |
           |  builder
           |    .withEasing(Easing.easeInOutCubic)
           |    .buildProgress(duration) { (from, to, ctx) =>
           |      val frame = renderFrame(...)
           |      if (done) ProgressResult.done(frame)
           |      else      ProgressResult.continue(frame)
           |    }
          |
          |The single addition of .withEasing(...) makes the wipe
          |start slow, accelerate, then decelerate — no changes
          |to the rendering logic at all.
          |
          |Available easings: linear, easeIn, easeOut, easeInOut,
          |easeInCubic, easeOutCubic, easeInOutCubic, easeOutBounce
          |
          |Source: slides/SimpleEasedWipeTransition.scala
          |
          |Next: buildWithSetup (watch the typewriter reveal!)""".stripMargin
        ).title("Eased Wipe — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SimpleTypewriterTransition.contextual[F]())
      }
      // ═══════════════════════════════════════════════
      //  7. TYPEWRITER TRANSITION (buildWithSetup demo)
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |SimpleTypewriterTransition — Recap
          |
          |The transition that just played revealed content
          |character by character, like a typewriter.
          |
          |It uses buildWithSetup — the most flexible builder:
          |
          |  builder.buildWithSetup { (from, to, complete) =>
          |    Ref.of[F, Int](0).map { revealRef =>
          |      TickedTransition.TickHandler(
          |        onTick = (nrOfSteps, _) => {
          |          // advance state, render, call complete when done
          |        }
          |      )
          |    }
          |  }
          |
          |Unlike buildProgress, YOU manage the state (Ref) and
          |signal completion manually. This gives full control.
          |
          |Source: slides/SimpleTypewriterTransition.scala
          |
          |Next: buildProgressWithGlide (watch the glide sweep!)""".stripMargin
        ).title("Typewriter — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
          .transition(SimpleSweepGlideTransition.contextual[F]())
      }
      // ═══════════════════════════════════════════════
      //  8. SWEEP WITH GLIDE (buildProgressWithGlide demo)
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |SimpleSweepGlideTransition — Recap
          |
          |The transition that just played was a left-to-right
          |sweep with GlideLayer rendering the edge characters.
          |
          |  builder
          |    .withGlideLayer(wrapThreshold = columnsPerStep)
          |    .buildProgressWithGlide(totalSteps) { (from, to, progress, glide) =>
          |      val frame = renderBackground(from, to, progress)
          |      val edge  = edgeChars(to, revealCol)
          |      glide.renderOnto(frame, edge)  // smooth sub-pixel on WebGL!
          |    }
          |
          |On WebGL: edge characters interpolate at sub-pixel positions.
          |On terminal: characters composite at integer grid positions.
          |Same code, both platforms — GlideLayer abstracts it away.
          |
          |Source: slides/SimpleSweepGlideTransition.scala""".stripMargin
        ).title("Sweep Glide — Recap")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      // ═══════════════════════════════════════════════
      //  DONE
      // ═══════════════════════════════════════════════
      .addTextSlide {
        _.content(
          """
          |Done!
          |
          |You've seen the core building blocks:
          |
          |  Slides:
          |    SimpleCounterSlide         (Ref + Ticker + userInput)
          |    SimpleGlideSlide           (+ FixedStep + GlideLayer)
          |    SimpleScrollSlide          (+ renderOntoScrolled)
          |
          |  Transitions:
          |    SimpleWipeTransition       (buildProgress)
          |    SimpleSweepTransition      (buildProgress + ProgressContext)
          |    SimpleEasedWipeTransition  (+ withEasing)
          |    SimpleTypewriterTransition (buildWithSetup + manual state)
          |    SimpleSweepGlideTransition (buildProgressWithGlide)
          |
          |From here you can explore the more complex examples:
          |  • ExampleInteractiveSlide — a full snake game
          |  • SweepRightTransition — full GlideLayer transition
          |  • LandscapeSlide — scrolling world with multiple figures
          |
          |Or run SharedAdvancedPresentation for the full tour.""".stripMargin
        ).title("Done")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
}


