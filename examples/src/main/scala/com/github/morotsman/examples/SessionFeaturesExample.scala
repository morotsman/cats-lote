package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.Milestone
import com.github.morotsman.lote.api.builders.SessionBuilder

import scala.concurrent.duration.DurationInt

object SessionFeaturesExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withTimer(5.minutes)
      .withProgressBar(List(
        Milestone("Intro", 0),
        Milestone("Summary", 2)
      ))
      .withQuickNavigation()
      .withFrameRate(60)
      .withAnimationFrameRate(25)
      .addTextSlide {
        _.content(
          """Session-wide features are configured once on `SessionBuilder`
            |and then quietly applied to every slide.
            |
            |This deck turns on a timer, a progress bar, quick navigation,
            |and custom frame-rate settings, because apparently one slide at a time was too easy to reason about.""".stripMargin
        ).title("What They Are")
      }
      .addTextSlide {
        _.content(
          """These settings apply to the whole presentation, not just a single slide.
            |
            |That makes them the right place for navigation, timing,
            |and other deck-level behavior that nobody wants to copy-paste
            |across twenty slides until morale improves.""".stripMargin
        ).title("How To Use Them")
      }
      .addTextSlide {
        _.content(
          """Look for the timer and progress bar overlays while you navigate.
            |
            |Press N to open quick navigation and see how titles become menu labels
            |instead of decorative optimism.
            |
            |If you forgot to add titles, the menu entries will be blank,
            |which is about as helpful as an unnamed commit.""".stripMargin
        ).title("What To Notice")
      }
      .addTextSlide {
        _.content(
          """The frame-rate settings shape all motion in the session:
            |
            |- `withFrameRate(...)` controls how often the terminal redraws
            |- `withAnimationFrameRate(...)` controls how fast transitions move
            |
            |Those two are independent, so you can have smooth rendering
            |without turning your transitions into caffeinated railguns.""".stripMargin
        ).title("What To Tweak")
      }
      .run()
}
