package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.SessionBuilder
import com.github.morotsman.lote.interpreter.middleware.Milestone

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
      .addTextSlide { _ =>
        _.content(
          """Session-wide features are configured once on `SessionBuilder`.
            |
            |This deck turns on a timer, a progress bar, quick navigation,
            |and custom frame-rate settings.""".stripMargin
        ).title("What They Are")
      }
      .addTextSlide { _ =>
        _.content(
          """The important idea is that these settings apply to the whole presentation,
            |not just a single slide.
            |
            |That makes them a good place to put navigation, timing,
            |and other deck-level behavior.""".stripMargin
        ).title("How To Use Them")
      }
      .addTextSlide { _ =>
        _.content(
          """Look for the timer and progress bar overlays while you move through the deck.
            |
            |Press N to open quick navigation and see how titles become menu labels.""".stripMargin
        ).title("What To Notice")
      }
      .addTextSlide { _ =>
        _.content(
          """The frame-rate settings shape motion in the session:
            |
            |- `withFrameRate(...)` changes render cadence
            |- `withAnimationFrameRate(...)` changes animation speed
            |
            |Those settings matter most once you start using animated transitions or slides.""".stripMargin
        ).title("What To Tweak")
      }
      .run()
}
