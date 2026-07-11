package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.CornerLabelOverlay
import com.github.morotsman.lote.builders.SessionBuilder

import scala.concurrent.duration.DurationInt

object CustomOverlayExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addOverlay(CornerLabelOverlay[IO]("CUSTOM OVERLAY"))
      .addTextSlide { _ =>
        _.content(
          """This example adds a custom overlay to the session.
            |
            |An overlay is a small component that rewrites rendered output
            |before it is shown on screen.
            |
            |Look in the top-right corner of every slide:
            |the label stays visible as you move through the deck.""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide { _ =>
        _.content(
          """You register an overlay once with `addOverlay(...)`.
            |
            |After that, it is applied every time a slide is rendered,
            |so it can stay visible across the whole presentation.
            |
            |That makes overlays a good fit for labels, timers, badges,
            |and other repeated session-wide information.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide { _ =>
        _.content(
          """The `Overlay[F]` interface gives you two hooks:
            |
            |1. `applyOverlay(...)` receives the screen context and rendered slide
            |2. `onUserInput(...)` can react to keys and other input events
            |
            |This example only uses `applyOverlay(...)`,
            |because the corner label is static.""".stripMargin
        ).title("Overlay Interface")
      }
      .addTextSlide { _ =>
        _.content(
          """The code stays small because the overlay does only three things:
            |
            |1. read the screen width from the context
            |2. update the top rendered line with a badge
            |3. return the modified `ScreenAdjusted` value
            |
            |That is the core pattern for simple decorative overlays.""".stripMargin
        ).title("How The Code Works")
      }
      .addTextSlide { _ =>
        _.content(
          """Static overlays are a good fit for labels, badges, and simple decorations.
            |
            |If your overlay needs state or setup work,
            |the next example shows the effectful version of the same idea.""".stripMargin
        ).title("What To Try Next")
      }
      .run()
}
