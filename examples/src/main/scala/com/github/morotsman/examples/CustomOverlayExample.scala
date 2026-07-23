package com.github.morotsman.examples

import cats.effect.IO
import com.github.morotsman.examples.slides.CornerLabelOverlay
import com.github.morotsman.lote.api.LoteApp
import com.github.morotsman.lote.api.builders.SessionBuilder

object CustomOverlayExample extends LoteApp {

  def presentation = SessionBuilder[IO]()
    .addOverlay(CornerLabelOverlay[IO]("CUSTOM OVERLAY"))
    .addTextSlide {
      _.content(
        """This example adds a custom overlay to the session.
        |
        |An overlay rewrites rendered output before it hits the screen,
        |which is a polite way of saying it draws on top of your slides whether they like it or not.
        |
        |Look in the top-right corner — the label stays visible on every slide,
        |quietly insisting on its own relevance.""".stripMargin
      ).title("What It Is")
    }
    .addTextSlide {
      _.content(
        """You register an overlay once with `addOverlay(...)`.
        |
        |After that, it runs every time a slide is rendered,
        |so it stays visible across the whole presentation.
        |
        |That makes overlays a good fit for labels, timers, badges,
        |and other information that refuses to wait its turn.""".stripMargin
      ).title("How To Use It")
    }
    .addTextSlide {
      _.content(
        """The `Overlay[F]` interface gives you two hooks:
        |
        |1. `applyOverlay(...)` receives the screen context and rendered slide
        |2. `onUserInput(...)` can react to keys and other input events
        |
        |This example only uses `applyOverlay(...)`,
        |because the corner label has no ambitions beyond being seen.""".stripMargin
      ).title("Overlay Interface")
    }
    .addTextSlide {
      _.content(
        """The code does exactly three things:
        |
        |1. read the screen width from the context
        |2. stamp a badge into the top line
        |3. return the modified `ScreenAdjusted`
        |
        |That's the whole pattern for simple decorative overlays.
        |Ornamentation without a side quest.""".stripMargin
      ).title("How The Code Works")
    }
    .addTextSlide {
      _.content(
        """Static overlays are a good fit for labels, badges, and simple decorations
        |that don't need to think.
        |
        |If your overlay needs state or setup work,
        |the next example shows the effectful version,
        |because even a label can eventually develop a personality.""".stripMargin
      ).title("What To Try Next")
    }
}
