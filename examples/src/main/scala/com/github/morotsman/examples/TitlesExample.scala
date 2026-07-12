package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.builders.SessionBuilder

object TitlesExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withQuickNavigation()
      .addTextSlide {
        _.content(
          """Titles are optional, but so is making a deck pleasant to navigate.
            |
            |They give each slide a short label,
            |which becomes increasingly useful once the deck stops fitting comfortably in human memory.""".stripMargin
        ).title("What Titles Do")
      }
      .addTextSlide {
        _.content(
          """You add a title directly on the slide with `.title(...)`.
            |
            |That keeps the label next to the content it describes,
            |which is a surprisingly effective alternative to guessing later.""".stripMargin
        ).title("How To Add Them")
      }
      .addTextSlide {
        _.content(
          """Press N to open quick navigation.
            |
            |The slide titles become the labels in that menu,
            |which is why short, descriptive titles beat a long row of identical regrets.
            |
            |Use ↑ and ↓ to move through the list,
            |press Enter to jump to the selected slide,
            |and press N again to leave navigation mode.""".stripMargin
        ).title("Where You See Them")
      }
      .addTextSlide {
        _.content(
          """A good title tells the audience what a slide is for
            |before they read the full content.
            |
            |Once that feels civilized enough, move on to AlignmentExample.""".stripMargin
        ).title("Why They Help")
      }
      .run()
}
