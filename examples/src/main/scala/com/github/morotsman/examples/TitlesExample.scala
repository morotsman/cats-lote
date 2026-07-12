package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.api.builders.SessionBuilder

object TitlesExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withQuickNavigation()
      .addTextSlide {
        _.content(
          """Titles are optional, but so is making your deck navigable.
            |
            |They give each slide a short label,
            |which becomes critical once your deck outgrows the attention span of the person who wrote it.""".stripMargin
        ).title("What Titles Do")
      }
      .addTextSlide {
        _.content(
          """You add a title directly on the slide with `.title(...)`.
            |
            |That keeps the label next to the content it describes,
            |which beats the alternative of scrolling through twenty slides trying to remember which one said what.""".stripMargin
        ).title("How To Add Them")
      }
      .addTextSlide {
        _.content(
          """Press N to open quick navigation.
            |
            |The slide titles become the labels in that menu,
            |which is why short, descriptive titles beat "Slide 7" and its seventeen identical siblings.
            |
            |Use ↑ and ↓ to move through the list,
            |press Enter to jump to the selected slide,
            |and press N again to close the menu.""".stripMargin
        ).title("Where You See Them")
      }
      .addTextSlide {
        _.content(
          """A good title tells the audience what a slide is for
            |before they have to read the full content.
            |
            |Once that feels civilized enough, move on to AlignmentExample,
            |where we address the even more pressing question of where your text sits.""".stripMargin
        ).title("Why They Help")
      }
      .run()
}
