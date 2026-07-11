package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.SessionBuilder

object TitlesExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .withQuickNavigation()
      .addTextSlide { _ =>
        _.content(
          """Titles are optional, but they make a presentation easier to use.
            |
            |They give each slide a short label,
            |which becomes especially helpful once a deck gets longer.""".stripMargin
        ).title("What Titles Do")
      }
      .addTextSlide { _ =>
        _.content(
          """You add a title directly on the slide with `.title(...)`.
            |
            |That keeps the label close to the slide content,
            |so naming slides becomes part of normal slide authoring.""".stripMargin
        ).title("How To Add Them")
      }
      .addTextSlide { _ =>
        _.content(
          """Press N to open quick navigation.
            |
            |The slide titles become the labels in that menu,
            |which is why short, descriptive titles help so much.
            |
            |Use ↑ and ↓ to move through the list,
            |press Enter to jump to the selected slide,
            |and press N again to leave navigation mode.""".stripMargin
        ).title("Where You See Them")
      }
      .addTextSlide { _ =>
        _.content(
          """A good title tells the audience what a slide is for
            |before they read the full content.
            |
            |Once this feels familiar, move on to AlignmentExample.""".stripMargin
        ).title("Why They Help")
      }
      .run()
}
