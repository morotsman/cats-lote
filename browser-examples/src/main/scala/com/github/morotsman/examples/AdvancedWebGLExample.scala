package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.examples.landscape.{Landscape3DSlide, LandscapeSlide}
import com.github.morotsman.examples.slides.{Bye, ExampleInteractiveSlide, SweepRightTransition}
import com.github.morotsman.lote.api.{Alignment, HorizontalAlignment, Milestone, TerminalPlatform, VerticalAlignment}
import com.github.morotsman.lote.api.builders.SessionBuilder
import org.scalajs.dom

import scala.concurrent.duration.DurationInt

/** The same full-featured presentation as AdvancedExample, rendered via Three.js / WebGL. */
object AdvancedWebGLExample extends IOApp.Simple {

  override def run: IO[Unit] = {
    val container = dom.document
      .getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
      SessionBuilder[IO]()
        .withTimer(30.minutes)
        .withProgressBar(
          List(
            Milestone("Start", 0),
            Milestone("Agenda", 1),
            Milestone("Slides", 2),
            Milestone("Overlays", 4),
            Milestone("Titles", 9),
            Milestone("Alignment", 10),
            Milestone("Transitions", 11),
            Milestone("Custom", 17),
            Milestone("Interactive", 20),
            Milestone("Landscapes", 22),
            Milestone("Summary", 25),
            Milestone("Bye", 26)
          )
        )
        .withQuickNavigation()
        .withIdleAnimation(idleTimeout = 10.seconds)
        .withFrameRate(60)
        .withAnimationFrameRate(25)
        .addTextSlide {
          _.content("""
                    |_________     _________________________         .____    ______________________________
                    |\_   ___ \   /  _  \__    ___/   _____/         |    |   \_____  \__    ___/\_   _____/
                    |/    \  \/  /  /_\  \|    |  \_____  \   ______ |    |    /   |   \|    |    |    __)_
                    |\     \____/    |    \    |  /        \ /_____/ |    |___/    |    \    |    |        \
                    | \______  /\____|__  /____| /_______  /         |_______ \_______  /____|   /_______  /
                    |        \/         \/               \/                  \/       \/                 \/
                    |
                    |                          ╔═══════════════════════════╗
                    |                          ║   WebGL / Three.js Mode   ║
                    |                          ╚═══════════════════════════╝
                    |""".stripMargin)
            .title("Start")
        }
        .addTextSlide {
          _.content(
            """
            |Agenda:
            |
            |- text slides and staged reveals
            |- overlays for timing, navigation, and progress
            |- titles, alignment, and transitions
            |- custom overlays and custom transitions
            |- interactive slides
            |
            |In other words, we are about to spend several slides proving that a terminal can absolutely develop a personality if left unsupervised.""".stripMargin
          ).title("Agenda")
        }
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
            |quietly reminding you that time remains a concept.""".stripMargin
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
            |Especially helpful in longer presentations,
            |or any deck that risks becoming an expedition.""".stripMargin
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
            |Alignment controls where content lands on the screen.
            |An easy way to make a splash screen, a note,
            |or a summary feel deliberately placed rather than just abandoned there.
            |
            |This is also the first slide with a transition,
            |so the next slide is where things start moving,
            |because placement alone was apparently too calm.""".stripMargin
          ).title("Alignment")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
            .flipTransition()
        }
        .addTextSlide {
          _.content(
            """
            |The transition that brought you here was `FlipTransition`.
            |
            |On WebGL, it rotates the terminal plane in 3D.
            |On a plain terminal, it falls back to a simple replace.
            |
            |Leave this slide to see `FlipVerticalTransition`,
            |which flips around the other axis.""".stripMargin
          ).title("FlipTransition")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
            .flipVerticalTransition()
        }
        .addTextSlide {
          _.content(
            """
            |The transition that brought you here was `FlipVerticalTransition`.
            |
            |Same idea, different axis — the view rotates left-to-right
            |instead of top-to-bottom.
            |
            |Leave this slide to see `SmokeTransition`,
            |which is what happens when text decides to evaporate.""".stripMargin
          ).title("FlipVerticalTransition")
            .smokeTransition()
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(
            """
            |The transition that brought you here was `SmokeTransition`.
            |
            |On WebGL, characters drift upward and fade out
            |with a slight wobble, like smoke dissipating.
            |On a plain terminal, it falls back to falling characters.
            |
            |Leave this slide to see `DissolveTransition`,
            |because smoke was apparently too dramatic.""".stripMargin
          ).title("SmokeTransition")
            .dissolveTransition()
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(
            """
            |The transition that brought you here was `DissolveTransition`.
            |
            |On WebGL, the content fades out and shrinks slightly,
            |like it's dissolving into the background.
            |On a plain terminal, it falls back to falling characters.
            |
            |Leave this slide to see `RotateTransition`,
            |which spins the whole view like a panel.""".stripMargin
          ).title("DissolveTransition")
            .rotateTransition()
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
        .addTextSlide {
          _.content(
            """
            |The transition that brought you here was `RotateTransition`.
            |
            |On WebGL, the slide spins around a vertical axis
            |like a revolving door or a spinning panel,
            |revealing the new content on the other side.
            |
            |Leave this slide to see `MorphTransition`,
            |which is back to character-grid territory.""".stripMargin
          ).title("RotateTransition")
            .morphTransition()
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
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
            .transition(SweepRightTransition.contextual[IO]())
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
        }
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
          _.addSlideF(ExampleInteractiveSlide.contextual[IO]())
            .map(
              _.title("Interactive")
            )
        }
        .addTextSlide {
          _.content(
            """
            |The next two slides show a landscape — first as ASCII art,
            |then as a full 3D fairy-tale scene rendered with Three.js.
            |
            |ASCII landscape controls:
            |  A — scroll left
            |  D — scroll right
            |  S — stop scrolling
            |
            |3D landscape controls:
            |  A / D — rotate camera
            |  W / S — zoom in / out
            |  Q — reset to auto-rotate
            |
            |Same idea, different dimensions.""".stripMargin
          ).title("Introducing Landscapes")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
            .smokeTransition()
        }
        .addSlideF {
          _.addSlideF(LandscapeSlide.contextual[IO]())
            .map(_.title("Landscape (ASCII)").smokeTransition())
        }
        .addSlideF {
          _.addSlideF(Landscape3DSlide.contextual[IO]())
            .map(_.title("Landscape (3D)"))
        }
        .addTextSlide {
          _.content(
            """
            |Summary:
            |
            |- text slides cover the common case
            |- staged reveals help with pacing
            |- overlays add deck-level information
            |- transitions and alignment shape the feel
            |- custom slides exist for the moment plain text starts wanting side quests
            |
            |The overall model is fairly small,
            |right up until you decide the terminal should also be entertaining.""".stripMargin
          ).title("Summary")
            .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
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
        }
        .run()
    }
  }

}
