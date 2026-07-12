package com.github.morotsman.examples

import cats.effect.{IO, IOApp}
import com.github.morotsman.examples.slides.InputStatusOverlay
import com.github.morotsman.lote.api.builders.SessionBuilder

object EffectfulOverlayExample extends IOApp.Simple {

  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addOverlay(InputStatusOverlay.make[IO]())
      .addTextSlide {
        _.content(
          """This example uses the effectful `addOverlay` overload.
            |
            |Use this form when the overlay needs setup work before the deck starts.
            |
            |Here, the overlay keeps a little bit of state
            |and shows the last input it has seen, because apparently the top line wanted a memory.""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide {
        _.content(
          """You attach it with `addOverlay(F[Overlay[F]])` instead of `addOverlay(overlay)`.
            |
            |That lets the session build the overlay first,
            |then keep using it while the deck runs.
            |
            |Press arrow keys or regular characters and watch the top line update, which is somehow enough to feel interactive.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide {
        _.content(
          """This overlay uses the same `Overlay[F]` interface as the simpler example,
            |but it also keeps state.
            |
            |`onUserInput(...)` updates that state,
            |and `applyOverlay(...)` renders it into the top line, where it can monitor your choices in real time.""".stripMargin
        ).title("Overlay Interface")
      }
      .addTextSlide {
        _.content(
          """The code allocates a `Ref` and uses it to keep track of:
            |
            |- how many inputs it has seen
            |- the most recent input label
            |
            |That state flows from input handling into rendering,
            |which is a common pattern for live status overlays and other tiny dashboard habits.""".stripMargin
        ).title("How The Code Works")
      }
      .addTextSlide {
        _.content(
          """Use `addOverlay(overlay)` for simple static overlays.
            |
            |Use `addOverlay(F[Overlay[F]])` when the overlay needs
            |state, subscriptions, or any other effectful setup that would feel awkward to pretend was pure.""".stripMargin
        ).title("When To Choose It")
      }
      .run()
}
