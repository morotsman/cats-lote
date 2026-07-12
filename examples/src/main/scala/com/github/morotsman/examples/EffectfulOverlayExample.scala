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
            |Use this when your overlay can't exist without setup work first —
            |allocating state, wiring subscriptions, the usual existential prerequisites.
            |
            |This one keeps a little state and shows the last input it saw,
            |because apparently the top line wanted a memory.""".stripMargin
        ).title("What It Is")
      }
      .addTextSlide {
        _.content(
          """You attach it with `addOverlay(F[Overlay[F]])` instead of `addOverlay(overlay)`.
            |
            |The session builds the overlay first,
            |then keeps using it while the deck runs.
            |
            |Press arrow keys or type characters and watch the top line update.
            |It's somehow enough to feel interactive.""".stripMargin
        ).title("How To Use It")
      }
      .addTextSlide {
        _.content(
          """Same `Overlay[F]` interface as the simpler example, but with state.
            |
            |`onUserInput(...)` updates the state,
            |`applyOverlay(...)` renders it into the top line.
            |
            |Your overlay is now monitoring your keyboard in real time,
            |which is either a feature or a warning sign depending on your perspective.""".stripMargin
        ).title("Overlay Interface")
      }
      .addTextSlide {
        _.content(
          """The code allocates a `Ref` to track:
            |
            |- how many inputs the overlay has seen
            |- the most recent input label
            |
            |That state flows from input handling into rendering,
            |which is a common pattern for live status overlays
            |and other small dashboard habits that start innocently enough.""".stripMargin
        ).title("How The Code Works")
      }
      .addTextSlide {
        _.content(
          """Use `addOverlay(overlay)` for simple static overlays
            |that can be constructed without ceremony.
            |
            |Use `addOverlay(F[Overlay[F]])` when the overlay needs
            |state, subscriptions, or any other effectful setup
            |that would feel dishonest to pretend was pure.""".stripMargin
        ).title("When To Choose It")
      }
      .run()
}
