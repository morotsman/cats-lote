package com.github.morotsman.lote.internal.interpreter.ticker

import cats.effect.{Async, Ref}
import cats.effect.implicits._
import cats.effect.kernel.Fiber
import cats.implicits._
import com.github.morotsman.lote.api.spi.{Ticker, TickerSubscription}
import org.scalajs.dom

/** A `Ticker` implementation driven by `requestAnimationFrame`.
  *
  * Unlike the sleep-based [[TickerInterpreter]] (which fires at a fixed interval unrelated to the display refresh),
  * this ticker fires once per display frame (~60 fps on most monitors). Each callback is executed within the same
  * frame's microtask batch, so any rendering triggered by the callback is naturally vsync-aligned — the browser will
  * not paint until all subscriber callbacks (and their downstream writes/renders) have completed.
  *
  * ==Why this matters for WebGL==
  *
  * The sleep-based ticker fires at ~25 fps (40 ms interval) and is not synchronised with the browser's paint cycle.
  * When an interactive slide (e.g. the worm game) updates its texture and calls `render()` from a sleep-based tick, the
  * frame may be presented between two vsyncs, causing perceived judder/blur on moving content.
  *
  * With `RafTickerInterpreter`, the tick loop is `rafDelay *> runCallbacks *> loop`. The `rafDelay` suspends until the
  * next `requestAnimationFrame`; `runCallbacks` executes all subscriber effects and waits for them to complete (via
  * `parTraverse_`, same as `TickerInterpreter`). Because JS is single-threaded and the browser defers painting until
  * the microtask queue is drained, any `write()` / `render()` call inside a subscriber callback lands in the same
  * visual frame.
  *
  * ==Interaction with FixedStep==
  *
  * Simulation speed is still controlled by `AnimationSettings.step` via `FixedStep.consumeSteps`. The ticker just
  * determines ''how often'' callbacks fire; `FixedStep` determines how many simulation steps to execute based on
  * elapsed wall-clock time. So switching from 25 fps sleep-based to 60 fps rAF-based does not change animation speed.
  */
object RafTickerInterpreter {

  private case class Entry[F[_]](id: Long, callback: F[Unit])

  private case class State[F[_]](
      subscribers: List[Entry[F]] = List.empty,
      nextId: Long = 0,
      fiber: Option[Fiber[F, Throwable, Unit]] = None,
      running: Boolean = false
  )

  /** Creates a rAF-driven `Ticker[F]`.
    *
    * Usage:
    * {{{
    * import com.github.morotsman.lote.internal.interpreter.ticker.RafTickerInterpreter
    *
    * SessionBuilder[IO]()
    *   .withCustomTicker(RafTickerInterpreter.make[IO])
    *   .addSlide(...)
    *   .run()
    * }}}
    */
  def make[F[_]: Async: Ref.Make]: F[Ticker[F]] =
    Ref[F].of(State[F]()).map { state =>
      new Ticker[F] {

        /** Suspend the fiber until the next `requestAnimationFrame` callback. */
        private val rafDelay: F[Unit] = Async[F].async_ { cb =>
          dom.window.requestAnimationFrame { (_: Double) =>
            cb(Right(()))
          }
          ()
        }

        private def tickLoop(): F[Unit] = for {
          _ <- rafDelay
          s <- state.get
          _ <- s.subscribers.parTraverse_(_.callback.attempt.void)
          running <- state.get.map(_.running)
          _ <- if (running) tickLoop() else Async[F].unit
        } yield ()

        override def subscribe(callback: F[Unit]): F[TickerSubscription[F]] =
          for {
            id <- state.modify(s => (s.copy(nextId = s.nextId + 1), s.nextId))
            _ <- state.update(s => s.copy(subscribers = s.subscribers :+ Entry(id, callback)))
          } yield new TickerSubscription[F] {
            override def cancel: F[Unit] =
              state.update(s => s.copy(subscribers = s.subscribers.filterNot(_.id == id)))
          }

        override def start: F[Unit] = for {
          s <- state.get
          _ <-
            if (s.running) Async[F].unit
            else
              for {
                _ <- state.update(_.copy(running = true))
                f <- tickLoop().start
                _ <- state.update(_.copy(fiber = Some(f)))
              } yield ()
        } yield ()

        override def stop: F[Unit] = for {
          s <- state.get
          _ <- s.fiber.traverse_(_.cancel)
          _ <- state.set(State[F]())
        } yield ()
      }
    }
}
