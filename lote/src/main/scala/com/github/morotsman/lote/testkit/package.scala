package com.github.morotsman.lote

/** Test utilities for unit-testing custom slides, transitions, and overlays without a real terminal.
  *
  * == Quick Start ==
  *
  * {{{
  * import cats.effect.IO
  * import com.github.morotsman.lote.api._
  * import com.github.morotsman.lote.testkit._
  * import munit.CatsEffectSuite
  *
  * class MySlideSpec extends CatsEffectSuite {
  *   test("my slide writes content on startShow") {
  *     for {
  *       harness <- SlideTestHarness.make[IO]()
  *       slide   <- MySlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
  *       _       <- slide.startShow
  *       _       <- harness.tick(3)
  *       frames  <- harness.writtenFrames
  *     } yield assert(frames.nonEmpty)
  *   }
  * }
  * }}}
  *
  * == Components ==
  *
  *   - [[testkit.TestConsole]] — mock `NConsole[F]` that records writes and injects reads
  *   - [[testkit.TestTicker]] — manual-control `Ticker[F]` (ticks only when you say)
  *   - [[testkit.SlideTestHarness]] — bundles the above with `AnimationSettings` for one-line setup
  */
package object testkit

