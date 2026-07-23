package com.github.morotsman.lote.internal.algebra

import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted}
import com.github.morotsman.lote.api.spi.Overlay
import com.github.morotsman.lote.internal.model.{Presentation, SlideSpecification}
import munit.CatsEffectSuite

class FeatureSpec extends CatsEffectSuite {

  /** A trivial overlay that appends a tag to content. */
  private def tagOverlay(tag: String): Overlay[IO] = new Overlay[IO] {
    override def applyOverlay(
        context: Screen,
        screenAdjusted: ScreenAdjusted,
        originalContent: ScreenAdjusted
    ): IO[ScreenAdjusted] =
      IO.pure(ScreenAdjusted(screenAdjusted.content + tag))
  }

  /** A stub PresentationExecutor for testing. */
  private def stubExecutor(slideRef: Ref[IO, Option[Int]]): PresentationExecutor[IO] =
    new PresentationExecutor[IO] {
      def start(): IO[Unit] = IO.unit
      def setSlide(index: Int): IO[Unit] = slideRef.set(Some(index))
    }

  /** A dummy Presentation with a given number of titled slides. */
  private def dummyPresentation(titles: List[String]): Presentation[IO] =
    Presentation(
      slideSpecifications = titles.map(t => SlideSpecification[IO](slide = null, out = None, title = Some(t)))
    )

  // ── Feature.fromOverlay ──

  test("fromOverlay — wraps the provided overlay") {
    val overlay = tagOverlay("[test]")
    val feature = Feature.fromOverlay[IO](overlay)
    assert(feature.overlay eq overlay)
  }

  test("fromOverlay — onSlideChange is a no-op (completes without error)") {
    val feature = Feature.fromOverlay[IO](tagOverlay(""))
    feature.onSlideChange(0).assertEquals(())
  }

  test("fromOverlay — onSlideChange succeeds for any index") {
    val feature = Feature.fromOverlay[IO](tagOverlay(""))
    for {
      _ <- feature.onSlideChange(0)
      _ <- feature.onSlideChange(5)
      _ <- feature.onSlideChange(999)
    } yield ()
  }

  test("fromOverlay — onPresentationBuilt is a no-op") {
    val feature = Feature.fromOverlay[IO](tagOverlay(""))
    val pres = dummyPresentation(List("A", "B"))
    feature.onPresentationBuilt(pres).assertEquals(())
  }

  test("fromOverlay — onExecutorReady is a no-op") {
    for {
      ref <- Ref.of[IO, Option[Int]](None)
      feature = Feature.fromOverlay[IO](tagOverlay(""))
      _ <- feature.onExecutorReady(stubExecutor(ref))
      slide <- ref.get
    } yield assertEquals(slide, None) // executor was never called
  }

  test("fromOverlay — overlay still functions correctly") {
    val feature = Feature.fromOverlay[IO](tagOverlay("[added]"))
    val screen = Screen(80, 24)
    val content = ScreenAdjusted("hello")
    for {
      result <- feature.overlay.applyOverlay(screen, content, content)
    } yield assertEquals(result.content, "hello[added]")
  }

  // ── Custom Feature implementations ──

  test("custom feature — onSlideChange callback is invoked with correct index") {
    for {
      ref <- Ref.of[IO, List[Int]](List.empty)
      feature = new Feature[IO] {
        val overlay: Overlay[IO] = tagOverlay("")
        def onSlideChange(index: Int): IO[Unit] = ref.update(index :: _)
        def onPresentationBuilt(p: Presentation[IO]): IO[Unit] = IO.unit
        def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] = IO.unit
      }
      _ <- feature.onSlideChange(0)
      _ <- feature.onSlideChange(3)
      _ <- feature.onSlideChange(1)
      indices <- ref.get
    } yield assertEquals(indices.reverse, List(0, 3, 1))
  }

  test("custom feature — onPresentationBuilt receives presentation metadata") {
    for {
      ref <- Ref.of[IO, Vector[String]](Vector.empty)
      feature = new Feature[IO] {
        val overlay: Overlay[IO] = tagOverlay("")
        def onSlideChange(index: Int): IO[Unit] = IO.unit
        def onPresentationBuilt(p: Presentation[IO]): IO[Unit] =
          ref.set(p.titles)
        def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] = IO.unit
      }
      pres = dummyPresentation(List("Intro", "Demo", "Q&A"))
      _ <- feature.onPresentationBuilt(pres)
      titles <- ref.get
    } yield assertEquals(titles, Vector("Intro", "Demo", "Q&A"))
  }

  test("custom feature — onExecutorReady can wire up navigation") {
    for {
      slideRef <- Ref.of[IO, Option[Int]](None)
      executor = stubExecutor(slideRef)
      feature = new Feature[IO] {
        val overlay: Overlay[IO] = tagOverlay("")
        def onSlideChange(index: Int): IO[Unit] = IO.unit
        def onPresentationBuilt(p: Presentation[IO]): IO[Unit] = IO.unit
        def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] =
          e.setSlide(42)
      }
      _ <- feature.onExecutorReady(executor)
      slide <- slideRef.get
    } yield assertEquals(slide, Some(42))
  }

  // ── Multiple features composed ──

  test("multiple features — all onSlideChange hooks are invoked") {
    for {
      ref1 <- Ref.of[IO, Int](0)
      ref2 <- Ref.of[IO, Int](0)
      feature1 = new Feature[IO] {
        val overlay: Overlay[IO] = tagOverlay("")
        def onSlideChange(index: Int): IO[Unit] = ref1.set(index)
        def onPresentationBuilt(p: Presentation[IO]): IO[Unit] = IO.unit
        def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] = IO.unit
      }
      feature2 = new Feature[IO] {
        val overlay: Overlay[IO] = tagOverlay("")
        def onSlideChange(index: Int): IO[Unit] = ref2.set(index)
        def onPresentationBuilt(p: Presentation[IO]): IO[Unit] = IO.unit
        def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] = IO.unit
      }
      features = List(feature1, feature2)
      _ <- features.traverse_(_.onSlideChange(7))
      v1 <- ref1.get
      v2 <- ref2.get
    } yield {
      assertEquals(v1, 7)
      assertEquals(v2, 7)
    }
  }

  test("multiple features — lifecycle hooks execute in sequence") {
    for {
      log <- Ref.of[IO, List[String]](List.empty)
      mkFeature = (name: String) =>
        new Feature[IO] {
          val overlay: Overlay[IO] = tagOverlay("")
          def onSlideChange(index: Int): IO[Unit] =
            log.update(s"$name:slide($index)" :: _)
          def onPresentationBuilt(p: Presentation[IO]): IO[Unit] =
            log.update(s"$name:built" :: _)
          def onExecutorReady(e: PresentationExecutor[IO]): IO[Unit] =
            log.update(s"$name:ready" :: _)
        }
      features = List(mkFeature("A"), mkFeature("B"))
      pres = dummyPresentation(List("S1"))
      slideRef <- Ref.of[IO, Option[Int]](None)
      executor = stubExecutor(slideRef)
      _ <- features.traverse_(_.onPresentationBuilt(pres))
      _ <- features.traverse_(_.onExecutorReady(executor))
      _ <- features.traverse_(_.onSlideChange(0))
      entries <- log.get
    } yield {
      val ordered = entries.reverse
      assertEquals(
        ordered,
        List(
          "A:built",
          "B:built",
          "A:ready",
          "B:ready",
          "A:slide(0)",
          "B:slide(0)"
        )
      )
    }
  }
}
