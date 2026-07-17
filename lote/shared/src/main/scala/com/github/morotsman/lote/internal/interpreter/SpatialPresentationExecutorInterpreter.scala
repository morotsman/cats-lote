package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.{Fiber, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{Key, RenderEffect, SlidePosition, SpecialKey, UserInput}
import com.github.morotsman.lote.internal.algebra.PresentationExecutor
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.internal.model.Presentation

/** Presentation executor for spatial (3D) mode.
  *
  * Unlike the standard `PresentationExecutorInterpreter`, this executor:
  *  1. Sends `InitSpatialLayout` at startup so the terminal creates one rendering layer per unique position
  *  2. Pre-renders the first slide of each position group onto its layer
  *  3. On navigation: re-renders the current slide's content onto its layer, and only moves the camera
  *     if the resolved position changed (slides sharing a position just swap content in-place)
  *  4. Still calls `startShow`/`stopShow` on the active slide for interactive behaviour
  */
private[lote] object SpatialPresentationExecutorInterpreter {

  private case class ExecutorState(
      currentIndex: Int,
      switchSlide: Boolean
  )

  private sealed trait Command
  private case object NavigateRight extends Command
  private case object NavigateLeft extends Command
  private case object Exit extends Command
  private case class DelegateInput(input: UserInput) extends Command

  private type LoopResult[F[_]] = Either[Fiber[F, Throwable, Unit], (Int, Boolean, Fiber[F, Throwable, Unit])]

  def make[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      onSlideChange: Int => F[Unit] = null
  ): F[PresentationExecutor[F]] = {
    val notifyOnSlideChange: Int => F[Unit] =
      if (onSlideChange != null) onSlideChange else (_: Int) => Monad[F].unit

    // Pre-compute resolved positions for all slides
    val specs = presentation.slideSpecifications
    val resolvedPositions: Vector[SlidePosition] = {
      var lastPos = SlidePosition(0, 0, 0)
      specs.map { spec =>
        spec.position match {
          case Some(pos) =>
            lastPos = pos
            pos
          case None =>
            lastPos
        }
      }.toVector
    }

    // Determine which slides are the first at each unique position (for pre-rendering)
    val seenPositions = scala.collection.mutable.Set.empty[(Double, Double, Double)]
    val isFirstAtPosition: Vector[Boolean] = resolvedPositions.map { pos =>
      val key = (pos.x, pos.y, pos.z)
      if (seenPositions.contains(key)) false
      else { seenPositions += key; true }
    }

    Ref[F].of(ExecutorState(currentIndex = 0, switchSlide = true)).map { stateRef =>
      new PresentationExecutor[F] {
        override def start(): F[Unit] = for {
          _ <- NConsole[F].clear()
          _ <- setupSpatialLayout()
          _ <- preRenderFirstSlidePerPosition()
          _ <- executionLoop()
        } yield ()

        override def setSlide(index: Int): F[Unit] = {
          val clampedIndex = Math.max(0, Math.min(index, specs.length - 1))
          stateRef.set(ExecutorState(currentIndex = clampedIndex, switchSlide = true))
        }

        /** Tell the terminal to create layers for each unique position. */
        private def setupSpatialLayout(): F[Unit] = {
          val positions: Vector[Option[SlidePosition]] = specs.map(_.position).toVector
          NConsole[F].applyEffect(RenderEffect.InitSpatialLayout(positions))
        }

        /** Pre-render only the first slide that occupies each unique position. */
        private def preRenderFirstSlidePerPosition(): F[Unit] = {
          specs.zipWithIndex.traverse_ { case (spec, idx) =>
            if (isFirstAtPosition(idx)) {
              for {
                _ <- NConsole[F].applyEffect(RenderEffect.ActivateLayer(idx))
                content <- spec.slide.content
                _ <- NConsole[F].writeString(content)
              } yield ()
            } else {
              Monad[F].unit
            }
          }
        }

        private def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
          Monad[F].unit.start.flatMap { initialWork =>
            // Track the last position the camera was at (None initially so the first slide always moves the camera)
            var lastCameraPos: Option[SlidePosition] = None

            Monad[F].tailRecM((initialWork: Fiber[F, Throwable, Unit])) { work =>
              for {
                state <- stateRef.get
                currentIndex = state.currentIndex
                current = specs(currentIndex)
                currentWork <-
                  if (state.switchSlide) {
                    val targetPos = resolvedPositions(currentIndex)
                    val posChanged = !lastCameraPos.contains(targetPos)

                    // Move camera only if position changed
                    val moveCamera =
                      if (posChanged) NConsole[F].applyEffect(RenderEffect.MoveCameraTo(targetPos))
                      else Monad[F].unit

                    // Activate this slide's layer and re-render its content
                    val activateAndRender = for {
                      _ <- NConsole[F].applyEffect(RenderEffect.ActivateLayer(currentIndex))
                      content <- current.slide.content
                      _ <- NConsole[F].writeString(content)
                    } yield ()

                    lastCameraPos = Some(targetPos)

                    notifyOnSlideChange(currentIndex) >>
                      stateRef.set(ExecutorState(currentIndex, switchSlide = false)) >>
                      activateAndRender >>
                      moveCamera >>
                      current.slide.startShow.start
                  } else {
                    Monad[F].pure(work)
                  }
                userInput <- NConsole[F].read()
                command = resolveCommand(userInput)
                result <- dispatch(command, presentation, stateRef, currentIndex, currentWork)
              } yield result
            }
          }
        }
      }
    }
  }

  private def resolveCommand(input: UserInput): Command = input match {
    case Key(k) if k == SpecialKey.Right => NavigateRight
    case Key(k) if k == SpecialKey.Left  => NavigateLeft
    case Key(k) if k == SpecialKey.Esc   => Exit
    case other                           => DelegateInput(other)
  }

  private def dispatch[F[_]: Temporal: NConsole](
      command: Command,
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = command match {
    case NavigateRight =>
      handleNavigateRight(presentation, stateRef, currentIndex, currentWork)
    case NavigateLeft =>
      handleNavigateLeft(presentation, stateRef, currentIndex, currentWork)
    case Exit =>
      handleExit(presentation, currentIndex, currentWork)
    case DelegateInput(input) =>
      handleDelegateInput(presentation, stateRef, currentIndex, currentWork, input)
  }

  private def handleNavigateRight[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val specs = presentation.slideSpecifications
    val current = specs(currentIndex)
    if (currentIndex < specs.length - 1) {
      val nextSlide = specs(currentIndex + 1).slide
      // Stop current slide and run transition if one is defined
      current.slide.stopShow >>
        currentWork.cancel >>
        current.out
          .fold(Monad[F].unit) { t =>
            // Activate the current slide's layer so the transition renders there
            NConsole[F].applyEffect(RenderEffect.ActivateLayer(currentIndex)) >>
              t.transition(current.slide, nextSlide)
          } >>
        stateRef.set(ExecutorState(currentIndex + 1, switchSlide = true)) >>
        Monad[F].pure(Either.left(currentWork))
    } else {
      // At last slide — stay
      Monad[F].pure(Either.left(currentWork))
    }
  }

  private def handleNavigateLeft[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    if (currentIndex > 0) {
      current.slide.stopShow >>
        currentWork.cancel >>
        stateRef.set(ExecutorState(currentIndex - 1, switchSlide = true)) >>
        Monad[F].pure(Either.left(currentWork))
    } else {
      // At first slide — stay
      Monad[F].pure(Either.left(currentWork))
    }
  }

  private def handleExit[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    current.slide.stopShow >>
      currentWork.cancel >>
      NConsole[F].clear() >>
      Monad[F].pure(Either.right((currentIndex, true, currentWork)))
  }

  private def handleDelegateInput[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit],
      input: UserInput
  ): F[LoopResult[F]] = {
    val current = presentation.slideSpecifications(currentIndex)
    current.slide.userInput(input) >>
      stateRef.get.flatMap { latestState =>
        if (latestState.switchSlide) {
          current.slide.stopShow >>
            currentWork.cancel >>
            Monad[F].pure(Either.left(currentWork))
        } else {
          Monad[F].pure(Either.left(currentWork))
        }
      }
  }
}

