package com.github.morotsman.lote.internal.interpreter

import cats.Monad
import cats.effect.Temporal
import cats.effect.implicits._
import cats.effect.kernel.{Fiber, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{Key, PlatformCapability, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.internal.algebra.{PlatformStrategy, PresentationExecutor}
import com.github.morotsman.lote.api.spi.NConsole
import com.github.morotsman.lote.internal.model.Presentation

/** Interpreter for the [[PresentationExecutor]] algebra.
  *
  * This is the core state machine that drives a presentation: it manages slide navigation, user input dispatch,
  * background fiber lifecycle, and delegates all platform-specific rendering concerns (spatial layout, layer
  * activation, camera navigation) to a [[PlatformStrategy]].
  *
  * ==Architecture==
  *
  * The executor is platform-agnostic. It only knows about:
  *   - '''Navigation state''' — which slide is current and whether a slide switch is pending.
  *   - '''User input''' — translating key events into navigation commands.
  *   - '''Fiber lifecycle''' — starting/cancelling per-slide background work (animations, etc.).
  *   - '''Transitions''' — orchestrating out-transitions between slides.
  *
  * Everything else (3-D layer management, camera flights, spatial layout) is handled by a [[PlatformStrategy]] that is
  * auto-selected based on `NConsole.capabilities`:
  *
  *   - '''Terminal (JVM)''' → [[TerminalPlatformStrategy]] — all platform hooks are no-ops.
  *   - '''WebGL (JS)''' → [[SpatialPlatformStrategy]] — manages 3-D layers, camera animation, and pre-rendering.
  *
  * ==Execution loop==
  *
  * The main loop (`executionLoop`) uses `Monad.tailRecM` for stack-safe iteration:
  *
  * {{{
  *   ┌──────────────────────────────────────────────────────────┐
  *   │                    executionLoop                         │
  *   │                                                          │
  *   │  1. Read state (currentIndex, switchSlide)               │
  *   │  2. If switchSlide:                                      │
  *   │       a. Notify listeners                                │
  *   │       b. strategy.activateSlide(index)                   │
  *   │       c. Render slide content                            │
  *   │       d. strategy.navigateToSlide(index)                 │
  *   │       e. Start slide's background show fiber             │
  *   │  3. Wait for user input                                  │
  *   │  4. Dispatch command (→ Right / Left / Esc / delegate)   │
  *   │  5. Loop (Left) or terminate (Right)                     │
  *   └──────────────────────────────────────────────────────────┘
  * }}}
  *
  * ==`activateSlide` call sites==
  *
  * `strategy.activateSlide` is called in two places, for different reasons:
  *
  *   1. '''`executionLoop` (switchSlide branch)''' — activates the ''destination'' slide's rendering layer before
  *      writing its content. This runs on '''every''' slide switch, regardless of navigation direction (left or right).
  *   2. '''`handleNavigateRight` (before out-transition)''' — activates the ''departing'' slide's layer so the
  *      transition effect (dissolve, smoke, etc.) renders to the correct surface. This only happens when navigating
  *      right ''and'' an out-transition is defined.
  *
  * `handleNavigateLeft` does '''not''' call `activateSlide` because it has no out-transition to render. The destination
  * slide's activation happens automatically when the loop iterates and enters the `switchSlide` branch on the next
  * cycle.
  */
private[lote] object PresentationExecutorInterpreter {

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  /** Mutable state for the execution loop.
    *
    * @param currentIndex
    *   the 0-based index of the currently active slide
    * @param switchSlide
    *   when `true`, the next loop iteration will activate and render the slide at `currentIndex` (then reset this flag
    *   to `false`). Navigation handlers and `setSlide` set this to `true` to trigger a slide transition on the next
    *   iteration.
    */
  private case class ExecutorState(
      currentIndex: Int,
      switchSlide: Boolean
  )

  // ---------------------------------------------------------------------------
  // Commands — internal ADT for user input classification
  // ---------------------------------------------------------------------------

  private sealed trait Command
  private case object NavigateRight extends Command
  private case object NavigateLeft extends Command
  private case object Exit extends Command
  private case class DelegateInput(input: UserInput) extends Command

  /** The result type for `tailRecM`:
    *   - `Left(fiber)` → continue looping (carry the current work fiber forward)
    *   - `Right((index, _, fiber))` → terminate the loop (Esc was pressed)
    */
  private type LoopResult[F[_]] = Either[Fiber[F, Throwable, Unit], (Int, Boolean, Fiber[F, Throwable, Unit])]

  // ---------------------------------------------------------------------------
  // Factory
  // ---------------------------------------------------------------------------

  /** Creates a [[PresentationExecutor]] for the given presentation.
    *
    * The platform strategy is auto-selected based on `NConsole.capabilities` unless explicitly provided via
    * `platformStrategy`:
    *   - If `Transforms3D` is present → [[SpatialPlatformStrategy]] (WebGL)
    *   - Otherwise → [[TerminalPlatformStrategy]] (JVM terminal, all no-ops)
    *
    * @param presentation
    *   the assembled presentation (slides, transitions, overlays)
    * @param onSlideChange
    *   optional callback invoked with the slide index whenever navigation occurs
    * @param platformStrategy
    *   optional explicit strategy override (useful for testing); when `None`, the strategy is auto-selected from
    *   capabilities
    */
  def make[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      onSlideChange: Int => F[Unit] = null,
      platformStrategy: Option[PlatformStrategy[F]] = None
  ): F[PresentationExecutor[F]] = {
    val notifyOnSlideChange: Int => F[Unit] =
      if (onSlideChange != null) onSlideChange else (_: Int) => Monad[F].unit

    val specs = presentation.slideSpecifications

    // Use the explicitly provided strategy, or auto-select based on backend capabilities.
    val resolveStrategy: F[PlatformStrategy[F]] = platformStrategy match {
      case Some(s) => Monad[F].pure(s)
      case None =>
        if (NConsole[F].capabilities.contains(PlatformCapability.Transforms3D))
          SpatialPlatformStrategy[F](specs).widen[PlatformStrategy[F]]
        else
          Monad[F].pure(new TerminalPlatformStrategy[F])
    }

    resolveStrategy.flatMap { strategy =>
      Ref[F].of(ExecutorState(currentIndex = 0, switchSlide = true)).map { stateRef =>
        new PresentationExecutor[F] {

          /** Entry point: clears the screen, runs one-time platform setup, then enters the loop. */
          override def start(): F[Unit] = for {
            _ <- NConsole[F].clear()
            _ <- strategy.setupPlatform()
            _ <- executionLoop()
          } yield ()

          /** Programmatically jump to a slide (e.g., from QuickNavigation). The index is clamped to valid bounds. The
            * actual switch happens on the next loop iteration when `switchSlide = true` is observed.
            */
          override def setSlide(index: Int): F[Unit] = {
            val clampedIndex = Math.max(0, Math.min(index, specs.length - 1))
            stateRef.set(ExecutorState(currentIndex = clampedIndex, switchSlide = true))
          }

          // -------------------------------------------------------------------
          // Main execution loop
          // -------------------------------------------------------------------

          /** Stack-safe main loop using `tailRecM`.
            *
            * Each iteration:
            *   1. Checks if a slide switch is pending (`switchSlide`).
            *   2. If so: activates the slide's platform layer, renders content, navigates the camera, and starts the
            *      slide's background show.
            *   3. Blocks on user input.
            *   4. Dispatches the input as a [[Command]] to the appropriate handler.
            *   5. Returns `Left` to continue or `Right` to terminate.
            */
          private def executionLoop(): F[(Int, Boolean, Fiber[F, Throwable, Unit])] = {
            // Start with a no-op fiber as the initial "current work".
            Monad[F].unit.start.flatMap { initialWork =>
              Monad[F].tailRecM(initialWork: Fiber[F, Throwable, Unit]) { work =>
                for {
                  state <- stateRef.get
                  currentIndex = state.currentIndex
                  current = specs(currentIndex)
                  currentWork <-
                    if (state.switchSlide) {
                      // --- Slide switch: activate layer, render content, navigate camera ---
                      val activateAndRender = for {
                        // Activate the destination slide's rendering layer.
                        // On WebGL this selects the correct 3-D layer/texture;
                        // on terminal this is a no-op.
                        _ <- strategy.activateSlide(currentIndex)
                        content <- current.slide.content
                        _ <- content match {
                          case Some(c) => NConsole[F].writeString(c)
                          case None    => NConsole[F].writeString(ScreenAdjusted(""))
                        }
                      } yield ()

                      notifyOnSlideChange(currentIndex) >>
                        stateRef.set(ExecutorState(currentIndex, switchSlide = false)) >>
                        activateAndRender >>
                        // Animate camera to the slide's 3-D position (WebGL) or no-op (terminal).
                        strategy.navigateToSlide(currentIndex) >>
                        // Start the slide's background show (e.g., typing animation) in a new fiber.
                        current.slide.startShow.start
                    } else {
                      // No switch pending — keep the existing work fiber.
                      Monad[F].pure(work)
                    }
                  userInput <- NConsole[F].read()
                  command = resolveCommand(userInput)
                  result <- dispatch(command, presentation, strategy, stateRef, currentIndex, currentWork)
                } yield result
              }
            }
          }
        }
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Input classification
  // ---------------------------------------------------------------------------

  /** Maps raw user input to an internal [[Command]]. */
  private def resolveCommand(input: UserInput): Command = input match {
    case Key(k) if k == SpecialKey.Right => NavigateRight
    case Key(k) if k == SpecialKey.Left  => NavigateLeft
    case Key(k) if k == SpecialKey.Esc   => Exit
    case other                           => DelegateInput(other)
  }

  // ---------------------------------------------------------------------------
  // Command dispatch
  // ---------------------------------------------------------------------------

  /** Routes a [[Command]] to the appropriate handler. */
  private def dispatch[F[_]: Temporal: NConsole](
      command: Command,
      presentation: Presentation[F],
      strategy: PlatformStrategy[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = command match {
    case NavigateRight =>
      handleNavigateRight(presentation, strategy, stateRef, currentIndex, currentWork)
    case NavigateLeft =>
      handleNavigateLeft(presentation, stateRef, currentIndex, currentWork)
    case Exit =>
      handleExit(presentation, currentIndex, currentWork)
    case DelegateInput(input) =>
      handleDelegateInput(presentation, stateRef, currentIndex, currentWork, input)
  }

  // ---------------------------------------------------------------------------
  // Navigation handlers
  // ---------------------------------------------------------------------------

  /** Navigate to the next slide (right arrow).
    *
    * Sequence:
    *   1. Stop the current slide's background show.
    *   2. Cancel the current work fiber.
    *   3. If an out-transition is defined:
    *      a. Activate the ''departing'' slide's layer (`strategy.activateSlide`) so the transition effect renders to
    *         the correct surface. Note: this is the only place where `activateSlide` is called outside of
    *         `executionLoop` — it targets the slide being ''left'', not the one being entered.
    *      b. Race the transition against an interruptible read (so the user can skip it).
    *   4. Set state to switch to `currentIndex + 1` on the next loop iteration.
    *
    * If already on the last slide, this is a no-op.
    */
  private def handleNavigateRight[F[_]: Temporal: NConsole](
      presentation: Presentation[F],
      strategy: PlatformStrategy[F],
      stateRef: Ref[F, ExecutorState],
      currentIndex: Int,
      currentWork: Fiber[F, Throwable, Unit]
  ): F[LoopResult[F]] = {
    val specs = presentation.slideSpecifications
    val current = specs(currentIndex)
    if (currentIndex < specs.length - 1) {
      val nextSlide = specs(currentIndex + 1).slide
      current.slide.stopShow >>
        currentWork.cancel >>
        current.out
          .fold(Monad[F].unit) { t =>
            // Activate the *departing* slide's layer for the transition effect.
            // The *destination* slide's layer will be activated by the executionLoop
            // on the next iteration when switchSlide = true.
            strategy.activateSlide(currentIndex) >>
              Temporal[F]
                .race(
                  t.transition(current.slide, nextSlide),
                  NConsole[F].readInterruptible()
                )
                .as(Monad[F].unit)
          } >>
        stateRef.set(ExecutorState(currentIndex + 1, switchSlide = true)) >>
        Monad[F].pure(Either.left(currentWork))
    } else {
      Monad[F].pure(Either.left(currentWork))
    }
  }

  /** Navigate to the previous slide (left arrow).
    *
    * Stops the current slide's show, cancels its work fiber, and sets state to switch to `currentIndex - 1`. No
    * out-transition is played when navigating backwards, so there is no `strategy.activateSlide` call here — the
    * destination slide's layer will be activated by the `executionLoop` on the next iteration via the `switchSlide`
    * branch.
    *
    * If already on the first slide, this is a no-op.
    */
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
      Monad[F].pure(Either.left(currentWork))
    }
  }

  /** Handle Esc — stop the current slide, cancel work, clear screen, and terminate the loop. */
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

  /** Forward unrecognized input to the current slide's `userInput` handler.
    *
    * After forwarding, checks whether the slide handler triggered a slide switch (e.g., a slide that programmatically
    * calls `setSlide`). If so, stops the current show and cancels work so the next loop iteration performs the switch.
    */
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
          // The slide's userInput handler triggered a slide switch (e.g., via setSlide).
          // Stop the current show and cancel work so the loop performs the switch.
          current.slide.stopShow >>
            currentWork.cancel >>
            Monad[F].pure(Either.left(currentWork))
        } else {
          Monad[F].pure(Either.left(currentWork))
        }
      }
  }
}
