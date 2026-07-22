# cats-lote

Do you ever have the urge to create truly underwhelming presentations?

Then this might be for you: A lo-tech presentation tool for making presentations in the terminal <sub>(or, if you really must, in a browser with webgl support)</sub>, built with Scala, [Cats Effect](https://typelevel.org/cats-effect/), and an unreasonable amount of functional programming, because why use PowerPoint when you can write a monad?

## Table of Contents

- [What Real Audiences Are Saying](#what-real-audiences-are-saying)
- [Getting Started](#getting-started)
- [Public API Reference](#public-api-reference)
- [Extension Points (SPIs)](#extension-points-spis)
- [Rendering Architecture](#rendering-architecture-why-not-everything-is-a-smoothchar)
- [Animation Tuning Presets](#animation-tuning-presets)
- [Examples](#examples)
- [Testing Custom Components](#testing-custom-components)
- [Build & Test](#build--test)
- [WebGL Support](#webgl-support)
  - [WebGL Configuration](#webgl-configuration)
- [Tech Stack](#tech-stack)
- [Project Structure](#project-structure)

## What Real Audiences Are Saying

> "Oh... what have you done now?"

> "What is that supposed to be?"

> "You sure can do a lot in the terminal."

Trusted by mass audiences of up to three people. Impossible to ignore once it starts rendering in the middle of a standup. Has been described as "technically impressive" by someone who was clearly trying to be polite.

If your goal is to make your colleagues question your priorities, congratulations, you've found the right tool.

---

## Getting Started

### Prerequisites

- Scala 2.13
- sbt
- A terminal (obviously)
- <sub>A browser with WebGL support (look, we're not proud of it either, but it's there if you need it)</sub>
- <sub>Python 3 (for serving the browser examples, yes, we've stooped to requiring a web server)</sub>
- Low expectations

### Installation

Add the dependency to your `build.sbt`:

```scala
// Not yet published to Maven Central – for now, depend on the project locally
lazy val myPresentation = (project in file("my-presentation"))
  .dependsOn(loteJVM)  // or loteJS for browser presentations
```

### Hello, Terminal

```scala
import cats.effect._
import com.github.morotsman.lote.api.TerminalPlatform
import com.github.morotsman.lote.api.builders.SessionBuilder

object MyPresentation extends IOApp.Simple {
  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      SessionBuilder[IO]()
        .addTextSlide(_.content("Hello, Terminal!"))
        .addTextSlide(_.content("Goodbye!"))
        .run()
    }
}
```

If that feels almost offensively straightforward, good. That means the compiler has not yet had time to develop opinions about your presentation.

### Navigation

Once a presentation is running:

- `←` / `→` arrow keys to navigate between slides
- `Esc` to exit (no judgement)
- `N` to toggle quick navigation, then `↑` / `↓` to browse and `Enter` to jump

## Public API Reference

Everything you need lives under `com.github.morotsman.lote.api`. The internal packages are, as the name implies, not your problem.

### `SessionBuilder[F]` – The Entry Point

`SessionBuilder[IO]()` creates a new presentation session. Chain methods to add slides, configure overlays, tune animation, and finally call `.run()` to start the show.

#### Adding Slides

Three ways to add a slide, depending on how much control you want over the universe:

| Method | Description |
| --- | --- |
| `.addTextSlide(builder => ...)` | Add a text slide. The builder must have `.content(...)` called. The compiler will insist. |
| `.addSlide(builder => ...)` | Add a custom interactive slide. The builder must have `.addSlide(...)` called with a `Slide[F]` instance, because apparently just passing a string wasn't complicated enough. |
| `.addSlideF(builder => F[...])` | Same as `.addSlide(...)` but for slides that need effectful construction (allocating `Ref`s, queues, etc.). For when your slide has more infrastructure than most web apps. |

#### Configuring Overlays

Because your slides alone clearly aren't enough:

| Method | Description |
| --- | --- |
| `.withTimer(duration)` | Adds a countdown timer so you know exactly how much of your audience's time you're wasting. |
| `.withProgressBar(milestones)` | Adds a progress bar. Gives your audience hope that the presentation will eventually end. Optionally pass `List(Milestone("Intro", 0), Milestone("Demo", 3))` for section markers. |
| `.withQuickNavigation()` | Adds quick navigation (press `N` to toggle). For when you need to skip ahead before your audience falls asleep. |
| `.withIdleAnimation(timeout)` | Adds an idle-screen animation that triggers after inactivity (default 2 minutes). Bugs crawl in, steal words, tell you the time. More entertaining than most presentations. |
| `.addOverlay(overlay)` | Register a custom `Overlay[F]` instance. For labels, badges, or whatever else you think your terminal needs. |
| `.addOverlay(F[Overlay[F]])` | Register a custom overlay that requires effectful construction. For overlays with commitment issues that can't be created without a `Ref`. |

#### Tuning Animation

Because terminal rendering is not exactly Pixar, here are the knobs:

| Method | Description |
| --- | --- |
| `.withFrameRate(fps)` | Render cadence in FPS (default ≈ 25). |
| `.withTickerInterval(duration)` | Same thing, but in milliseconds for people who prefer suffering. |
| `.withAnimationFrameRate(fps)` | Built-in animation speed in FPS (default ≈ 25). |
| `.withAnimationStep(duration)` | Same thing, in milliseconds. |

These are independent: you can have smooth rendering without making transitions look like they just discovered espresso.

```scala
// Recommended starting point
SessionBuilder[IO]()
  .withFrameRate(60)
  .withAnimationFrameRate(25)
```

#### Other Settings

| Method | Description |
| --- | --- |
| `.onSlideChanged(index => F[Unit])` | Callback invoked on each slide change. In case you want to track how far your audience made it before giving up. |

#### Running

```scala
.run()  // Builds everything and starts the presentation loop.
        // Requires an implicit Terminal[F] in scope (from TerminalPlatform).
        // Returns when the presenter presses Esc.
```

Requires at least one slide and an implicit `Terminal[F]`. On the JVM, get one via `TerminalPlatform.jlineTerminal[IO]().use { implicit terminal => ... }`. The library will tell you if you forget either.

---

### `TextSlideBuilder` – Text Slides

Inside `.addTextSlide(builder => ...)`, the builder exposes:

| Method | Description |
| --- | --- |
| `.content("...")` | **Required.** Sets the slide text. Without this, you have achieved the world's first content-free presentation. |
| `.title("...")` | Names the slide. Shows up in quick navigation so your audience can skip directly to the one slide they care about. |
| `.alignment(Alignment(...))` | Positions text on screen, because centering text is apparently hard enough to warrant an API. |
| `.step("...")` | Adds a progressive reveal fragment. Press any key to show the next one. Dramatic pauses, terminal-style. |
| `.separator("...")` | Sets the separator between steps (e.g., `"\n"`). |
| `.hint("...")` | Sets the "press any key" prompt text, for audiences that need explicit instructions. |
| `.at(x, y, z)` | Sets the 3D world-space position of the slide in CSS-pixel-scale units. A typical 80×24 slide is 800×480 units; use spacing of ~1800 horizontally and ~1200–2400 vertically. Positive x = right, y = down, z = toward camera. On WebGL backends the camera navigates here when the slide becomes active. On terminal backends this is a no-op, which is probably for the best. |
| `.rotatedBy(rx, ry, rz)` | Sets the 3D rotation (Euler angles in degrees) of the slide surface. On terminal backends, also a no-op. Your terminal doesn't rotate, and neither should you. |
| `.transparentBackground()` | Makes the slide's background transparent in spatial mode (WebGL), so 3D scene content behind the slide shows through. On terminal backends, ignored. Transparency is a concept your terminal has not discovered. |

#### Transition Helpers

| Method | Description |
| --- | --- |
| `.morphTransition()` | Characters from the current slide gracefully transform into the next slide's characters, one existential crisis at a time. |
| `.replaceTransition(char)` | Characters are progressively replaced with the given character before revealing the next slide. |
| `.fallingCharactersTransition(gravity, selectAccelerator)` | Your content plummets off the screen with configurable gravity. Physics in the terminal. You're welcome. |
| `.grabTransition(stepSize)` | An ASCII snake crawls in, opens its mouth, grabs your slide content, and drags it away. No, we can't explain why either. |
| `.smokeTransition()` | Characters drift upward and dissipate like smoke. On WebGL backends this uses GPU-accelerated particle effects; on terminal backends it falls back to falling characters, because smoke is hard to render with monospace fonts. |
| `.dissolveTransition()` | Characters fade out with per-cell randomized timing. On WebGL backends this produces a smooth dissolve; on terminal backends it falls back to falling characters. Same reason. |
| `.transition(transition)` | Use a custom `Transition[F]` instance. |

Example with step-by-step reveal:

```scala
.addTextSlide(
  _.content("First point")
    .separator("\n")
    .step("Second point")
    .step("Third point, mic drop")
    .hint("[press any key to continue]")
    .title("Agenda")
)
```

---

### `SlideBuilder` – Custom Interactive Slides

For when plain text isn't over-engineered enough. Inside `.addSlide(builder => ...)` or `.addSlideF(builder => F[...])`, the builder exposes:

| Method | Description |
| --- | --- |
| `.addSlide(slide)` | **Required.** Adds a `Slide[F]` instance. This is where your lovingly hand-crafted interactive slide goes. |
| `.title("...")` | Names the slide. Same as text slides; quick navigation will thank you. |
| `.morphTransition()` | Same transition helpers as text slides. Because custom slides deserve nice exits too. |
| `.replaceTransition(char)` | Progressive character replacement. |
| `.fallingCharactersTransition(...)` | Terminal gravity. |
| `.grabTransition(...)` | Snake-based content removal. |
| `.smokeTransition()` | Smoke dissolve (WebGL) or falling characters (terminal). |
| `.dissolveTransition()` | Fade-out dissolve (WebGL) or falling characters (terminal). |
| `.transition(transition)` | Use a custom `Transition[F]`, if the built-in ones aren't weird enough. |
| `.at(x, y, z)` | 3D world-space position in CSS-pixel-scale units (see `SlidePosition` for coordinate system details). No-op on terminal. |
| `.rotatedBy(rx, ry, rz)` | 3D rotation in degrees (WebGL spatial mode). No-op on terminal. |
| `.transparentBackground()` | Transparent slide background in spatial mode (WebGL). |

---

### Text Alignment

```scala
import com.github.morotsman.lote.api.{Alignment, VerticalAlignment, HorizontalAlignment}

.addTextSlide(
  _.content("Top-left!")
    .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
)
```

Vertical: `Up`, `Center`, `Down`. Horizontal: `Left`, `Center`, `Right`. That's 9 whole alignment combinations. Take that, Google Slides.

---

### `Milestone` – Progress Bar Markers

```scala
import com.github.morotsman.lote.api.Milestone

SessionBuilder[IO]()
  .withProgressBar(List(
    Milestone("Intro", 0),
    Milestone("Demo", 3),
    Milestone("Q&A", 7)
  ))
```

Each milestone has a `label` and a `slideIndex` (zero-based). They show up as named markers above the progress bar so your audience knows how much hope to maintain.

---

## Extension Points (SPIs)

If the built-in features aren't over-engineered enough for your taste, implement these traits to add your own components.

### `Slide[F]` – Custom Slides

```scala
import com.github.morotsman.lote.api.spi.Slide
import com.github.morotsman.lote.api.{ScreenAdjusted, UserInput}

trait Slide[F[_]] {
  def content: F[ScreenAdjusted]       // What to render on screen
  def startShow: F[Unit]               // Called when the slide becomes active
  def stopShow: F[Unit]                // Called when the slide is navigated away from
  def userInput(input: UserInput): F[Unit]  // Called on each keyboard/mouse event
}
```

This is how you build slides that respond to user input, maintain state, run animations, or do anything else that plain text can't. Finally, audience participation that doesn't involve eye contact.

### `Transition[F]` – Custom Transitions

```scala
import com.github.morotsman.lote.api.spi.Transition

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): F[Unit]  // Animate from one slide to the next
  def userInput(input: UserInput): F[Unit]                // Handle input during the transition
}
```

Two methods. That's it. The hard part is deciding what visual atrocity to commit between slides.

If you'd rather not start from scratch, `TickedTransition` handles all the lifecycle boilerplate (ticker subscribe/unsubscribe, `FixedStep`, `Deferred` completion, final-frame rendering) and lets you focus on just the rendering logic. The simplest path is `buildProgress`, which gives you a progress value from 0.0 → 1.0 and screen dimensions:

```scala
import com.github.morotsman.lote.api.support.TickedTransition

// Minimum viable custom transition (~10 lines of logic):
val myTransition = TickedTransition.contextual[IO] { builder =>
  builder.buildProgress(500.millis) { (from, to, ctx) =>
    val cols = (ctx.progress * ctx.screenWidth).toInt
    val blended = ScreenAdjusted(
      from.content.split("\n", -1).zip(to.content.split("\n", -1)).map {
        case (f, t) => t.take(cols) + f.drop(cols)
      }.mkString("\n")
    )
    TickedTransition.ProgressResult.continue(blended)
  }
}

// Use it:
.addTextSlide(_.content("Hello").transition(myTransition))
```

`TickedTransition` offers four builder methods with increasing control:

| Method | What you provide | What's handled for you |
| --- | --- | --- |
| `buildProgress(duration)` | `(from, to, ProgressContext) => ProgressResult` | Ticker lifecycle, FixedStep, screen dimensions, linear progress 0.0→1.0, completion |
| `buildStepped(duration)` | `(from, to, StepContext) => F[Unit]` | Ticker lifecycle, FixedStep, Deferred for completion |
| `buildProgressWithGlide(duration)` | `(from, to, ProgressContext, GlideLayer.Overlay) => ProgressResult` | Everything above + GlideLayer for sub-pixel interpolation |
| `buildWithSetup(...)` | Full control | Only ticker lifecycle |

**Recommended starting point:** [`SimpleEasedWipeTransition`](shared-examples/src/main/scala/com/github/morotsman/examples/slides/SimpleEasedWipeTransition.scala) (~118 lines, ~20 lines of rendering logic) demonstrates easing (`.withEasing(Easing.easeInOutCubic)`), early completion (`ProgressResult.done`), screen-dimension-aware rendering (`ctx.screenHeight`), and content-aware completion — all in one copy-and-modify example.

For simpler starting points, see [`SimpleSweepTransition`](shared-examples/src/main/scala/com/github/morotsman/examples/slides/SimpleSweepTransition.scala) (~40 lines) and [`SimpleWipeTransition`](shared-examples/src/main/scala/com/github/morotsman/examples/slides/SimpleWipeTransition.scala) (~45 lines). Compare with [`SweepRightTransition`](shared-examples/src/main/scala/com/github/morotsman/examples/slides/SweepRightTransition.scala) (~147 lines) to see what `TickedTransition` eliminates.

### `Overlay[F]` – Custom Overlays

```scala
import com.github.morotsman.lote.api.spi.Overlay
import com.github.morotsman.lote.api.{Screen, ScreenAdjusted, UserInput}

trait Overlay[F[_]] {
  def applyOverlay(
      context: Screen,                  // Terminal dimensions
      screenAdjusted: ScreenAdjusted,   // Current rendered content (after other overlays)
      originalContent: ScreenAdjusted   // Original slide content (before any overlays)
  ): F[ScreenAdjusted]

  def onUserInput(userInput: UserInput): F[Unit]  // Optional: react to input
}
```

Register custom overlays with `.addOverlay(...)` on `SessionBuilder`. For overlays that need effectful setup (e.g., allocating a `Ref` for state), use the `F[Overlay[F]]` overload. `onUserInput` is optional; not every overlay needs to eavesdrop on the keyboard.

---

### Supporting Types

The data types you'll encounter when building custom components. None of them bite:

| Type | Description |
| --- | --- |
| `Screen(screenWidth, screenHeight)` | Terminal dimensions in characters. Now you know exactly how little space you have to work with. |
| `ScreenAdjusted(content)` | Rendered content fitted to the terminal. It's a `String` in a trench coat. |
| `UserInput` | Sealed trait: `Key(SpecialKey)`, `Character(Char)`, `MouseClick(x, y)`, `MouseMove(x, y)`. Covers everything your audience might press, click, or accidentally bump. |
| `SpecialKey` | Enum: `Enter`, `Space`, `Up`, `Down`, `Left`, `Right`, `Esc`, `Unknown`, `Timeout`. The `Unknown` variant is for keys even the library doesn't recognize. |
| `AnimationSettings(step)` | Controls built-in animation speed. Typically configured via `SessionBuilder` rather than directly, unless you enjoy making things harder for yourself. |
| `SlidePosition(x, y, z, rotX, rotY, rotZ, transparentBackground)` | Describes the 3D world-space position and rotation of a slide. Coordinates are in CSS-pixel-scale units (an 80×24 slide at default cell size is 800 × 480 units). Positive x = right, y = down, z = toward camera. Use spacing of ~1800 (x) / ~1200–2400 (y) between slides. On WebGL backends this is where the camera navigates to; on terminal backends it's cheerfully ignored. |
| `PlatformCapability` | Sealed trait: `CharacterGrid`, `SubPixelRendering`, `Effects`, `Transforms3D`. Describes what the backend can render. All backends support `CharacterGrid`; WebGL adds the rest. Query via `SlideContext.capabilities` to decide whether your slide deserves particle effects or just more ASCII. |
| `RenderEffect` | Sealed trait for GPU-accelerated visual effects: `Dissolve`, `Smoke`, `Glow`, `Fade`, `MoveCameraTo`, `JumpCameraTo`, `InitSpatialLayout`, `ActivateLayer`, `ClearEffects`. Apply via `NConsole.applyEffect(...)`. Backends that don't support them silently ignore the call, which is the most polite thing a rendering engine has ever done. |

### `Contextual[F, A]` and `ContextualF[F, A]`

Helper wrappers for the inevitable moment when your custom component's factory method needs implicit instances that only exist inside the builder. Instead of threading five parameters through your constructor, wrap it in a `Contextual` and let the builder sort it out:

```scala
import com.github.morotsman.lote.api.builders.Contextual

val myTransition = Contextual[IO, Transition[IO]] { ctx =>
  implicit val console = ctx.console
  implicit val ticker = ctx.ticker
  implicit val settings = ctx.animationSettings
  new SweepRightTransition[IO](columnsPerStep = 3)
}

.addTextSlide(_.content("Hello").transition(myTransition))
```

`ContextualF` is the same thing but effectful, for when even your wrappers need `F`.

The `SlideContext` also exposes platform information:

| Method | Description |
| --- | --- |
| `ctx.capabilities` | Returns a `Set[PlatformCapability]` describing what the backend can render. |
| `ctx.supports(capability)` | Shorthand for checking a single capability. |
| `ctx.scene3DRef` | Returns `Some(Scene3DRef)` on WebGL backends in spatial mode, `None` on terminal. For when you want to add 3D geometry to the shared scene, because apparently text slides weren't enough. |

### `FixedStep` – Animation Timing Helper

If you're building custom transitions or interactive slides with frame-by-frame animation, `FixedStep` decouples your animation speed from the render cadence. Without it, your animations would speed up and slow down depending on how fast the terminal can redraw, which is about as professional as it sounds:

```scala
import com.github.morotsman.lote.api.support.FixedStep

for {
  stateRef        <- FixedStep.makeRef[IO]
  (steps, progress) <- FixedStep.consumeSteps(stateRef) // steps to advance + fractional progress toward next
} yield ()
```

Call `consumeSteps` on each tick and it tells you how many simulation steps to run, plus a fractional `progress` (0.0–1.0) toward the next step. Use `steps` to advance your simulation and `progress` to interpolate smoothly between discrete positions. Your snake crawls at the same speed whether you're at 25 FPS or 60 FPS. Physics, basically.

### `GlideLayer` – Sub-Pixel Character Rendering

If you want characters to glide smoothly between cell positions on WebGL backends (instead of jumping cell-to-cell), `GlideLayer` handles all the interpolation boilerplate for you:

```scala
import com.github.morotsman.lote.api.support.{GlideLayer, SmoothChar}

for {
  overlay <- GlideLayer.make[IO](console, step = 40.millis, wrapThreshold = 2)
  // In your tick callback, give it the current integer cell positions:
  _       <- overlay.render(Vector(
               SmoothChar('*', col = 10, row = 5, key = 0),
               SmoothChar('@', col = 12, row = 5, key = 1)
             ))
  // When done:
  _       <- overlay.clear()
} yield ()
```

You provide integer cell positions each tick. `GlideLayer` automatically:
- Detects when positions change (a new simulation step)
- Interpolates from previous to current positions at sub-pixel precision between steps
- Snaps instead of interpolating when a character jumps more than `wrapThreshold` cells (e.g. screen-edge wrapping)

> **Choosing `wrapThreshold`:** Set it to the maximum distance (in cells) a character might legitimately move in a single simulation step. Characters that jump farther than this will snap to their new position instead of interpolating, which prevents unwanted sliding across the screen during wrap-around. For a game character that moves 1 cell per step, use `wrapThreshold = 1` (the default). For a transition that sweeps 3 columns per step, use `wrapThreshold = 3`.
- Dispatches floating characters to the WebGL renderer via `NConsole.applyEffect`
- No-ops on terminal backends (where the caller composites characters onto the grid instead)

The `key` parameter gives each character stable identity across frames, so if characters are added or removed the interpolation pairs the right previous/current positions.

Together, `FixedStep` + `GlideLayer` is the standard pattern for smooth animations:
1. `FixedStep.consumeSteps` gives you discrete simulation steps + fractional progress
2. Advance your simulation on steps (consistent game logic)
3. `GlideLayer.render` handles the visual interpolation between steps (smooth motion)

---

## Rendering Architecture: Why Not Everything Is a SmoothChar

A natural question when working with `GlideLayer` is: why not just represent _all_ slide content as `SmoothChar`s from the start? If every character were a `SmoothChar`, transitions would be trivial — just move characters from position A to position B and let the overlay interpolate. No need to distinguish between "background" and "overlay" characters.

The answer is **performance**. The library maintains two rendering paths for good reason:

| Path | Mechanism | Cost | Use case |
| --- | --- | --- | --- |
| **Bulk string** | `console.writeString(ScreenAdjusted(...))` | One canvas draw operation for the entire grid | Static slide content, background text |
| **Floating overlay** | `GlideLayer.render(Vector[SmoothChar])` | Individual `fillText()` per character on a separate canvas | Characters that need sub-pixel motion |

On WebGL, the bulk path renders the entire character grid as a single texture in one pass. The floating overlay draws each character individually with `ctx.fillText()` at sub-pixel coordinates on a separate transparent canvas. For a typical 80×24 screen:

- **Bulk path:** 1 draw operation, regardless of content size
- **All-SmoothChar path:** 1,920 individual `fillText()` calls per frame, plus interpolation state tracking (previous positions, step detection, key-based matching) for every character — even ones that never move

At 60 FPS, that's **115,200 `fillText()` calls per second** for a slide that's just sitting there doing nothing. The interpolation bookkeeping (`positionsChanged` scanning a 1,920-element vector, maintaining `OverlayState` maps with 1,920 entries each) adds further overhead that's entirely wasted on static content.

On terminal backends the difference is less dramatic but still present: writing a pre-formed string is cheaper than splitting it into lines, converting to char arrays, and placing 1,920 characters individually.

**The design principle:** only characters that actually move pay the overlay cost. Static content stays on the efficient bulk path. The `GlideLayer.renderOnto` method bridges the two paths so transition authors don't need to think about it:

```scala
// Transition author writes this — no platform branching needed:
val frame = renderBackground(state)   // static content as ScreenAdjusted
val moving = movingChars(state)       // only the characters that are animating
overlay.renderOnto(frame, moving).flatMap { composited =>
  console.clear() *> console.writeString(composited)
}
```

On WebGL, `renderOnto` dispatches `moving` to the floating overlay (with sub-pixel interpolation) and returns `frame` unchanged for the bulk renderer. On terminal, it composites `moving` at integer positions onto `frame` and returns the result. Either way, the transition author writes the same code.

This hybrid approach gives you:
- **Zero overhead for static content** (95% of the time, characters don't move)
- **Sub-pixel smooth motion** for the few characters that are animating
- **One unified API** that works identically on both platforms

If you're building a transition where _many_ characters move simultaneously (like `CharactersTransition` where characters scatter across the screen), the pattern is the same: render the non-moving characters as a background string, promote only the moving ones to `SmoothChar`s. The overlay handles 50–200 floating characters with no issues; it would struggle with 1,920.

---

## Animation Tuning Presets

If tweaking two knobs sounds dangerously close to system administration, here are a few presets:

| Preset | Frame Rate | Animation Frame Rate | Vibe |
| --- | --- | --- | --- |
| **Demo mode** | 60 | 25 | Smooth enough to look intentional |
| **Slightly dramatic** | 60 | 41.67 | More "behold my ASCII art" energy |
| **Laptop fan diplomacy** | 25 | 25 | Fewer opportunities for your machine to file a complaint |
| **Leisurely snake** | 60 | 16.67 | The animations take their sweet time |

In short: higher frame rate = smoother redraws, higher animation frame rate = faster built-in animations, and lower self-respect = trying `1.milli` again to see if maybe this time it works out.

Pro tip: If you find the animations too smooth for your liking, use Microsoft Teams when presenting. That will guarantee a lousy frame rate, giving you the stuttering animations we've all come to love.

---

## Examples

The `examples/` directory contains a progressive set of examples that walk you through the API one concept at a time.

### Running Examples

From the repository root:

```bash
# Clone the repo
git clone https://github.com/morotsman/cats-lote.git
cd cats-lote

# Run any example
sbt "examples/runMain com.github.morotsman.examples.MinimalExample"
```

Replace `MinimalExample` with any of the example names below.

> **IntelliJ users:** if copying from the markdown preview mangles the quotes into `&quot;`, copy from the raw file instead, or type the command manually. The quotes are required by sbt.

### Recommended Order

| # | Example | Focus |
| --- | --- | --- |
| 1 | `MinimalExample` | Smallest possible presentation |
| 2 | `TitlesExample` | Slide titles |
| 3 | `AlignmentExample` | Content placement |
| 4 | `TransitionsExample` | Slide transitions |
| 5 | `CustomTransitionExample` | Creating your own transition |
| 6 | `SessionFeaturesExample` | Timer, progress bar, quick navigation, and frame-rate settings |
| 7 | `CustomOverlayExample` | Adding a custom overlay |
| 8 | `EffectfulOverlayExample` | An overlay with state and setup |
| 9 | `StepByStepExample` | Progressive reveals with `.step(...)` |
| 10 | `InteractiveSlideExample` | An interactive slide with WASD controls |
| 11 | `AdvancedExample` | A full feature tour, including interactivity |

If you just want the shortest path from "clone repo" to "text is moving in my terminal," start with `MinimalExample`.

If you want the version that ties everything together and gives you a user-friendly overview of the feature set in one place, end with `AdvancedExample`.

### What You Will Learn in Each Example

Pick the example that best matches what you want to try next. Or run them all in order. We're not your manager.

#### `MinimalExample`
- `SessionBuilder[IO]()` plus two `addTextSlide` calls. That's the whole thing.
- The smallest deck that still runs. Proof that the library does, in fact, work.

#### `TitlesExample`
- Adds `.title(...)` to plain text slides so they have names like civilized presentations.
- Enables quick navigation so you can see titles in action immediately.
- Press `N` to open the menu, `↑` / `↓` to move, `Enter` to jump, `N` again to close.
- Shows why naming slides early is a good habit. Unnamed slides show up as blank entries in the navigation menu, which is about as helpful as it sounds.

#### `AlignmentExample`
- Uses `.alignment(...)` with top-left, center, and bottom-right layouts.
- Shows that positioning is per-slide, so you can scatter content across the terminal like confetti.
- Useful for understanding how sparse content can still feel intentional rather than just lonely.

#### `TransitionsExample`
- Introduces the built-in transition helpers: `.replaceTransition(...)`, `.morphTransition()`, `.fallingCharactersTransition()`, `.smokeTransition()`, and `.dissolveTransition()`.
- Keeps the deck simple so the transition itself is the star. For once.
- Helps you choose between transitions based on the vibe you want, not just the visual effect.
- On WebGL backends, `.smokeTransition()` and `.dissolveTransition()` use GPU-accelerated effects; on terminal they gracefully degrade to falling characters.

#### `CustomTransitionExample`
- Shows how to build your own `Transition[F]` when the built-in ones aren't weird enough.
- Walks through the main transition loop so you can see how the effect comes together frame by frame.
- Highlights the split between render cadence (`withFrameRate`) and transition speed (`withAnimationFrameRate`): two independent knobs for two independent anxieties.
- Points out the first tuning knobs to try: frame rate, animation frame rate, and `columnsPerStep`.

#### `SessionFeaturesExample`
- Enables `withTimer`, `withProgressBar`, `withQuickNavigation`, `withFrameRate`, and `withAnimationFrameRate`, all the session-wide settings in one place.
- Shows that these are configured once on `SessionBuilder` and apply everywhere, because who wants to configure things per slide? (Don't answer that.)
- A good place to try pressing `N` and exploring quick navigation.

#### `CustomOverlayExample`
- Shows how to register your own overlay with `.addOverlay(...)`.
- Overlays are session-wide, so they show up on every slide whether the slide asked for it or not.
- Breaks the implementation into a small three-step pattern that's easy to steal for your own overlays.
- A good fit for labels, badges, and other decorations that demand constant attention.

#### `EffectfulOverlayExample`
- Shows the effectful `addOverlay(F[Overlay[F]])` overload, for overlays that need to allocate state before they can exist.
- Demonstrates overlays that react to user input through `onUserInput(...)` and update in real time.
- Shows how a `Ref` can be updated in `onUserInput(...)` and read back in `applyOverlay(...)`. State management in a presentation tool. We've come so far.

#### `StepByStepExample`
- Shows how `addTextSlide` supports progressive reveal directly with `.content(...)`, `.step(...)`, `.separator(...)`, and `.hint(...)`.
- Bridges the gap between plain text slides and fully interactive custom slides. All the drama of progressive disclosure, none of the custom `Slide[F]` boilerplate.

#### `InteractiveSlideExample`
- Focuses on a single interactive slide with WASD controls.
- Shows the full `Slide[F]` lifecycle: `startShow`, `stopShow`, `content`, and `userInput`.
- Uses `Animator.make` plus `ExampleInteractiveSlide.make` inside `addSlideF`.
- Demonstrates that a custom `Slide[F]` can keep its own state, input handling, and rendering behavior. It's basically a tiny game running inside your presentation, which is exactly what your audience was hoping for.
- Ends with concrete ideas for what to tweak if you want to build your own custom slide.

#### `AdvancedExample`
- The friendly overview deck that ties the smaller examples together in one session.
- Uses `SharedAdvancedPresentation` under the hood, the same presentation code that runs in the browser with WebGL. On the terminal, spatial features (3D positioning, camera flight) are no-ops, but everything else works identically.
- Covers titles, alignment, built-in and custom transitions, overlays, milestones, quick navigation, step-by-step slides, and interactive slides. All the greatest hits.
- Still demonstrates the features live, including idle animation and progress milestones.
- Best when you want to see how the pieces fit together, or when you want to show someone what you've built and need it to look like you planned it.

### Building Custom Components

If the built-in features aren't enough (and honestly, they probably are, but here we are), each custom component is built around one of the three SPIs:

- **Custom transition** → implement `Transition[F]`. You control how one slide turns into the next. The possibilities are endless. The results are terminal.
- **Custom overlay** → implement `Overlay[F]`. You decorate rendered slide content and optionally react to input. Session-wide, always watching.
- **Custom slide** → implement `Slide[F]`. You control rendering, lifecycle, and input handling. It's basically writing a small application that happens to live inside a presentation.

The examples build on each other in escalating order of ambition:

1. `CustomTransitionExample`: animate between two slides
2. `CustomOverlayExample`: decorate already-rendered slide content
3. `EffectfulOverlayExample`: allocate state before attaching an overlay
4. `InteractiveSlideExample`: full custom slide with rendering, lifecycle, and input

When reading the custom examples, keep these questions in mind (they'll save you from reading the source code more than once):

- What type does this component implement?
- Where does it get attached to the session or slide builder?
- Which method contains the rendering or animation logic?
- Which settings or constructor parameters are the easiest to tweak first?

---

## Testing Custom Components

Because even terminal presentations deserve test coverage. And well, I needed it myself for writing the tests for this library so it's kind of there, and sharing is caring, right?

The `testkit` package (`com.github.morotsman.lote.testkit`) provides a complete set of test doubles so you can unit-test your custom slides, transitions, and overlays without a real terminal. No `IO.sleep`, no flickering screen, no existential dread.

### `SlideTestHarness` – One-Line Setup

The harness bundles a fake console, a controllable ticker, and a simulated clock into a single object. Create one, hand its fields to your component, and assert on the results:

```scala
import cats.effect.IO
import com.github.morotsman.lote.api._
import com.github.morotsman.lote.testkit.SlideTestHarness
import munit.CatsEffectSuite
import scala.concurrent.duration._

class MyTransitionSpec extends CatsEffectSuite {

  test("transition completes and shows target content") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 24),   // simulated terminal dimensions
        tickStep = 5.millis        // duration per tick
      )
      from = SlideTestHarness.fixedSlide[IO]("Hello")
      to   = SlideTestHarness.fixedSlide[IO]("Goodbye")
      transition = {
        implicit val clock = harness.clockInstance
        MyTransition.create[IO](harness.console, harness.ticker, harness.animationSettings)
      }
      _       <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield assert(written.head.contains("Goodbye"))
  }
}
```

That's it. No real terminal, no wall-clock delays, deterministic results. The test runs in milliseconds because the simulated clock doesn't actually sleep. It just pretends time has passed, which is more than most of us can say on a Monday morning.

### `SlideTestHarness.make` Parameters

| Parameter | Default | Description |
| --- | --- | --- |
| `screen` | `Screen(80, 24)` | Simulated terminal dimensions. Pick something small for focused tests. |
| `inputs` | `Nil` | Pre-loaded `UserInput` events consumed by `console.read()`. |
| `tickStep` | `16.millis` | Duration per tick. Matches the default render cadence. |
| `animationStep` | same as `tickStep` | Simulation step for `AnimationSettings`. Override if you want animation to advance at a different rate. |
| `readDelay` | `Duration.Zero` | Delay before each `console.read()` returns. Set to e.g. `1.millis` for integration tests using `SessionBuilder.runWith`, so slide fibers have time to write content. |

### Manual Ticking and `runWithTicking`

Call `harness.tick(n)` to advance the simulated clock by `n` steps and fire all subscriber callbacks. Great for slides and animations where you want to assert on specific intermediate states:

```scala
test("my slide renders after 3 ticks") {
  for {
    harness <- SlideTestHarness.make[IO](tickStep = 16.millis)
    slide   <- MySlide.create[IO](harness.console, harness.ticker, harness.animationSettings)
    _       <- slide.startShow
    _       <- harness.tick(3)   // advances clock by 3×16ms, fires callbacks 3 times
    frames  <- harness.writtenFrames
  } yield assert(frames.nonEmpty)
}
```

For transitions and other components that **block until completion** (via `Deferred.get`), you can't call `tick()` after the blocking call because it would never be reached. Use `runWithTicking` instead, which forks the blocking task and ticks alongside it:

```scala
test("sweep transition completes without hanging") {
  for {
    harness <- SlideTestHarness.make[IO](
      screen = Screen(10, 1),
      tickStep = 5.millis
    )
    from = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA")
    to   = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB")
    transition = /* ... create transition using harness ... */
    _ <- harness.runWithTicking(transition.transition(from, to))
    written <- harness.writtenFrames
  } yield assertEquals(written.head, "BBBBBBBBBB")
}
```

`runWithTicking` fires a bounded number of ticks (default 10) with a 1ms pause between each, giving the task fiber scheduling time between ticks. The tick count is explicit, making it easy to reason about how much simulated time passes.

> **How many ticks does my transition need?** For a duration-based transition (e.g., `buildProgress(500.millis)`) with `tickStep = 5.millis`, you need at least `500 / 5 = 100` ticks. Add some headroom (e.g., 150) for scheduling and completion. For step-based transitions, count: `totalSteps × (animationStep / tickStep)` ticks. When in doubt, use a generous tick count — extra ticks after completion are harmless.

### Harness API

Once you have a harness, these are the methods you'll reach for:

| Method | Returns | Description |
| --- | --- | --- |
| `harness.console` | `TestConsole[F]` | The fake console. Pass it to your component. |
| `harness.ticker` | `TestTicker[F]` | The controllable ticker. Pass it to your component. |
| `harness.animationSettings` | `AnimationSettings` | Animation config derived from `tickStep` / `animationStep`. |
| `harness.clockInstance` | `AnimationClock[F]` | The simulated clock. Use as an implicit for `FixedStep`. |
| `harness.tick(n)` | `F[Unit]` | Fire `n` ticks manually. Advances the simulated clock and fires subscriber callbacks. |
| `harness.runWithTicking(task, n)` | `F[Unit]` | Fork `task`, fire `n` ticks alongside it, then join. Use for blocking operations like transitions. |
| `harness.writtenFrames` | `F[List[String]]` | All frames written to console, most recent first. |
| `harness.writtenFramesInOrder` | `F[List[String]]` | Same, but oldest first. For people who read left to right. |
| `harness.lastWrittenFrame` | `F[Option[String]]` | The most recently rendered frame. |
| `harness.clearCount` | `F[Int]` | How many times `clear()` was called. |
| `harness.enqueueInputs(inputs)` | `F[Unit]` | Inject additional user inputs mid-test. |
| `harness.reset` | `F[Unit]` | Clear all recorded frames and counters. For multi-phase tests. |
| `harness.clock.currentTime` | `F[FiniteDuration]` | Inspect current simulated time. Useful for asserting that animations finish promptly. |

### `SlideTestHarness.fixedSlide` – Stub Slides

When testing transitions, you typically don't care about the slide implementation. You just need something that returns known content. `fixedSlide` is that thing:

```scala
val from = SlideTestHarness.fixedSlide[IO]("OLD CONTENT")
val to   = SlideTestHarness.fixedSlide[IO]("NEW CONTENT")
```

It returns a `Slide[F]` whose `content` is a `ScreenAdjusted` wrapping the given string, and whose `startShow`/`stopShow`/`userInput` are all no-ops. The minimum viable slide, if you will.

### Using Individual Test Doubles

If `SlideTestHarness` is too opinionated for your test, you can use the pieces individually:

```scala
import com.github.morotsman.lote.testkit.{TestConsole, TestTicker, SimulatedClock}

for {
  console <- TestConsole.make[IO](screen = Screen(40, 10))
  ticker  <- TestTicker.make[IO](step = 10.millis)
  clock   <- SimulatedClock.make[IO]()
} yield ()
```

`SimulatedClock` is especially useful if you're testing `FixedStep` logic in isolation. Advance time, call `consumeSteps`, assert on the step count. No sleeping involved, just pure deterministic time manipulation:

```scala
for {
  clock   <- SimulatedClock.make[IO]()
  stepper <- FixedStep.makeRef[IO](clock, implicitly)
  _       <- clock.advance(48.millis)
  steps   <- FixedStep.consumeSteps(stepper, 16.millis)(clock)
} yield assertEquals(steps, 3)  // 48ms / 16ms = 3 steps
```

### Testing Tips

- **Keep screen dimensions small** in tests. A `Screen(4, 1)` is easier to assert on than `Screen(80, 24)`. Your test assertions will thank you.
- **Use `runWithTicking`** for transitions. They block on a `Deferred` internally, so they need ticks to fire concurrently.
- **Use manual `tick()`** for slides and interactive components. You get precise control over how many frames are rendered.
- **`fixedSlide` content is NOT padded.** Unlike production slides that go through the `Aligner`, `fixedSlide` returns raw content. This is intentional: it lets you test transition logic without fighting screen-width padding.

### Complete Feature Showcase

For a comprehensive, runnable tour of every test framework feature (`TestConsole`, `SimulatedClock`, `FixedStep`, `TestTicker`, `SlideTestHarness`, `runWithTicking`, overlay testing, `reset`, `readDelay`, and more), see [`TestFrameworkShowcaseSpec.scala`](lote/shared/src/test/scala/com/github/morotsman/lote/testkit/TestFrameworkShowcaseSpec.scala). Each section introduces one concept with commented examples that you can copy into your own tests.

### Testing `TickedTransition`-Based Transitions

If your transition uses `TickedTransition.contextual[F]`, the test pattern is:

1. Get an `AnimationClock` from `harness.clockInstance`
2. Get a pre-wired builder via `TickedTransition.forTest(harness)`
3. Pass the builder to your transition's factory method

```scala
class SimpleSweepTransitionSpec extends CatsEffectSuite {
  test("sweep completes and shows target content") {
    for {
      harness <- SlideTestHarness.make[IO](screen = Screen(10, 1), tickStep = 5.millis)
      from     = SlideTestHarness.fixedSlide[IO]("AAAAAAAAAA")
      to       = SlideTestHarness.fixedSlide[IO]("BBBBBBBBBB")
      transition = {
        implicit val clock: AnimationClock[IO] = harness.clockInstance
        SimpleSweepTransition.create[IO](500.millis, TickedTransition.forTest(harness))
      }
      _       <- harness.runWithTicking(transition.transition(from, to))
      written <- harness.writtenFrames
    } yield assertEquals(written.head, "BBBBBBBBBB")
  }
}
```

For complete working test files, see [`SimpleSweepTransitionSpec`](examples/src/test/scala/com/github/morotsman/examples/slides/SimpleSweepTransitionSpec.scala) and [`SimpleWipeTransitionSpec`](examples/src/test/scala/com/github/morotsman/examples/slides/SimpleWipeTransitionSpec.scala).

### Testing a Full Presentation

Testing individual components is all well and good, but at some point you'll want to know if the whole thing actually works when you wire it together. `SessionBuilder.runWith` lets you run your entire presentation (slides, overlays, middleware, the full existential stack) against a test harness instead of a real terminal.

The trick: expose your `SessionBuilder` from the presentation object so tests can call `.runWith(...)` instead of `.run()`. It's the same session, just pointed at a fake terminal that won't judge your slide content:

```scala
// In your presentation object
object EffectfulOverlayExample extends IOApp.Simple {

  /** The session configuration, exposed so tests can exercise it
    * without needing a real terminal or a willing audience.
    */
  def session: SessionBuilder[IO] =
    SessionBuilder[IO]()
      .addOverlay(InputStatusOverlay.make[IO]())
      .addTextSlide(_.content("...").title("Slide One"))
      .addTextSlide(_.content("...").title("Slide Two"))

  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      session.run()
    }
}
```

Then feed it pre-loaded inputs and let it run. The session reads from the input queue, renders frames, and exits when it hits `Esc`. Just like production, except nobody has to sit through it:

```scala
// In your test spec
class EffectfulOverlayExampleSpec extends CatsEffectSuite {

  private val readDelay = 1.millis

  test("session starts and exits on Esc") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- EffectfulOverlayExample.session.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield assert(written.nonEmpty)
  }

  test("navigating right shows second slide") {
    for {
      harness <- SlideTestHarness.make[IO](
        screen = Screen(80, 10),
        inputs = List(Key(SpecialKey.Right), Key(SpecialKey.Esc)),
        readDelay = readDelay
      )
      _ <- EffectfulOverlayExample.session.runWith(harness.console, harness.ticker)
      written <- harness.writtenFrames
    } yield {
      val allContent = written.mkString("\n")
      assert(allContent.contains("Slide Two"))
    }
  }
}
```

The `readDelay` parameter adds a small pause between consuming pre-loaded inputs. In production, `console.read()` blocks on the keyboard so the slide has all the time in the world to render. In tests, inputs are available immediately, so without a small delay the session would consume them faster than your slides can write frames. `1.millis` is enough to be polite about it.

Why bother? Because this tests the **actual production configuration**, the same slides, the same overlays, the same middleware chain. If you add a slide, rearrange the deck, or accidentally break an overlay, the tests will catch it. The alternative is maintaining a separate test-only session definition that starts out identical to production and then quietly drifts until you discover the divergence during a live demo. Which is a learning experience, just not the kind you want.

---

## Build & Test

In case you want to verify that the code compiles before trusting it with your presentation career:

```bash
# Compile everything from scratch
sbt clean compile

# Run the full test suite
sbt test

# Clean build + tests
sbt clean test
```

---

## WebGL Support

Some say the terminal is not enough for them. To those people I say: <sub>we have WebGL</sub>.

For audiences that fail to appreciate the greatness of the terminal, a browser-based WebGL fallback is available. It renders the same presentations, just with fewer principles. Think of it as the accessibility ramp for people who haven't yet discovered that `terminal` is a lifestyle.

### How It Works

The core library (`lote/`) is cross-compiled for both JVM and JS using Scala.js. The public API (`SessionBuilder`, `Slide[F]`, `Transition[F]`, `Overlay[F]`, and all the builder DSLs) is identical on both platforms. The difference is in the terminal backend:

| Platform | Backend | What you get |
| --- | --- | --- |
| JVM | JLine terminal | The real thing. Text in a terminal. Your ancestors would be proud. |
| JS (browser) | Three.js / WebGL | Slides rendered as textured planes in a 3D scene. Same content, more polygons than strictly necessary. |

The pattern is: you write your presentation once using the shared API, and then run it on whichever backend your audience deserves.

### Cross-Platform Presentations

The `shared-examples/` module contains `SharedAdvancedPresentation`, a full 33-slide presentation that compiles and runs on **both** platforms from the same source code. The terminal examples use it, and the browser examples use it. Same slides, same overlays, same transitions, same interactive worm game.

```scala
// Terminal (examples/)
TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
  SharedAdvancedPresentation.build[IO]().run()
}

// Browser (browser-examples/)
TerminalPlatform.threeJsTerminal[IO](container).use { implicit terminal =>
  SharedAdvancedPresentation.build[IO](
    landscape3DSlide = Some(Landscape3DSlide.contextual[IO]())
  ).run()
}
```

The browser version can optionally inject platform-specific slides (like a 3D landscape with rolling hills and roaming figures) via a parameter. When running on the terminal, that slot falls back to the ASCII landscape instead. The rest of the deck is byte-for-byte identical, which is either elegant or obsessive depending on how you feel about code reuse.

### Feature Degradation

Not every feature is available on every backend. The library handles this gracefully: WebGL-only features are silent no-ops on the terminal, not compile errors:

| Feature | Terminal | WebGL |
| --- | --- | --- |
| Text slides, overlays, step-by-step reveals | ✓ | ✓ |
| Transitions (morph, grab, falling, replace) | ✓ | ✓ |
| `.smokeTransition()` | Falls back to falling characters | GPU-accelerated smoke particles |
| `.dissolveTransition()` | Falls back to falling characters | Per-character randomized fade-out |
| `.at(x, y, z)` positioning | No-op (slides swap in place) | Camera flies between positions |
| `.rotatedBy(rx, ry, rz)` | No-op | Slide surface rotated in 3D |
| `.transparentBackground()` | No-op | 3D scene content visible behind slide |
| `RenderEffect` (Glow, Fade, Dissolve, Smoke) | No-op | GPU-accelerated visual effects |
| Custom 3D scene geometry (`scene3DRef`) | Not available | Shared Three.js scene |

This means you can use every spatial and effect method freely in shared code without `#ifdef`-style platform checks. The terminal simply ignores what it can't render, which is a level of quiet professionalism we should all aspire to.

### Querying Platform Capabilities

If you're writing a custom transition and want to produce richer visuals on WebGL while keeping a terminal fallback, you can query the platform at runtime:

```scala
val myTransition = Contextual[IO, Transition[IO]] { ctx =>
  if (ctx.supports(PlatformCapability.Effects))
    // Use GPU-accelerated dissolve
    MyFancyDissolveTransition(ctx.console, ctx.ticker, ctx.animationSettings)
  else
    // Fall back to something the terminal can manage
    MySimpleFadeTransition(ctx.console, ctx.ticker, ctx.animationSettings)
}
```

Available capabilities: `CharacterGrid` (all backends), `SubPixelRendering`, `Effects`, `Transforms3D`.

### Running the WebGL Examples

```bash
# Compile the Scala.js output
sbt browserExamples/fastLinkJS

# Serve the files (must be from browser-examples/, not project root)
cd browser-examples
python3 -m http.server 8080
```

Then open [http://127.0.0.1:8080/](http://127.0.0.1:8080/) in your browser of shame. The compiled JS uses ES modules, so you must serve over HTTP. Opening `index.html` as a `file://` URL will not work, which is the universe's way of adding one more step between you and your compromise.

For a fast edit-compile-refresh cycle, run `sbt ~browserExamples/fastLinkJS` in watch mode in a separate terminal.

> **Incremental compilation gotcha:** The browser examples depend on `shared-examples`, where the presentation logic lives. sbt's incremental compiler sometimes doesn't notice changes in cross-project dependencies, so if your edits to `shared-examples` aren't showing up, clean before rebuilding:
> ```bash
> sbt "sharedExamplesJS/clean; browserExamples/clean; browserExamples/fastLinkJS"
> ```
> Or, if subtlety has failed you: `sbt clean browserExamples/fastLinkJS`.

### WebGL Configuration

The WebGL renderer ships with sensible defaults, but if you're the kind of person who adjusts kerning in terminal presentations, you'll appreciate the `WebGLConfig` knobs. Pass a customised config to `TerminalPlatform.threeJsTerminal` to tune the renderer without touching internals:

```scala
import com.github.morotsman.lote.api.{TerminalPlatform, WebGLConfig}

val config = WebGLConfig(
  nearClip = 0.1,
  farClip = 50000.0,
  fieldOfView = 60.0,
  backgroundColor = "#1a1a2e",
  cameraTransitionBaseMs = 500.0
)

TerminalPlatform.threeJsTerminal[IO](container, config).use { implicit terminal =>
  SessionBuilder[IO]()
    .addTextSlide(_.content("Configured!"))
    .run()
}
```

All parameters have defaults, only specify the ones you want to override.

#### Clipping Planes

| Parameter | Default | Description |
| --- | --- | --- |
| `nearClip` | `1.0` | Near clipping plane distance (world units). Geometry closer than this to the camera is not rendered. Decrease for close-up content; increase to improve depth-buffer precision in large scenes. |
| `farClip` | `100000.0` | Far clipping plane distance (world units). Geometry farther than this from the camera is not rendered. Decrease to improve depth precision; increase if distant slides are being clipped. |

Tightening the near/far range improves depth-buffer precision (fewer z-fighting artefacts), but setting them too tight will clip slide content. The defaults are deliberately generous.

#### Camera & Field of View

| Parameter | Default | Description |
| --- | --- | --- |
| `fieldOfView` | `50.0` | Base vertical field-of-view in degrees. Smaller values zoom in (telephoto effect); larger values zoom out (wide-angle). Interactive zoom (`+`/`-`/`0` keys) adjusts relative to this base. |
| `maxZoom` | `20.0` | Maximum interactive zoom multiplier. Caps how far in the audience can zoom with `+`/`=`. |
| `maxFieldOfView` | `160.0` | Widest field-of-view allowed when zooming out (degrees). Prevents the fish-eye distortion that makes your audience question reality. |

#### Camera Transitions

| Parameter | Default | Description |
| --- | --- | --- |
| `cameraTransitionBaseMs` | `800.0` | Minimum duration (milliseconds) of an animated camera fly-through between slides. Lower values make short hops snappier; higher values add gravitas. |
| `cameraTransitionFactor` | `1.8` | Scaling factor for distance-dependent duration. The total transition time is `baseMs + factor × √distance`. Higher values make long journeys take proportionally more time. |

The transition duration grows with the square root of the travel distance, so short hops are fast while cross-scene flights remain watchable without becoming a screensaver.

#### Rendering

| Parameter | Default | Description |
| --- | --- | --- |
| `antialias` | `true` | Enable multi-sample anti-aliasing on the WebGL renderer. Produces smoother edges at a small GPU cost. Disable on low-end hardware if performance is more important than aesthetics (a sentence no one should have to write). |
| `backgroundColor` | `"#000000"` | Scene background colour as a CSS hex string. Set to match your slide theme, or to `"#ff00ff"` if you want to make a statement. |
| `devicePixelRatio` | `None` | Override the display scaling factor. `None` uses the browser's `window.devicePixelRatio` (recommended). Set explicitly to e.g. `Some(1.0)` to reduce GPU load on HiDPI displays, at the cost of slightly blurrier rendering. |

#### Cell Grid

| Parameter | Default | Description |
| --- | --- | --- |
| `cellWidth` | `10` | Width of a single monospace character cell in CSS pixels. Affects how many columns fit on screen and the resolution of the offscreen text canvases. |
| `cellHeight` | `20` | Height of a single monospace character cell in CSS pixels. Affects how many rows fit on screen. |

Changing cell dimensions is useful if you want denser text (smaller cells) or more readable text at a distance (larger cells). The defaults match typical monospace font metrics.

### Spatial Mode

The WebGL backend supports spatial mode: slides can be positioned and rotated in 3D space using `.at(x, y, z)` and `.rotatedBy(rx, ry, rz)`. The camera smoothly navigates between slides as you present. Combined with `.transparentBackground()`, you can have 3D scene content (rolling hills, castles, roaming figures, whatever your ASCII art ambitions demand) visible behind your slides.

On terminal backends, all spatial methods are no-ops. The same presentation code runs on both platforms; the terminal just quietly ignores the parts it can't handle.

### Available Browser Examples

| Example | Description |
| --- | --- |
| `AdvancedWebGLExample` | The full `SharedAdvancedPresentation` with 3D spatial layout and a landscape scene. The same 33 slides that run in the terminal, plus a 3D landscape with rolling hills, castles, and roaming figures. |

The default (and currently only) main class is `AdvancedWebGLExample`. See `browser-examples/README.md` for details.

### When to Use It

| Scenario | Recommended Renderer |
| --- | --- |
| You respect your craft | Terminal |
| Your audience "needs visuals" | WebGL |
| Someone asks for "something more professional" | WebGL, reluctantly |
| You are presenting to other terminal enthusiasts | Terminal, obviously |
| You want to embed a presentation on a web page | WebGL, but know that you've compromised |
| You want to prove the same code runs everywhere | Both, smugly |

The WebGL renderer exists so you don't have to explain to a room full of project managers why your presentation is "that black window with the white text." It's a concession, not a feature. But it's there, and it works, and we won't judge you. Much.


---

## Tech Stack

- **Scala 2.13** – Because one does not simply use a dynamically typed language
- **Cats Effect 3** – Purely functional concurrency for a presentation tool (totally necessary)
- **Cats Core** – Functional programming abstractions your coworkers will love reviewing
- **Scala.js** – Compiles to JavaScript so your presentation can run in a browser, for when the terminal is too avant-garde
- **scalajs-dom** – DOM bindings for Scala.js (WebGL canvas, event handling, the usual browser rituals)
- **JLine 3** – Terminal input handling (the actually practical dependency)

## Project Structure

```
cats-lote/
├── lote/                  # Core library (cross-compiled JVM + JS)
│   ├── shared/src/main/scala/com/github/morotsman/lote/
│   │   ├── api/
│   │   │   ├── builders/  # SessionBuilder, TextSlideBuilder, SlideBuilder, Contextual
│   │   │   ├── spi/       # Extension points: Slide, Transition, Overlay, NConsole, Ticker
│   │   │   └── support/   # FixedStep animation helper
│   │   ├── testkit/       # Test doubles: SlideTestHarness, TestConsole, TestTicker, SimulatedClock
│   │   └── internal/      # Not your problem
│   ├── jvm/src/            # JVM-specific: JLine terminal backend
│   └── js/src/             # JS-specific: xterm.js + WebGL terminal backends
├── shared-examples/       # Example code shared between terminal and browser targets
├── examples/              # Terminal example presentations (+ tests using the testkit)
├── browser-examples/      # Browser/WebGL example presentations (Scala.js)
└── project/               # sbt build configuration
```
