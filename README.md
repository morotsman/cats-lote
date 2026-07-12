# cats-lote

Do you ever have the urge to create truly underwhelming presentations?

Then this might be for you: A lo-tech presentation tool for making presentations in the terminal, built with Scala, [Cats Effect](https://typelevel.org/cats-effect/), and an unreasonable amount of functional programming, because why use PowerPoint when you can write a monad?

## Getting Started

### Prerequisites

- Scala 2.13
- sbt
- A terminal (obviously)
- Low expectations

### Installation

Add the dependency to your `build.sbt`:

```scala
// Not yet published to Maven Central – for now, depend on the project locally
lazy val myPresentation = (project in file("my-presentation"))
  .dependsOn(lote)
```

### Hello, Terminal

```scala
import cats.effect._
import com.github.morotsman.lote.api.builders.SessionBuilder

object MyPresentation extends IOApp.Simple {
  override def run: IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide(_.content("Hello, Terminal!"))
      .addTextSlide(_.content("Goodbye!"))
      .run()
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
| `.addTextSlide(builder => ...)` | Add a text slide. The builder must have `.content(...)` called — the compiler will insist. |
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
        // Returns when the presenter presses Esc.
```

Requires at least one slide. The library will tell you if you forget.

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

#### Transition Helpers

| Method | Description |
| --- | --- |
| `.morphTransition()` | Characters from the current slide gracefully transform into the next slide's characters, one existential crisis at a time. |
| `.replaceTransition(char)` | Characters are progressively replaced with the given character before revealing the next slide. |
| `.fallingCharactersTransition(gravity, selectAccelerator)` | Your content plummets off the screen with configurable gravity. Physics in the terminal. You're welcome. |
| `.grabTransition(stepSize)` | An ASCII snake crawls in, opens its mouth, grabs your slide content, and drags it away. No, we can't explain why either. |
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
| `.title("...")` | Names the slide. Same as text slides — quick navigation will thank you. |
| `.morphTransition()` | Same transition helpers as text slides. Because custom slides deserve nice exits too. |
| `.replaceTransition(char)` | Progressive character replacement. |
| `.fallingCharactersTransition(...)` | Terminal gravity. |
| `.grabTransition(...)` | Snake-based content removal. |
| `.transition(transition)` | Use a custom `Transition[F]`, if the built-in ones aren't weird enough. |

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

Register custom overlays with `.addOverlay(...)` on `SessionBuilder`. For overlays that need effectful setup (e.g., allocating a `Ref` for state), use the `F[Overlay[F]]` overload. `onUserInput` is optional — not every overlay needs to eavesdrop on the keyboard.

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

### `FixedStep` – Animation Timing Helper

If you're building custom transitions or interactive slides with frame-by-frame animation, `FixedStep` decouples your animation speed from the render cadence. Without it, your animations would speed up and slow down depending on how fast the terminal can redraw, which is about as professional as it sounds:

```scala
import com.github.morotsman.lote.api.support.FixedStep

for {
  stateRef <- FixedStep.makeRef[IO]
  steps    <- FixedStep.consumeSteps(stateRef) // returns how many animation steps to advance
} yield ()
```

Call `consumeSteps` on each tick and it tells you how many simulation steps to run. Your snake crawls at the same speed whether you're at 25 FPS or 60 FPS. Physics, basically.

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
- Shows why naming slides early is a good habit — unnamed slides show up as blank entries in the navigation menu, which is about as helpful as it sounds.

#### `AlignmentExample`
- Uses `.alignment(...)` with top-left, center, and bottom-right layouts.
- Shows that positioning is per-slide, so you can scatter content across the terminal like confetti.
- Useful for understanding how sparse content can still feel intentional rather than just lonely.

#### `TransitionsExample`
- Introduces the built-in transition helpers: `.replaceTransition(...)`, `.morphTransition()`, and `.fallingCharactersTransition()`.
- Keeps the deck simple so the transition itself is the star. For once.
- Helps you choose between transitions based on the vibe you want, not just the visual effect.

#### `CustomTransitionExample`
- Shows how to build your own `Transition[F]` when the built-in ones aren't weird enough.
- Walks through the main transition loop so you can see how the effect comes together frame by frame.
- Highlights the split between render cadence (`withFrameRate`) and transition speed (`withAnimationFrameRate`) — two independent knobs for two independent anxieties.
- Points out the first tuning knobs to try: frame rate, animation frame rate, and `columnsPerStep`.

#### `SessionFeaturesExample`
- Enables `withTimer`, `withProgressBar`, `withQuickNavigation`, `withFrameRate`, and `withAnimationFrameRate` — all the session-wide settings in one place.
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
- Bridges the gap between plain text slides and fully interactive custom slides — all the drama of progressive disclosure, none of the custom `Slide[F]` boilerplate.

#### `InteractiveSlideExample`
- Focuses on a single interactive slide with WASD controls.
- Shows the full `Slide[F]` lifecycle: `startShow`, `stopShow`, `content`, and `userInput`.
- Uses `Animator.make` plus `ExampleInteractiveSlide.make` inside `addSlideF`.
- Demonstrates that a custom `Slide[F]` can keep its own state, input handling, and rendering behavior. It's basically a tiny game running inside your presentation, which is exactly what your audience was hoping for.
- Ends with concrete ideas for what to tweak if you want to build your own custom slide.

#### `AdvancedExample`
- The friendly overview deck that ties the smaller examples together in one session.
- Covers titles, alignment, built-in and custom transitions, overlays, milestones, quick navigation, step-by-step slides, and interactive slides. All the greatest hits.
- Still demonstrates the features live, including idle animation and progress milestones.
- Best when you want to see how the pieces fit together — or when you want to show someone what you've built and need it to look like you planned it.

### Building Custom Components

If the built-in features aren't enough (and honestly, they probably are, but here we are), each custom component is built around one of the three SPIs:

- **Custom transition** → implement `Transition[F]`. You control how one slide turns into the next. The possibilities are endless. The results are terminal.
- **Custom overlay** → implement `Overlay[F]`. You decorate rendered slide content and optionally react to input. Session-wide, always watching.
- **Custom slide** → implement `Slide[F]`. You control rendering, lifecycle, and input handling. It's basically writing a small application that happens to live inside a presentation.

The examples build on each other in escalating order of ambition:

1. `CustomTransitionExample` — animate between two slides
2. `CustomOverlayExample` — decorate already-rendered slide content
3. `EffectfulOverlayExample` — allocate state before attaching an overlay
4. `InteractiveSlideExample` — full custom slide with rendering, lifecycle, and input

When reading the custom examples, keep these questions in mind (they'll save you from reading the source code more than once):

- What type does this component implement?
- Where does it get attached to the session or slide builder?
- Which method contains the rendering or animation logic?
- Which settings or constructor parameters are the easiest to tweak first?

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

## Tech Stack

- **Scala 2.13** – Because one does not simply use a dynamically typed language
- **Cats Effect 3** – Purely functional concurrency for a presentation tool (totally necessary)
- **Cats Core** – Functional programming abstractions your coworkers will love reviewing
- **JLine 3** – Terminal input handling (the actually practical dependency)

## Project Structure

```
cats-lote/
├── lote/          # Core library
│   └── src/main/scala/com/github/morotsman/lote/
│       ├── api/
│       │   ├── builders/  # SessionBuilder, TextSlideBuilder, SlideBuilder, Contextual
│       │   ├── spi/       # Extension points: Slide, Transition, Overlay, NConsole, Ticker
│       │   └── support/   # FixedStep animation helper
│       └── internal/      # Not your problem
├── examples/      # Example presentations
└── project/       # sbt build configuration
```
