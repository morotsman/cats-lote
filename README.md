# cats-lote

Do you ever have the urge to create truly underwhelming presentations?

Then this might be for you: A lo-tech presentation tool for making presentations in the terminal, built with Scala, [Cats Effect](https://typelevel.org/cats-effect/), and an unreasonable amount of functional programming, because why use PowerPoint when you can write a monad? 

## Features

### 🎞️ Slide Transitions

Because nothing says "professional" like watching ASCII characters tumble off your screen:

- **MorphTransition** – Characters from the current slide gracefully transform into the next slide's characters, one existential crisis at a time.
- **FallingCharactersTransition** – Your carefully crafted content plummets off the screen with configurable gravity. Physics in the terminal. You're welcome.
- **ReplaceTransition** – Characters are progressively replaced with a character of your choosing (e.g. spaces) before revealing the next slide. Minimalism at its finest.
- **GrabTransition** – A multi-line ASCII snake crawls in from the edge, opens its mouth, grabs your slide content, and drags it away. No, we can't explain why either.

### 📐 Text Alignment

Full control over where your text sits on screen, because centering text is apparently hard enough to warrant a feature section:

- **Vertical**: `Up`, `Center`, `Down`
- **Horizontal**: `Left`, `Center`, `Right`

That's 9 whole alignment combinations. Take that, Google Slides.

### 🎨 ANSI Color Support

Built-in ANSI color constants so you can make your terminal presentations almost as colorful as a 1996 GeoCities page:

- Black, Red, Green, Yellow, Blue, Purple, Cyan, White, Gray
- Bold variants for when you really need to make a point
- Reset code for when you've gone too far

### 🧩 Overlays

Overlays that render on top of your slides, because your content alone clearly isn't enough:

- **Timer** – A countdown timer so you know exactly how much of your audience's time you're wasting.
- **ProgressBar** – Shows how far through the presentation you are. Gives your audience hope that it will eventually end. Supports named milestones for section markers via `.withProgressBar(List(Milestone(...)))`.
- **Quick Navigation** – Press `N` to pop up a slide list, use Up/Down arrows to browse, Enter to jump. For when you need to skip ahead before your audience falls asleep.
- **Idle** – An idle-screen animation that activates when you stop presenting. Bugs crawl in from the edges, steal words from your slide, and eventually rearrange themselves to display the current time. It's more entertaining than most presentations.

Pro tip: If you find the animations and transitions too smooth for your liking, use Microsoft Teams when presenting. That will guarantee a lousy frame rate, giving you the stuttering animations and transitions we've all come to love.

### 🏗️ Type-Safe Builder DSL

A type-safe builder pattern using phantom types, because if you're going to build a presentation tool in Scala, you might as well make the compiler yell at you for forgetting a slide:

- **SessionBuilder** – The high-level API that wires everything together: slides, transitions, overlays, and execution. Just call `.run()` and try to contain your excitement.
- **TextSlideBuilder** – Quickly create text-based slides with content, alignment, and transitions.
- **SlideBuilder** – Add custom interactive slides with their own logic, for when plain text isn't over-engineered enough.

The safety guarantees are now explicit:

- `TextSlideBuilder.build()` is only available after `.content(...)`
- `SlideBuilder.build()` is only available after `.addSlide(...)`
- `PresentationBuilder.build()` is only available after at least one slide has been added
- `SessionBuilder.run()` still validates at runtime that the session is not empty

### 🖥️ Interactive Slides

Implement the `Slide[F]` trait to create fully custom, interactive slides that respond to user keyboard input. Finally, audience participation that doesn't involve eye contact.

### 📋 Step-by-Step Slides

`StepByStepSlide` lets you reveal content progressively; press any key to advance to the next stage. Perfect for bullet points you want to dramatically unveil one at a time, as if each one is a plot twist.

### ⚡ Ticker-Based Animation Engine

A `Ticker` algebra provides a subscribe/publish mechanism for frame-by-frame animations. It's basically a game engine, except the game is "watching text move slowly."

Also, in a shocking display of restraint, render cadence and animation speed are separate knobs:

- **`withTickerInterval(...)`** – Controls how often the terminal redraws.
- **`withFrameRate(...)`** – Same thing, but in FPS for people who prefer human-friendly numbers.
- **`withAnimationStep(...)`** – Controls how quickly built-in animations advance.
- **`withAnimationFrameRate(...)`** – Same thing, but in FPS so you can say "25 FPS" instead of "40 milliseconds" like someone who has accepted their fate.

So if you want smoother rendering without turning your snake into a caffeinated railgun, you can do that now.

### 🐍 ASCII Art

The GrabTransition features a multi-frame animated ASCII snake with crawling, mouth-opening, biting, and dragging animations. Peak software engineering.

## Getting Started

### Prerequisites

- Scala 2.13
- sbt
- A terminal (obviously)
- Low expectations

### Running the Examples

```bash
# Clone the repo
git clone https://github.com/morotsman/cats-lote.git
cd cats-lote

# Run the tiny "is this thing alive?" example
sbt "examples/runMain com.github.morotsman.examples.MinimalExample"

# Add titles to plain text slides
sbt "examples/runMain com.github.morotsman.examples.TitlesExample"

# Place text in different parts of the terminal
sbt "examples/runMain com.github.morotsman.examples.AlignmentExample"

# Add transitions between slides
sbt "examples/runMain com.github.morotsman.examples.TransitionsExample"

# Build and use a custom transition
sbt "examples/runMain com.github.morotsman.examples.CustomTransitionExample"

# Turn on overlays and session-wide settings
sbt "examples/runMain com.github.morotsman.examples.SessionFeaturesExample"

# Register a custom overlay for the whole presentation
sbt "examples/runMain com.github.morotsman.examples.CustomOverlayExample"

# Build a stateful overlay with effectful setup
sbt "examples/runMain com.github.morotsman.examples.EffectfulOverlayExample"

# Build an effectful custom slide with step-by-step reveal
sbt "examples/runMain com.github.morotsman.examples.StepByStepExample"

# Focus on a single interactive slide with WASD controls
sbt "examples/runMain com.github.morotsman.examples.InteractiveSlideExample"

# Run the full showcase example with overlays, transitions, step-by-step slides,
# interactive slides, and other suspiciously elaborate terminal behavior
sbt "examples/runMain com.github.morotsman.examples.AdvancedExample"
```

If you want a gradual path through the API, run the examples in this order:

1. `MinimalExample` - the smallest possible presentation with two text slides
2. `TitlesExample` - adds slide titles and shows how quick navigation uses them
3. `AlignmentExample` - shows how to position content on screen
4. `TransitionsExample` - introduces slide transitions
5. `CustomTransitionExample` - shows how to build a user-defined transition
6. `SessionFeaturesExample` - enables timer, progress bar, quick navigation, and frame-rate settings
7. `CustomOverlayExample` - shows how to register a user-defined overlay
8. `EffectfulOverlayExample` - shows how to allocate and register a stateful overlay effectfully
9. `StepByStepExample` - introduces `addSlideF` with a custom step-by-step slide
10. `InteractiveSlideExample` - focuses on custom interactivity and WASD input handling
11. `AdvancedExample` - gives a user-friendly overview of the smaller examples and features in one deck

There is also a dedicated guide in `examples/README.md` with run commands, notes on what to pay attention to in each example, and lightweight screenshot/GIF suggestions for future docs.

When an example enables quick navigation, press `N` to open it, use `↑` / `↓` to move, press `Enter` to jump to the highlighted slide, and press `N` again to leave navigation mode.

If you just want the shortest path from "clone repo" to "text is moving in my terminal," start with `MinimalExample`.

If you want the version that ties the smaller examples together and gives you a user-friendly overview of the feature set in one place, end with `AdvancedExample`.

The showcase example is configured with a fairly sensible balance:

- `withFrameRate(60)` for smoother redraws
- `withAnimationFrameRate(25)` for animation speed that doesn't look like it just discovered espresso

### Build & Test

Verified from this repo:

```bash
# Compile everything from scratch
sbt clean compile

# Run the full test suite
sbt test

# Clean build + tests
sbt clean test
```

If `sbt clean` leaves you with a transient error about creating files under `target/streams`, recreate the missing directories and retry:

```bash
mkdir -p examples/target/streams/_global/ivyConfiguration/_global/streams
mkdir -p lote/target/streams/_global/ivyConfiguration/_global/streams
mkdir -p project/target/streams/_global/ivyConfiguration/_global/streams

sbt test
```

### Navigation

- Use `←` / `→` arrow keys to navigate between slides
- Press `Esc` to leave the presentation immediately (no judgement)
- Press `N` to toggle quick navigation overlay, then `↑` / `↓` to browse and `Enter` to jump

## Quick Example

```scala
import cats.effect._
import com.github.morotsman.lote.builders.SessionBuilder
import com.github.morotsman.lote.interpreter.middleware.Milestone

object MyPresentation extends IOApp.Simple {
  override def run(): IO[Unit] =
    SessionBuilder[IO]()
      .addTextSlide(_ => _.content("Hello, Terminal!"))
      .addTextSlide(_ => _.content("Goodbye!"))
      .run()
}
```

If that feels almost offensively straightforward, good. That means the compiler has not yet had time to develop opinions about your presentation.

When you're ready for the more over-engineered version, `AdvancedExample` shows transitions, overlays, and interactive slides in one place.

If you want section markers in the progress bar, you can do this:

```scala
SessionBuilder[IO]()
  .withProgressBar(List(
    Milestone("Intro", 0),
    Milestone("Demo", 3),
    Milestone("Q&A", 7)
  ))
```

## Animation Tuning

Because terminal rendering is not exactly Pixar, here are the two settings you'll most likely want to tweak:

- **`withTickerInterval(...)`** – redraw frequency
- **`withFrameRate(...)`** – redraw frequency, but in FPS
- **`withAnimationStep(...)`** – built-in animation speed
- **`withAnimationFrameRate(...)`** – built-in animation speed, but in FPS

Example:

```scala
SessionBuilder[IO]()
  .withFrameRate(60)
  .withAnimationFrameRate(25)
```

Same thing, if milliseconds are more your style for some reason:

```scala
SessionBuilder[IO]()
  .withTickerInterval(16.millis)
  .withAnimationStep(40.millis)
```

Some practical settings:

- **Smoother rendering, normal animation speed**
  - `.withTickerInterval(16.millis)`
  - `.withAnimationStep(40.millis)`
- **Less terminal pressure, normal animation speed**
  - `.withTickerInterval(40.millis)`
  - `.withAnimationStep(40.millis)`
- **Smoother rendering, faster built-in animations**
  - `.withTickerInterval(16.millis)`
  - `.withAnimationStep(20.millis)`
- **Smoother rendering, slower built-in animations**
  - `.withTickerInterval(16.millis)`
  - `.withAnimationStep(60.millis)`

If you set `.withTickerInterval(1.milli)`, the terminal will do its best, which is adorable, but not especially effective.

If you prefer FPS, the rough conversions are:

- `60 FPS` ≈ `16.67 ms`
- `30 FPS` ≈ `33.33 ms`
- `25 FPS` = `40 ms`

### Recommended Presets

If tweaking two knobs sounds dangerously close to system administration, here are a few presets:

- **Demo mode** – smooth enough to look intentional, restrained enough to not melt your terminal
  - `.withFrameRate(60)`
  - `.withAnimationFrameRate(25)`
- **Slightly dramatic mode** – smoother redraws, faster transitions, more "behold my ASCII art" energy
  - `.withFrameRate(60)`
  - `.withAnimationFrameRate(41.67)`
- **Laptop fan diplomacy mode** – less redraw pressure, normal animation speed, fewer opportunities for your machine to file a complaint
  - `.withFrameRate(25)`
  - `.withAnimationFrameRate(25)`
- **Leisurely snake mode** – same smooth redraws, but the built-in animations take their sweet time getting anywhere
  - `.withFrameRate(60)`
  - `.withAnimationFrameRate(16.67)`

In short: lower ticker interval = smoother redraws, lower animation step = faster built-in animations, and lower self-respect = trying `1.milli` again to see if maybe this time it works out.

### Interactive Slides with `addSlideF`

For slides that need effectful construction (allocating `Ref`s, building stateful components), use `addSlideF`:

```scala
.addSlideF { implicit ctx => import ctx._
  for {
    slide <- StepByStepSlide.make[IO](Vector(
      "First point",
      "First point\nSecond point",
      "First point\nSecond point\nThird point, mic drop"
    ))
  } yield {
    _.addSlide(slide).title("Agenda")
  }
}
```

## Tech Stack

- **Scala 2.13** – Because one does not simply use a dynamically typed language
- **Cats Effect 3** – Purely functional concurrency for a presentation tool (totally necessary)
- **Cats Core** – Functional programming abstractions your coworkers will love reviewing
- **Monocle** – Optics library for immutable data manipulation, because `copy()` is for quitters
- **JLine 3** – Terminal input handling (the actually practical dependency)

## Project Structure

```
cats-lote/
├── lote/          # Core library
│   └── src/main/scala/com/github/morotsman/lote/
│       ├── algebra/       # Core traits (Slide, Transition, Middleware, etc.)
│       ├── builders/      # Type-safe builder DSL (SessionBuilder, TextSlideBuilder, SlideBuilder)
│       ├── interpreter/   # Implementations
│       │   ├── middleware/    # Internal overlay implementations: Timer, ProgressBar, QuickNavigation, Idle
│       │   └── transition/   # Morph, Falling, Replace, Grab
│       ├── model/         # Data types (Alignment, Screen, Presentation)
│       └── util/          # Colors and helpers
├── examples/      # Example presentations
└── project/       # sbt build configuration
```
