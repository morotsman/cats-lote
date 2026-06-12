# cats-lote

Do you ever have the urge to create truly underwhelming presentations?

Then this might be for you: A lo-tech presentation tool for making presentations in the terminal, built with Scala, [Cats Effect](https://typelevel.org/cats-effect/), and an unreasonable amount of functional programming — because why use PowerPoint when you can write a monad?

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

### 🧩 Middleware & Overlays

Overlays that render on top of your slides, because your content alone clearly isn't enough:

- **Timer** – A countdown timer so you know exactly how much of your audience's time you're wasting.
- **ProgressBar** – Shows how far through the presentation you are. Gives your audience hope that it will eventually end. Supports named milestones for section markers.
- **Idle** – An idle-screen animation that activates when you stop presenting. Bugs crawl in from the edges, steal words from your slide, and eventually rearrange themselves to display the current time. It's more entertaining than most presentations.

### 🏗️ Type-Safe Builder DSL

A type-safe builder pattern using phantom types, because if you're going to build a presentation tool in Scala, you might as well make the compiler yell at you for forgetting a slide:

- **PresentationBuilder** – Compose a full presentation from slides, transitions, overlays, and an optional exit slide.
- **TextSlideBuilder** – Quickly create text-based slides with content, alignment, and transitions.
- **SlideBuilder** – Add custom interactive slides with their own logic, for when plain text isn't over-engineered enough.

### 🖥️ Interactive Slides

Implement the `Slide[F]` trait to create fully custom, interactive slides that respond to user keyboard input. Finally, audience participation that doesn't involve eye contact.

### ⚡ Ticker-Based Animation Engine

A `Ticker` algebra provides a subscribe/publish mechanism for frame-by-frame animations. It's basically a game engine, except the game is "watching text move slowly."

### 🐍 ASCII Art

The GrabTransition features a multi-frame animated ASCII snake with crawling, mouth-opening, biting, and dragging animations. Peak software engineering.

## Getting Started

### Prerequisites

- Scala 2.13
- sbt
- A terminal (obviously)
- Low expectations

### Running the Example

```bash
# Clone the repo
git clone https://github.com/morotsman/cats-lote.git
cd cats-lote

# Start sbt
sbt

# Run the example presentation (prepare to be underwhelmed)
examples/runMain com.github.morotsman.examples.Session1
```

### Navigation

- Use arrow keys to navigate between slides
- Press `q` to quit (no judgement)

## Quick Example

```scala
import cats.effect._
import com.github.morotsman.lote.algebra.{NConsole, Ticker}
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.interpreter.transition._
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}

// Build a presentation (the hard way)
val presentation = PresentationBuilder[IO]()
  .addTextSlide {
    _.content("Hello, Terminal!")
      .transition(MorphTransition())
      .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
  }
  .addTextSlide {
    _.content("Goodbye!")
      .transition(FallingCharactersTransition())
      .alignment(Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
  }
  .build()
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
│       ├── builders/      # Type-safe builder DSL
│       ├── interpreter/   # Implementations
│       │   ├── middleware/    # Timer, ProgressBar, Idle
│       │   └── transition/   # Morph, Falling, Replace, Grab
│       ├── model/         # Data types (Alignment, Screen, Presentation)
│       └── util/          # Colors and helpers
├── examples/      # Example presentations
└── project/       # sbt build configuration
```
