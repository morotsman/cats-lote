# Examples Guide

This guide walks you through the library one concept at a time.

## How to run an example

From the repository root:

```bash
sbt "examples/runMain com.github.morotsman.examples.MinimalExample"
```

Replace the class name with any of the examples below.

## Recommended order

| Example | Focus | Run command |
| --- | --- | --- |
| `MinimalExample` | smallest possible presentation | `sbt "examples/runMain com.github.morotsman.examples.MinimalExample"` |
| `TitlesExample` | slide titles | `sbt "examples/runMain com.github.morotsman.examples.TitlesExample"` |
| `AlignmentExample` | content placement | `sbt "examples/runMain com.github.morotsman.examples.AlignmentExample"` |
| `TransitionsExample` | slide transitions | `sbt "examples/runMain com.github.morotsman.examples.TransitionsExample"` |
| `CustomTransitionExample` | creating your own transition | `sbt "examples/runMain com.github.morotsman.examples.CustomTransitionExample"` |
| `SessionFeaturesExample` | timer, progress bar, quick navigation, and frame-rate settings | `sbt "examples/runMain com.github.morotsman.examples.SessionFeaturesExample"` |
| `CustomOverlayExample` | adding a custom overlay | `sbt "examples/runMain com.github.morotsman.examples.CustomOverlayExample"` |
| `EffectfulOverlayExample` | an overlay with state and setup | `sbt "examples/runMain com.github.morotsman.examples.EffectfulOverlayExample"` |
| `StepByStepExample` | `addSlideF` and progressive reveals | `sbt "examples/runMain com.github.morotsman.examples.StepByStepExample"` |
| `InteractiveSlideExample` | an interactive slide with WASD controls | `sbt "examples/runMain com.github.morotsman.examples.InteractiveSlideExample"` |
| `AdvancedExample` | a full feature tour, including interactivity | `sbt "examples/runMain com.github.morotsman.examples.AdvancedExample"` |

## What you will learn in each example

Use this section to pick the example that best matches what you want to try next.

## Building custom components

If you want to go beyond the built-in features, start by looking at the main type each example is built around:

- A custom transition implements `Transition[F]`, which lets you define how one slide changes into the next and how the transition reacts to input.
- A custom overlay implements `Overlay[F]`, which lets you add content around rendered slides and optionally react to input.
- A custom slide implements `Slide[F]`, which lets you control rendering, lifecycle hooks, and input handling.

The examples build on each other in this order:

- `CustomTransitionExample` shows how to animate between two slides.
- `CustomOverlayExample` shows how to decorate already-rendered slide content.
- `EffectfulOverlayExample` shows how to allocate state before attaching an overlay to the session.
- `InteractiveSlideExample` shows how a full custom slide handles rendering, lifecycle, and input together.

When reading the custom examples, keep the same four questions in mind:

- What type does this component use?
- Where do you attach it to your session or slide builder?
- Which method contains the rendering or animation logic?
- Which settings or constructor parameters are the easiest to tweak first?

### `MinimalExample`
- Uses `SessionBuilder[IO]()` plus two `addTextSlide` calls.
- Shows the smallest deck that still runs.
- Best when you just want to confirm the library is working.

### `TitlesExample`
- Adds `.title(...)` to plain text slides.
- Also enables quick navigation so you can see titles in use right away.
- Walks through what titles do, how to add them, and where they show up in navigation.
- Press `N` to open the menu, use `↑` / `↓` to move, and press `Enter` to jump to the selected slide.
- Press `N` again to leave quick navigation without selecting a slide.
- Shows why naming slides early is a good habit.

### `AlignmentExample`
- Uses `.alignment(...)` with top-left, center, and bottom-right layouts.
- Shows that positioning is configured per slide, so different slides can use different layouts.
- Shows the kinds of slide content that fit naturally in each position.
- Useful for understanding how sparse content can still feel intentional.

### `TransitionsExample`
- Introduces `.transition(...)` on text slides.
- Shows `ReplaceTransition`, `MorphTransition`, and `FallingCharactersTransition` separately.
- Helps you choose between the built-in transitions based on the kind of presentation flow you want, not just the visual effect.
- Keeps the deck simple so the transition itself is the main thing to notice.

### `CustomTransitionExample`
- Shows how to build your own transition when the built-in ones are not enough.
- Shows how to attach your own `Transition[F]` to a slide.
- Demonstrates that custom transitions follow the same timing model as the built-in transitions.
- Highlights the split between render cadence (`withFrameRate`) and transition speed (`withAnimationFrameRate`).
- Walks through the main transition loop so you can see how the effect comes together frame by frame.
- Points out the first tuning knobs to try: frame rate, animation frame rate, and `columnsPerStep`.
- A good next step after `TransitionsExample` if you want your deck to have a custom visual style.

### `SessionFeaturesExample`
- Enables `withTimer`, `withProgressBar`, `withQuickNavigation`, `withFrameRate`, and `withAnimationFrameRate`.
- Shows that these are configured once on `SessionBuilder` and apply to the full presentation.
- Connects the visible overlays to the session settings that enable them.
- A good place to try pressing `N` and exploring quick navigation.

### `CustomOverlayExample`
- Shows how to register your own overlay with `addOverlay(...)`.
- Demonstrates that overlays are session-wide and run whenever a slide is rendered.
- Explains the main `Overlay[F]` methods and how a simple overlay can decorate rendered content.
- Breaks the implementation into a small three-step pattern that is easy to reuse.
- A good fit for labels, badges, and other static decorations that should stay visible across slides.

### `EffectfulOverlayExample`
- Shows the effectful `addOverlay(F[Overlay[F]])` overload.
- Demonstrates that overlays can react to user input through `onUserInput(...)` and update while the current slide stays on screen.
- Explains how state stored in a `Ref` can be updated in `onUserInput(...)` and then shown in `applyOverlay(...)`.
- A good reference when an overlay needs setup work before it can be attached.
- Useful when an overlay needs state, subscriptions, or other effectful setup.

### `StepByStepExample`
- Introduces `addSlideF` for effectful slide construction.
- Uses `StepByStepSlide.make` to reveal content progressively.
- Explains that the slide is built from a sequence of stages, with one stage per reveal step.
- Bridges the gap between plain text slides and fully interactive custom slides.

### `InteractiveSlideExample`
- Focuses only on the interactive slide, without the rest of the advanced showcase around it.
- Starts with a short introduction that explains the `Slide[F]` lifecycle and how a custom slide is attached.
- Uses `Animator.make` plus `ExampleInteractiveSlide.make` inside `addSlideF`.
- Reacts to `w`, `a`, `s`, and `d` input so you can see how a custom `Slide[F]` can respond to user actions.
- Highlights that a custom `Slide[F]` can keep its own state, input handling, and rendering behavior while it stays on screen.
- Calls out the `Slide[F]` lifecycle methods so you can connect the example code to the parts you may need in your own slide.
- Ends with concrete ideas for what to tweak next if you want to build your own custom slide.

### `AdvancedExample`
- Acts as the friendly overview deck for the smaller examples.
- Explains the main feature groups in one place: titles, alignment, built-in and custom transitions, overlays, milestones, quick navigation, custom overlays, step-by-step slides, and interactive slides.
- Still demonstrates the features live, including idle animation, progress milestones, and navigation overlays.
- Best when you want to see how the pieces fit together in one session.

## Navigation reminders

- Use `←` and `→` to move between slides
- Press `Esc` to exit the presentation
- Press `N` to toggle quick navigation in examples that enable it
- In quick navigation, use `↑` / `↓` to move and `Enter` to select a slide
- Press `N` again to leave quick navigation mode


