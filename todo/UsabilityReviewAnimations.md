# Usability Review: Creating Animations, Transitions & Custom Slides

**Perspective:** A developer using the lote library to build presentations  
**Scope:** Both terminal (JVM/JLine) and WebGL (browser/Three.js) backends  
**Date:** 2025-07-20

---

## Executive Summary

The library offers a surprisingly ergonomic API for creating custom animations given the complexity of what's happening under the hood (Cats Effect, cross-platform rendering, 3D spatial layout). The `SessionBuilder` DSL makes the happy path easy, and the SPI traits (`Slide[F]`, `Transition[F]`, `Overlay[F]`) are refreshingly minimal. However, the learning curve steepens significantly once you leave the built-in components, and the gap between "use a built-in transition" and "write your own" is substantial.

---

## 1. Built-in Transitions — What Works Well

| Aspect | Assessment |
|--------|-----------|
| **Discoverability** | ✅ Excellent. All transitions are available as builder methods (`.morphTransition()`, `.grabTransition()`, etc.) directly on the slide builder. IDE autocompletion surfaces them immediately. |
| **Zero-config usage** | ✅ One method call, no parameters needed for most transitions. |
| **Parameterisation** | ✅ Optional tuning (e.g. `gravity`, `columnsPerStep`) with sensible defaults. |
| **Platform degradation** | ✅ WebGL-only transitions (smoke, dissolve) automatically fall back to terminal-safe alternatives. Users don't need `if/else` branches. |
| **Fallback override** | ✅ `.withFallback(TransitionType.Grab())` is a clean one-liner to customise terminal fallback behaviour. |

**Verdict:** For users who just want transitions between text slides, the experience is close to ideal. Pick a method, attach it to a slide, done.

---

## 2. Custom Transitions — Pain Points

### 2.1 The Complexity Cliff

Going from `.morphTransition()` to writing your own `Transition[F]` requires understanding:

- The `Transition[F]` trait (2 methods — fine)
- `NConsole[F]` for rendering (clear, write, context, applyEffect)
- `Ticker[F]` and `TickerSubscription[F]` for frame scheduling
- `AnimationSettings` and the step duration model
- `FixedStep` for decoupled animation timing
- `GlideLayer` for sub-pixel rendering on WebGL
- `Deferred[F, Unit]` completion signalling
- `Ref[F]` for mutable state
- `Contextual[F, A]` for dependency injection from the builder

That's 8–9 concepts to internalise before writing a transition that "feels" as good as the built-ins. The `SweepRightTransition` example is 154 lines — substantial for what is visually a simple left-to-right wipe.

**Suggestion:** A `TransitionHelper` or `TransitionLoop` base that handles the ticker subscribe/unsubscribe/done lifecycle would reduce the boilerplate to just "given current progress 0.0–1.0, return a frame". Something like:

```scala
TransitionLoop.create[F](console, ticker, settings) { (from, to, progress) =>
  // return ScreenAdjusted for this progress value
}
```

### 2.2 The Contextual Pattern

The `Contextual[F, Transition[F]]` wrapper is clever but unfamiliar. Users must understand:
- Why they can't just `new MyTransition(...)` directly (because `NConsole`, `Ticker`, etc. only exist inside the builder context)
- The difference between `Contextual` (pure) and `ContextualF` (effectful)
- That `SlideContext` provides the dependencies

The README explains this well, but in practice the pattern adds cognitive overhead compared to simply passing dependencies explicitly.

**Suggestion:** Consider offering a simpler alternative for the common case — e.g. a method on the builder that takes a factory function directly:

```scala
.transition { (console, ticker, settings) => new MyTransition(...) }
```

### 2.3 Platform-Aware Transitions

Writing a transition that works well on *both* terminal and WebGL requires:
1. Checking `console.capabilities` for `PlatformCapability.Effects`
2. Conditionally using `GlideLayer` (WebGL) vs. direct character compositing (terminal)
3. Understanding when `applyEffect(RenderEffect.RenderFloatingChars(...))` is appropriate

The `SweepRightTransition` demonstrates this pattern well, but it's a lot of branching logic for what should ideally be transparent.

**Suggestion:** Document a "terminal-only transition" recipe (simpler, no WebGL concerns) separately from a "cross-platform transition" recipe. Most users will target one platform initially.

---

## 3. Custom Slides — Assessment

### 3.1 The Slide Trait

```scala
trait Slide[F[_]] {
  def content: F[ScreenAdjusted]
  def startShow: F[Unit]
  def stopShow: F[Unit]
  def userInput(input: UserInput): F[Unit]
}
```

**Strengths:**
- ✅ Minimal surface area — 4 methods, all with clear semantics
- ✅ `startShow`/`stopShow` lifecycle is intuitive
- ✅ `userInput` is a simple event handler pattern
- ✅ The `InteractiveSlideExample` demonstrates the pattern concisely

**Pain Points:**
- ❌ **Rendering loop ownership is unclear.** A slide returns `content` (pulled by the framework), but for animations it needs to push frames. This requires subscribing to the `Ticker` manually, maintaining a `Ref` for current frame state, and coordinating with `startShow`/`stopShow`. The pull-vs-push tension isn't immediately obvious.
- ❌ **No built-in animation helper for slides.** Transitions get `FixedStep` + `GlideLayer`, but custom slides must wire these up from scratch. The `ExampleInteractiveSlide` likely has significant boilerplate for frame scheduling.
- ❌ **Screen dimensions require reading `console.context`.** It would be more ergonomic if `content` received `Screen` as a parameter, since almost every slide needs it.

### 3.2 Adding Custom Slides to the Builder

```scala
.addSlideF { builder =>
  builder.addSlideF(ExampleInteractiveSlide.contextual[IO]())
    .map(_.title("WASD Demo"))
}
```

The double-builder pattern (outer `addSlideF` + inner `builder.addSlideF(...)`) is confusing on first encounter. The outer builder configures metadata (title, transition), the inner one provides the slide instance. This distinction isn't obvious from the types alone.

**Suggestion:** Consider whether the simpler `.addSlide(mySlideInstance).title("X")` chain could work, deferring the `Contextual` resolution internally.

---

## 4. Animations (Frame-by-Frame) — Assessment

### 4.1 FixedStep

**Strengths:**
- ✅ Solves the real problem of decoupling simulation from render rate
- ✅ Returns both step count and fractional progress — enables smooth interpolation
- ✅ The pattern is well-documented with clear examples

**Pain Points:**
- ❌ **Requires understanding the `Ref`-based state model.** Users must call `FixedStep.makeRef[F]` and pass the ref to `consumeSteps` on every tick. This is a functional-programming pattern that may confuse users coming from imperative animation frameworks.
- ❌ **The implicit `Clock[F]` requirement** adds another dependency to track. In tests it's the `SimulatedClock`; in production it's... where does it come from? The README mentions it's available in `SlideContext` but the threading isn't always obvious.

### 4.2 GlideLayer

**Strengths:**
- ✅ Handles interpolation, snap-detection, and dispatch automatically
- ✅ The `key` parameter for stable identity is a good design choice
- ✅ No-ops gracefully on terminal

**Pain Points:**
- ❌ **Only useful on WebGL.** Terminal users get no benefit from it, yet the API surface is present and must be understood when reading examples.
- ❌ **The `wrapThreshold` parameter** is non-obvious. What value should a user pick? The relationship between `wrapThreshold` and `columnsPerStep` in the sweep transition isn't immediately clear without reading the implementation.

---

## 5. WebGL-Specific Concerns

### 5.1 3D Spatial Layout

```scala
.at(3600, 2400, 500)
.rotatedBy(10, 0, 0)
.transparentBackground()
```

**Strengths:**
- ✅ Extremely simple API for 3D positioning
- ✅ No-ops silently on terminal — shared code just works
- ✅ Camera transitions are automatic (ease-in-out flight)

**Pain Points:**
- ❌ **No spatial preview on terminal.** Users developing on terminal have zero feedback about their 3D layout. They must switch to the browser to see if positions make sense. A "spatial map" debug view (even ASCII) would help.
- ❌ **Units are magic numbers.** What does `at(3600, 4800, 0)` mean? Are these pixels? Character cells × some factor? The relationship between position values and visual spacing isn't documented.
- ❌ **No relative positioning.** Every slide needs absolute coordinates. For a 33-slide deck, this means managing a coordinate system manually. A layout helper (grid, circle, spiral) would reduce the burden.

### 5.2 RenderEffect

The `RenderEffect` sealed trait (Dissolve, Smoke, Glow, Fade, etc.) is powerful but:
- ❌ **Only accessible via `NConsole.applyEffect`.** There's no builder-level API for "apply glow to this slide". Users must drop into the SPI layer.
- ❌ **No documentation on effect parameters.** What does `Dissolve(progress: Double)` look like at 0.3 vs 0.7? Trial and error is the only way to tune.
- ❌ **Effect lifecycle management** (when to clear, when to reapply) is left to the user.

### 5.3 WebGLConfig

**Strengths:**
- ✅ Well-documented parameters with sensible defaults
- ✅ All optional — override only what you need
- ✅ Camera transition timing formula is documented

**Pain Points:**
- ❌ **Cell dimensions affect slide layout** but this isn't immediately obvious. Changing `cellWidth`/`cellHeight` changes how many characters fit on screen, which changes how content is laid out — a non-obvious side effect.

---

## 6. Terminal-Specific Concerns

### 6.1 What Works Well

- ✅ `TerminalPlatform.jlineTerminal[IO]().use { ... }` is a one-liner setup
- ✅ All built-in transitions work identically
- ✅ No WebGL concepts leak into terminal-only code
- ✅ The testing framework (`SlideTestHarness`) makes terminal animations testable without a real terminal

### 6.2 Pain Points

- ❌ **No hot-reload.** Every change requires recompilation and restart. For animation tuning, this is painfully slow (Scala compile times + sbt overhead).
- ❌ **Terminal flicker** depends on the terminal emulator and is outside the library's control, but there's no guidance on which terminals work best.
- ❌ **Character rendering limitations** (no colour, no bold/italic in the current API) limit what terminal animations can express compared to WebGL.

---

## 7. Testing — A Bright Spot

The `SlideTestHarness` + `TestConsole` + `TestTicker` + `SimulatedClock` testing framework is genuinely excellent:

- ✅ Deterministic, no wall-clock delays
- ✅ `runWithTicking` solves the concurrent-test problem elegantly
- ✅ `fixedSlide` stubs reduce test setup
- ✅ `writtenFrames` makes assertion easy
- ✅ `SessionBuilder.runWith(...)` enables full integration testing

This is one of the strongest aspects of the library from a usability perspective. Custom animations are testable from day one, which is rare in terminal/graphics libraries.

---

## 8. Documentation & Examples

**Strengths:**
- ✅ Progressive example series (Minimal → Advanced) is well-structured
- ✅ README covers the full API with humour that keeps readers engaged
- ✅ Each example has a clear "What To Tweak" section
- ✅ The `SharedAdvancedPresentation` serves as both documentation and proof of cross-platform capability

**Pain Points:**
- ❌ **No "cookbook" for common patterns.** E.g., "How do I make text fade in word by word?" requires assembling knowledge from FixedStep + Ticker + ScreenAdjusted manipulation.
- ❌ **No architecture diagram** showing the relationship between Ticker → FixedStep → GlideLayer → NConsole → actual pixels.
- ❌ **The custom transition example** (`SweepRightTransition`) is complex enough that a simplified "minimum viable transition" (without GlideLayer, without platform branching) would be valuable as a starting point.

---

## 9. Platform Branching in GlideLayer — A Deeper Look

A significant usability issue is that **every transition author must manually branch on platform capabilities**, even though `GlideLayer` was designed to abstract this away. The branching happens because `GlideLayer.render()` is a **no-op on terminal** (line 156 of GlideLayer.scala), which means floating characters simply vanish — nobody composites them onto the character grid.

### The Pattern That Repeats

Every transition that uses `GlideLayer` follows this pattern:

```scala
// At construction time
val supportsFloatingChars: Boolean =
  console.capabilities.contains(PlatformCapability.Effects)

// In the render loop
if (supportsFloatingChars) {
  // WebGL: render background on grid, characters on overlay
  console.writeString(backgroundOnly) *> overlay.render(smoothChars)
} else {
  // Terminal: composite everything onto the character grid manually
  console.writeString(everythingOnGrid)
}
```

This appears in:
- **`SweepRightTransition`** (the example transition — lines 121, 130–135)
- **`GrabTransition`** (internal — lines 150–151, 315–331)
- **`CharactersTransition`** (internal — lines 57–58, 76–103)

### Why This Is a Problem

1. **Every custom transition must understand the platform model.** A user who just wants "characters that move smoothly" must reason about WebGL floating chars vs. terminal grid compositing.
2. **The branching logic is error-prone.** Forgetting the terminal branch means characters are invisible on terminal; forgetting the WebGL branch means no sub-pixel smoothing in the browser.
3. **It contradicts the library's own design goal.** The README says WebGL-only features are "silent no-ops on the terminal" — but for GlideLayer the no-op means *lost characters*, not graceful degradation.

### Proposed Solution

Make `GlideLayer` handle terminal rendering internally, so transition authors always call `overlay.render(chars)` regardless of platform:

**Option A — GlideLayer composites onto the grid on terminal:**

`GlideLayer.render()` on terminal backends would write the `SmoothChar` positions directly to the console's character grid (at integer cell positions) via `NConsole.writeString` or a character-level write API. The transition author always provides `SmoothChars` and always calls `overlay.render()`; the overlay figures out the right rendering strategy.

```scala
// Transition author writes this — no platform branching:
val frame = renderFrame(from, to, revealColumns, width, height)
console.clear() *> console.writeString(frame) *> overlay.render(edgeChars)
```

**Option B — GlideLayer exposes resolved positions for grid compositing:**

`GlideLayer.render()` returns the characters at their final positions (interpolated on WebGL, snapped on terminal) and the caller composites them into the frame. A helper method like `overlay.renderOnto(frame, chars)` could do both: dispatch to WebGL overlay if available, or composite onto the `ScreenAdjusted` string if not.

```scala
// Transition author writes this — still no platform branching:
val frame = renderFrame(from, to, revealColumns, width, height)
overlay.renderOnto(frame, edgeChars).flatMap(console.writeString)
```

**Option B is probably the better fit** because some transitions need different *grid content* depending on whether characters are on the overlay (e.g., `GrabTransition` renders the snake on the overlay on WebGL but composites it into the grid on terminal, and `CharactersTransition` uses `renderBackground()` on WebGL but `render()` on terminal). A single `renderOnto(frame, chars)` method could:
- On WebGL: dispatch chars to the floating overlay, return the frame unchanged (the caller's frame is the "background")
- On terminal: composite chars at integer positions onto the frame, return the modified frame

This would eliminate the most common platform branching pattern from all transitions — both built-in and custom.

### Impact on the Example

The `SweepRightTransition` (the teaching example for custom transitions) would shrink from 154 lines to roughly 120, and the conceptual load drops from "understand platform capabilities and two rendering paths" to "call overlay.renderOnto and move on."

---

## 10. Recommendations Summary

| Priority | Recommendation |
|----------|---------------|
| **High** | **Make `GlideLayer` platform-transparent** — add a `renderOnto(frame, chars)` method (or similar) that handles WebGL floating-char dispatch *and* terminal grid compositing internally, so transition authors never branch on `PlatformCapability.Effects` |
| **High** | Add a `TransitionLoop` helper that handles ticker lifecycle, reducing custom transitions from ~150 lines to ~30 lines for simple cases |
| **High** | Provide a "minimum viable custom transition" example (terminal-only, no GlideLayer, ~40 lines) |
| **Medium** | Document the coordinate system for `.at(x, y, z)` — what units are these? |
| **Medium** | Add a layout helper for spatial mode (e.g., `Layout.grid(rows=3, cols=4, spacing=1800)`) |
| **Medium** | Consider a simpler alternative to `Contextual` for users who find the pattern unfamiliar |
| **Low** | Add a "which terminal emulator works best" section to the README |
| **Low** | Provide an architecture diagram for the animation pipeline |
| **Low** | Document RenderEffect parameters with visual examples (screenshots or ASCII approximations) |

---

## 11. Overall Rating

| Dimension | Terminal | WebGL |
|-----------|---------|-------|
| **Using built-in components** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Writing custom transitions** | ⭐⭐⭐ | ⭐⭐½ |
| **Writing custom slides** | ⭐⭐⭐½ | ⭐⭐⭐ |
| **Animation tuning** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Testing** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Documentation** | ⭐⭐⭐⭐ | ⭐⭐⭐½ |

The library excels at making the common case trivial and provides all the building blocks for advanced use. The main gap is the steep jump from "consumer" to "creator" of custom components — a mid-level abstraction layer (animation loop helpers, simplified factories) would significantly improve the experience for users attempting their first custom transition or animated slide.

