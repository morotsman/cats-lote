# cats-lote — Comprehensive Usability Review (Library User Perspective)

_Reviewed: July 2026_

---

## Executive Summary

cats-lote is a functional, type-safe terminal presentation library built on Cats Effect 3. This review evaluates the library from the perspective of a Scala developer who wants to create terminal (or browser) presentations. The review covers onboarding, daily usage, extensibility, documentation, error experience, and ecosystem fit.

**Overall rating: 7.5/10** — Excellent API design and documentation for its target audience, but significant friction in onboarding, distribution, and the jump from simple to custom slides.

---

## 1. First Impressions & Onboarding

### 1.1 Discovery & Installation

| Aspect | Rating | Notes |
|--------|--------|-------|
| README quality | ⭐⭐⭐⭐⭐ | Exceptional. Humorous, progressive, technically precise. The tone lowers the intimidation factor significantly. |
| Installation instructions | ⭐⭐ | `publishLocal` is the only option. No Maven Central, no GitHub Packages, no binary release. This is a hard stop for most potential users who want to `libraryDependencies += ...` and move on. |
| Prerequisites clarity | ⭐⭐⭐⭐ | Clearly stated (Scala 2.13, sbt, terminal). The "low expectations" line is charming. |
| Time-to-hello-world | ⭐⭐⭐ | If already cloned: ~2 minutes. From scratch (clone, sbt resolve, compile): 5–10 minutes due to sbt cold start and dependency resolution. |

**Key friction:** A user evaluating the library must clone the entire repo and set up a local dependency. There's no way to "kick the tires" without committing to that upfront cost.

### 1.2 Hello World Experience

```scala
SessionBuilder[IO]()
  .addTextSlide(_.content("Hello, Terminal!"))
  .addTextSlide(_.content("Goodbye!"))
  .run()
```

**Verdict:** Outstanding. This is genuinely 3 lines of meaningful code (plus the IOApp boilerplate and terminal resource). The builder reads naturally, the method names are self-documenting, and the result is immediately visible. Compared to other presentation-as-code tools (reveal.js config files, Marp markdown), this is competitive in brevity.

### 1.3 The IOApp Boilerplate

```scala
object MyPresentation extends IOApp.Simple {
  override def run: IO[Unit] =
    TerminalPlatform.jlineTerminal[IO]().use { implicit terminal =>
      // ...
    }
}
```

This is 4 lines of boilerplate that every single presentation must include. For Cats Effect users, this is natural. For others, `IOApp.Simple`, `Resource.use`, and implicit terminals are three concepts to understand before writing slide content.

**Suggestion:** A convenience entry point like `LoteApp` that handles the terminal resource internally would flatten the learning curve:
```scala
object MyPresentation extends LoteApp {
  def presentation = SessionBuilder[IO]()
    .addTextSlide(_.content("Hello!"))
}
```

---

## 2. Core API Ergonomics

### 2.1 SessionBuilder — The Happy Path

| Feature | Ergonomics | Notes |
|---------|-----------|-------|
| Adding text slides | ⭐⭐⭐⭐⭐ | `.addTextSlide(_.content("..."))` is near-perfect. |
| Chaining multiple slides | ⭐⭐⭐⭐⭐ | Fluent builder pattern works beautifully. |
| Adding transitions | ⭐⭐⭐⭐ | `.morphTransition()` on the slide builder is intuitive. |
| Configuring overlays | ⭐⭐⭐⭐ | `.withTimer(...)`, `.withProgressBar(...)` are obvious. |
| Frame rate tuning | ⭐⭐⭐ | Two independent knobs (frame rate vs animation frame rate) is conceptually clean but initially confusing. The presets table in the README helps. |
| Running | ⭐⭐⭐⭐⭐ | `.run()` — can't get simpler. |

### 2.2 TextSlideBuilder — Progressive Complexity

The text slide builder demonstrates excellent progressive disclosure:

1. **Level 1** — `.content("text")` → done
2. **Level 2** — Add `.title("...")`, `.alignment(...)` → still one-liners
3. **Level 3** — Add `.step("...")` for reveals → slightly more setup
4. **Level 4** — Add `.morphTransition()` → still declarative

Each level adds exactly one concept. The user never needs to understand Level 4 to use Level 1. This is textbook API design.

### 2.3 Alignment API

```scala
.alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
```

This is verbose for what it does. Compare:
- Current: `Alignment(VerticalAlignment.Up, HorizontalAlignment.Left)` (58 characters)
- Possible: `Alignment.topLeft` (18 characters)

For the 9 alignment combinations, pre-built constants would reduce friction significantly without losing type safety.

### 2.4 Transition Selection

The built-in transitions are well-named and discoverable through method names on the builder:
- `.morphTransition()` — evocative name, behavior matches expectation
- `.replaceTransition('*')` — clear parameter semantics
- `.fallingCharactersTransition()` — descriptive, if long
- `.grabTransition()` — requires reading docs to understand (a snake?)
- `.smokeTransition()` — clear
- `.dissolveTransition()` — clear

**Issue:** There's no way to preview transitions without running the full presentation. A `TransitionPreview` utility that renders a transition between two fixed strings in a loop would save significant trial-and-error time.

### 2.5 Milestone/Progress Bar

```scala
.withProgressBar(List(
  Milestone("Intro", 0),
  Milestone("Demo", 3),
  Milestone("Q&A", 7)
))
```

The zero-based slide index is error-prone. Adding or removing a slide mid-deck means manually re-numbering all subsequent milestones. A string-based reference (matching slide titles) would be more maintainable:
```scala
Milestone("Intro", slideTitle = "Welcome")
```

---

## 3. Extension Points (Custom Components)

### 3.1 The Complexity Cliff

The library has a dramatic complexity cliff between built-in text slides and custom interactive slides:

| Component | Lines of code | Concepts required |
|-----------|--------------|-------------------|
| Text slide | 1–5 | Builder DSL |
| Custom overlay (stateless) | ~20 | `Overlay[F]` trait, `ScreenAdjusted` |
| Custom overlay (stateful) | ~40 | Above + `Ref`, effectful construction |
| Custom transition | ~40–150 | `Transition[F]` or `TickedTransition`, `FixedStep`, `Deferred`, fibers |
| Custom interactive slide (raw) | 80–350 | `Slide[F]`, `Ref`, `Queue`, `TickerSubscription`, fiber lifecycle, `FixedStep`, `GlideLayer` |
| Custom interactive slide (TickedSlide) | ~20–30 | `TickedSlide` builder, `Ref` |

~~The jump from "text slide user" to "custom slide author" requires understanding ~5 Cats Effect concepts simultaneously. This is the library's most significant usability gap.~~ **Update:** `TickedSlide` significantly narrows this gap. Users still need `Ref` for state, but ticker subscription, FixedStep, GlideLayer lifecycle, and fiber management are fully handled. The jump is now mainly about understanding `Ref` and effectful callbacks.

### 3.2 TickedTransition — A Bright Spot

The `TickedTransition` helper significantly reduces custom transition boilerplate:

```scala
TickedTransition.contextual[IO] { builder =>
  builder.buildProgress(500.millis) { (from, to, ctx) =>
    // ~5 lines of rendering logic
    TickedTransition.ProgressResult.continue(blended)
  }
}
```

This is genuinely well-designed. The four-tier builder (`buildProgress` → `buildStepped` → `buildProgressWithGlide` → `buildWithSetup`) mirrors the progressive complexity philosophy of the rest of the API. **This pattern should be replicated for custom slides.**

### 3.3 Contextual Wrapper

```scala
val myTransition = Contextual[IO, Transition[IO]] { ctx =>
  implicit val console = ctx.console
  implicit val ticker = ctx.ticker
  implicit val settings = ctx.animationSettings
  new MyTransition[IO]()
}
```

The need to manually extract and declare implicits is repetitive. Every custom component example repeats these 3 lines. A convenience method like `ctx.withImplicits { ... }` or automatic implicit provision would clean this up.

### 3.4 Custom Slide SPI

The `Slide[F]` trait is clean:
```scala
trait Slide[F[_]] {
  def content: F[ScreenAdjusted]
  def startShow: F[Unit]
  def stopShow: F[Unit]
  def userInput(input: UserInput): F[Unit]
}
```

~~However, the gap between this simple trait and a working interactive slide is enormous.~~ **Update:** The `TickedSlide` helper (symmetric with `TickedTransition`) closes most of this gap. It eliminates boilerplate around ticker subscription, `FixedStep`, `GlideLayer` lifecycle, and fiber management, offering three progressive tiers:

1. **`build`** — simple tick callback, no FixedStep or GlideLayer
2. **`buildStepped`** — adds FixedStep (discrete simulation steps)
3. **`buildWithGlide`** — adds FixedStep + GlideLayer (auto-cleared on stop)

The `contextual` factory also auto-injects `NConsole`, `Ticker`, and `AnimationSettings` (addressing the repetitive implicit extraction noted in §3.3):

```scala
TickedSlide.contextual[F] { builder =>
  for {
    stateRef <- Ref[F].of(initialState)
    slide    <- builder.build(
      onTick  = renderLogic(stateRef),
      onInput = inputHandler(stateRef),
      onStart = stateRef.set(initialState)
    )
  } yield slide
}
```

**Remaining gap:** Users still manage their own `Ref` and wire state manually in the callbacks. A higher-level `AnimatedSlide[F, S]` abstraction that accepts only `initialState`, `update(state, input) → state`, and `render(state) → ScreenAdjusted` would eliminate this last piece of boilerplate, but the current `TickedSlide` already reduces a 300-line custom slide to ~20–30 lines of user-supplied logic.

---

## 4. Documentation Quality

### 4.1 README

| Aspect | Rating | Notes |
|--------|--------|-------|
| Structure | ⭐⭐⭐⭐⭐ | Table of contents, progressive sections, clear headings. |
| Tone | ⭐⭐⭐⭐⭐ | Self-deprecating humor that makes dense material approachable. |
| Code examples | ⭐⭐⭐⭐⭐ | Every concept has a runnable snippet. |
| API reference completeness | ⭐⭐⭐⭐ | All public methods documented with descriptions. |
| Architecture explanation | ⭐⭐⭐⭐⭐ | The rendering architecture section (bulk vs overlay path) is unusually well-explained for a library this size. |
| Troubleshooting | ⭐⭐ | No troubleshooting section. Common issues (terminal too small, ANSI not supported, sbt compilation errors) are not addressed. |

### 4.2 Examples

The 11 progressive examples are the library's strongest documentation asset:
- Each focuses on exactly one concept
- Comments explain *why*, not just *what*
- Recommended reading order is provided
- The "What You Will Learn" section for each is excellent

**Gap:** No "cookbook" or "recipes" section for common patterns:
- "How do I show a code block with syntax highlighting?" → Not possible (undocumented limitation)
- "How do I add a slide with a bullet list?" → Must use `\n` and manual formatting
- "How do I make text appear character-by-character?" → Must implement custom Slide[F]
- "How do I embed an image (ASCII art)?" → Not addressed

### 4.3 Scaladoc / API Docs

No published Scaladoc. Users must read source code or the README for type signatures. For a library with phantom types and complex type-level constraints, this is a significant gap.

---

## 5. Error Experience

### 5.1 Compile-Time Errors

The phantom-type builder pattern catches missing `.content()` at compile time. However, the error message is a Scala 2 type mismatch:

```
type mismatch;
 found   : TextSlideBuilder[IO, HasNothing]
 required: TextSlideBuilder[IO, HasContent]
```

For users unfamiliar with phantom types, this is opaque. A `@implicitNotFound` annotation could produce:
```
Cannot build slide: .content(...) has not been called. 
Every text slide requires content.
```

### 5.2 Runtime Errors

| Scenario | Current behavior | Ideal behavior |
|----------|-----------------|----------------|
| Terminal too small | Unknown (not documented) | Graceful message: "Terminal must be at least 40×10" |
| No slides added | Likely runtime exception | Compile-time prevention or clear error |
| Invalid milestone index | Silent (renders wrong) | Warning or validation at build time |
| WebGL not available | Unknown | Fallback or clear browser console message |

### 5.3 Debug Experience

No logging, no verbose mode, no way to inspect the render pipeline. When something looks wrong visually, users have no tools to diagnose whether it's their content, the alignment, the terminal, or a library bug.

---

## 6. Testing Support

### 6.1 Test Harness

The `SlideTestHarness` is genuinely excellent:
- One-line setup
- Deterministic (simulated clock, no real I/O)
- Rich assertion API (`writtenFrames`, `clearCount`, `lastWrittenFrame`)
- Supports both manual ticking and auto-ticking
- Well-documented with a showcase spec

This is better testing support than most presentation libraries offer (which is typically zero).

### 6.2 Testing Gaps

- **No visual snapshot testing:** Users can assert on string content but can't easily verify that their slide "looks right" without running it visually.
- **No test for navigation flow:** Can't easily assert "after pressing Right 3 times, I'm on slide 4" without running the full session.
- **`SessionBuilder.runWith` requires exposing internals:** The pattern of extracting `session` as a public method is reasonable but adds boilerplate to production code for testing purposes.

---

## 7. Cross-Platform (JVM + Browser) Experience

### 7.1 Shared Code Model

The cross-compilation story is clean: write once in `shared/`, run on both JVM and JS. The `SharedAdvancedPresentation` proves this works for a complex 33-slide deck.

### 7.2 Feature Degradation

Silent no-ops for unsupported features (`.at()`, `.rotatedBy()` on terminal) is the right design choice. Users don't need platform checks.

### 7.3 Browser Setup Friction

Running browser examples requires:
1. `sbt browserExamples/fastLinkJS`
2. `cd browser-examples`
3. `python3 -m http.server 8080`
4. Open browser

Four steps, two tools (sbt + python), and a directory change. A single `sbt browserExamples/serve` task would be friendlier.

---

## 8. Comparison with Alternatives

| Aspect | cats-lote | reveal.js | Marp | Slidev |
|--------|-----------|-----------|------|--------|
| Input format | Scala DSL | HTML/Markdown | Markdown | Markdown + Vue |
| Learning curve (simple) | Low | Low | Very low | Low |
| Learning curve (custom) | Very high | Medium | Low | Medium |
| Type safety | Strong | None | None | Partial |
| Terminal rendering | Native | No | No | No |
| Distribution | Unpublished | npm | npm | npm |
| Speaker notes | No | Yes | Yes | Yes |
| PDF export | No | Yes | Yes | Yes |
| Theming | No | Yes | Yes | Yes |
| Live reload | No | Yes | Yes | Yes |

cats-lote occupies a unique niche (type-safe, terminal-native, FP-first) but lacks table-stakes features that presentation tools are expected to have.

---

## 9. Missing Features (User Perspective)

### High Impact (users will ask for these immediately)

| Feature | Impact | Effort Estimate |
|---------|--------|----------------|
| **Speaker notes** | Users presenting to an audience need private notes | Medium |
| **Markdown content** | Most users think in markdown, not raw strings | High |
| **Theming / color support** | No way to use ANSI colors in text slides without custom Slide[F] | Medium |
| **Code blocks with highlighting** | Primary use case for developer presentations | High |
| **Bullet/numbered lists** | Must be manually formatted with `\n` and `- ` | Low |
| **Auto-advance / timed slides** | Useful for kiosk/demo mode | Low |

### Medium Impact (nice-to-have)

| Feature | Impact | Notes |
|---------|--------|-------|
| **Slide templates/layouts** | Two-column, title+body, etc. | Common in every other tool |
| **Hot reload** | Edit → see changes without restarting | Huge productivity boost |
| **PDF/image export** | Sharing presentations after the fact | Expected feature |
| **Presenter view** | Current slide + next slide + notes + timer | Standard in presentation tools |
| **Global default transition** | Set once, applies to all slides without per-slide config | Quick win |

---

## 10. Specific Usability Nitpicks

1. **No blank slide shortcut** — Many presenters want to blank the screen temporarily (common `B` key in PowerPoint). Not available.

2. **No slide count indicator** — Users don't know how many slides remain. The progress bar helps but isn't a substitute for "slide 5/12".

3. **`.withTimer(duration)`** — Timer counts down but there's no way to pause it. If you stop presenting to take a question, the timer keeps running.

4. **No undo for navigation** — Pressing left/right during a transition should queue or cancel, not be lost.

5. **Escape exits immediately** — No confirmation. Accidental Esc ends the presentation. A double-tap or confirmation prompt would prevent this.

6. **`.addSlideF` naming** — The `F` suffix is Cats Effect convention but reads oddly to newcomers. `.addSlideEffect` or `.addSlideWithSetup` would be clearer.

7. **Quick navigation shows untitled slides as blank** — Should show slide index or first line of content as fallback.

8. **No way to go to a specific slide by number** — Common in presentation tools (type slide number + Enter).

9. **Terminal resize handling** — Not documented whether the library handles terminal resize mid-presentation or if it renders at the initial size forever.

10. **No "dry run" mode** — Can't validate a presentation (all slides compile, transitions exist, milestones valid) without running it.

---

## 11. Strengths Summary

1. **Best-in-class builder API** — The fluent DSL with phantom types is genuinely one of the best Scala builder APIs I've seen. Progressive complexity is handled perfectly.

2. **Documentation above its weight class** — For a 0.0.1 library, the README is remarkably thorough, well-structured, and entertaining to read.

3. **Testing support is a differentiator** — `SlideTestHarness` with simulated clock and deterministic assertions is better than what most mature libraries offer.

4. **Cross-platform without compromise** — The JVM/JS cross-compilation with silent feature degradation is elegant engineering.

5. **Animation architecture** — `FixedStep` + `GlideLayer` + the bulk/overlay rendering split shows deep thought about performance. The architecture docs explaining *why* are rare and valuable.

6. **Progressive examples** — 11 examples with a recommended order and "What You Will Learn" sections is excellent pedagogy.

---

## 12. Recommendations (Prioritized)

### P0 — Adoption Blockers

1. **Publish to Maven Central** (or at minimum GitHub Packages). No library gains users while requiring `git clone` + `publishLocal`.
2. **Add a `LoteApp` convenience base class** that eliminates the `IOApp` + `TerminalPlatform.use` boilerplate for the 90% case.

### P1 — Reduce the Complexity Cliff

3. **~~Create an `AnimatedSlide[F, S]` abstraction~~** — ✅ Largely addressed by `TickedSlide`. A further `AnimatedSlide[F, S]` (state + update + render, no manual `Ref`) would eliminate the last bit of boilerplate but is now a nice-to-have rather than essential.
4. **Add ANSI color support to `TextSlideBuilder`** — e.g., `.content(styled"Hello ${bold("world")}!")` or a simple markup DSL.
5. **Add pre-built `Alignment` constants** — `Alignment.topLeft`, `Alignment.center`, etc.

### P2 — Missing Table-Stakes Features

6. **Speaker notes** (rendered to a separate terminal or file).
7. **Markdown content support** (at least headings, bullets, bold/italic, code blocks).
8. **Global default transition** so users don't repeat `.morphTransition()` on every slide.
9. **Slide number indicator** overlay (built-in, one-liner to enable).
10. **Blank screen toggle** (press `B` to black out, press again to return).

### P3 — Quality of Life

11. **Improve phantom-type error messages** with `@implicitNotFound`.
12. **Add a troubleshooting section** to the README.
13. **Validate milestones at build time** (index out of bounds → clear error).
14. **Add title-based milestone references** instead of fragile integer indices.
15. **Hot reload support** (`sbt ~run` with automatic re-render on recompile).

---

## 13. Conclusion

cats-lote is a well-crafted library that excels in API design, documentation, and engineering rigor. It's a joy to use for simple text presentations and demonstrates that functional programming can produce genuinely ergonomic APIs.

However, its usability is limited by:
- **Distribution** (not published, biggest single blocker)
- **The complexity cliff** (text slides → custom slides requires a PhD in Cats Effect)
- **Missing standard features** (no colors, no speaker notes, no markdown, no theming)
- **Niche audience** (requires Scala + Cats Effect + terminal enthusiasm)

For its target audience — Scala/FP developers who enjoy terminal aesthetics and want type-safe, testable presentations — it's an 8.5/10 experience on the happy path. For the broader developer community, adoption barriers keep it at 6/10 until publishing and convenience abstractions are addressed.

The library is well-positioned to grow. Its architecture is extensible, its documentation sets a high bar, and its testing story is ahead of the competition. The recommendations above are ordered to maximize user impact with minimum disruption to the existing (excellent) design.

