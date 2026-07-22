# cats-lote — Maintainability Review (Fourth Pass)

_Reviewed: July 2026_

---

## Summary

Since the third review, the project has introduced a **high-level animation builder layer** (`TickedSlide`, `TickedTransition`, `GlideLayer`, `Easing`) that dramatically reduces the boilerplate required to create animated slides and transitions. A `SpatialState` extraction has partially addressed the ThreeJsTerminal god-object concern. The test toolkit (`SlideTestHarness`) has been enhanced with `forTest()` factory integration and a comprehensive `TestFrameworkShowcaseSpec` (631 lines) demonstrating all testing patterns.

The codebase now contains **~55 main source files** in the library proper (down from the 102 reported previously due to reclassification — shared-examples and browser-examples are now clearly separated as example code). The shared library core has grown from 20 to ~25 files with the new `api/support/` module. Test coverage has improved in examples (8 test files demonstrating the new APIs) but the JS-side gap remains.

The major theme of this review: **the library developer experience has improved significantly**, but the gap between the polished shared-layer APIs and the untested JS rendering internals has widened.

---

## Changes Since Third Review

| Previous Concern | Status | What Changed |
|------------------|--------|--------------|
| Null usage in executor factory | ⚠️ Unchanged | `onSlideChange: Int => F[Unit] = null` still present |
| Builder DSL adapter boilerplate | ✅ Mitigated | New `TickedSlide`/`TickedTransition` builders bypass most ceremony for common cases |
| Single TODO in codebase (idle + animated slides) | ⚠️ Unchanged | Still unresolved |
| Race condition in `handleDelegateInput` | ⚠️ Unchanged | Not atomic |
| `IdleAwareNConsole` not testable via `runWith()` | ⚠️ Unchanged | Documented but still a gap |
| ThreeJsTerminal — 538-line god object | ⚠️ Partially addressed | `SpatialState` extracted (197 lines), but ThreeJsTerminal still 533 lines |
| Zero test coverage for JS-only code | ⚠️ Unchanged | Still 0 JS-specific tests |
| Heavy `js.Dynamic` usage | ⚠️ Unchanged | `ThreeJsFacade` not extended |
| Duplicated particle spawning | ⚠️ Unchanged | Still duplicated |
| Landscape3DSlide — 673 lines of untyped JS | ⚠️ Unchanged | |
| Resize handling incomplete | ⚠️ Unchanged | |
| Scene3DRef `Option[Any]` pattern | ⚠️ Unchanged | |
| Three.js version not pinned | ⚠️ Unchanged | |
| Scala 3 migration | ⚠️ Unchanged | Still Scala 2.13.15 with kind-projector 0.13.3 |

### Major New Features

| Feature | Impact | Key Files |
|---------|--------|-----------|
| **TickedTransition builder** | Eliminates ~60% transition boilerplate; 4 build variants | `TickedTransition.scala` (562 lines) |
| **TickedSlide builder** | Eliminates slide ticker lifecycle boilerplate; 3 build variants | `TickedSlide.scala` (260 lines) |
| **GlideLayer** | Platform-agnostic smooth character interpolation | `GlideLayer.scala` (327 lines) |
| **Easing library** | 14+ built-in easing functions | `Easing.scala` (133 lines) |
| **AnimationSettings** | Configurable simulation step duration | `AnimationSettings.scala` (17 lines) |
| **Contextual[F, A]** | DI pattern for builders — framework injects dependencies | `Contextual.scala` (31 lines) |
| **TickedTransition.forTest()** | Zero-ceremony test harness integration | `TickedTransition.scala` |
| **SpatialState extraction** | Partial refactor of ThreeJsTerminal mutable state | `SpatialState.scala` (197 lines) |
| **TestFrameworkShowcaseSpec** | 631-line comprehensive testing documentation | `TestFrameworkShowcaseSpec.scala` |
| **IdleDetector algebra** | Inactivity detection with noop default | `IdleDetector.scala` (50 lines) |
| **Milestone** | Named slide bookmarks | `Milestone.scala` (4 lines) |
| **ScreenAdjusted** | Screen-content wrapper type | `ScreenAdjusted.scala` (4 lines) |

---

## Architecture at a Glance (Updated)

```
┌─────────────────────────────────────────────────────────────────┐
│  Public API  (shared/api/)                                      │
│  ├── builders/  SessionBuilder, BuilderDslAdapters, Contextual   │
│  ├── spi/       Terminal, EffectfulTerminal, NConsole, Slide,    │
│  │              Transition, Overlay, Ticker                      │
│  ├── support/   ★ NEW HIGH-LEVEL LAYER ★                        │
│  │   ├── TickedTransition  (progress/stepped/glide/setup builds) │
│  │   ├── TickedSlide       (simple/stepped/glide builds)         │
│  │   ├── GlideLayer        (smooth character interpolation)      │
│  │   ├── Easing            (14+ animation curves)                │
│  │   ├── FixedStep         (fixed-rate simulation steps)         │
│  │   └── Clock             (AnimationClock abstraction)          │
│  └── data types (SlidePosition, RenderEffect, PlatformCapability,│
│                  WebGLConfig, TransitionType, AnimationSettings,  │
│                  Milestone, ScreenAdjusted, Screen, etc.)        │
├─────────────────────────────────────────────────────────────────┤
│  Testkit (shared/testkit/)                                       │
│  └── SlideTestHarness, TestConsole, TestTicker, SimulatedClock   │
│      TickedTransition.forTest(), TickedSlide.forTest()            │
├─────────────────────────────────────────────────────────────────┤
│  Internal (shared/internal/)                                     │
│  ├── algebra/     PresentationExecutor, PlatformStrategy,        │
│  │                Feature, IdleDetector, Middleware               │
│  ├── model/       Presentation, SlideSpecification               │
│  ├── builders/    Internal builder implementations               │
│  ├── interpreter/                                                │
│  │   ├── PresentationExecutorInterpreter (command dispatch)      │
│  │   ├── SpatialPlatformStrategy (WebGL layer/camera mgmt)       │
│  │   ├── TerminalPlatformStrategy (no-op for JVM)                │
│  │   ├── ticker/     TickerInterpreter                           │
│  │   ├── middleware/ Feature impls (Timer, Progress, QuickNav,   │
│  │   │               Idle)                                       │
│  │   ├── transition/ Morph, Replace, Falling, Grab, Smoke,       │
│  │   │               WithFallback                                │
│  │   └── nconsole/   NConsoleInterpreter, Aligner,               │
│  │                   AnsiFrameRenderer, AnsiParser               │
│  └── tools/         ScreenHelper, Colors, Symbols                │
├─────────────────────────────────────────────────────────────────┤
│  JVM-only (jvm/)                                                 │
│  ├── api/  TerminalPlatform (JLine entry point)                  │
│  └── internal/  JLineTerminal                                    │
├─────────────────────────────────────────────────────────────────┤
│  JS-only (js/)                                                   │
│  ├── api/  TerminalPlatform, WebGLConfig, Scene3DRef             │
│  └── internal/interpreter/nconsole/                              │
│       ├── ThreeJsTerminal      (533 lines)                       │
│       ├── CameraAnimator       (461 lines)                       │
│       ├── WebGLEffectRenderer  (327 lines)                       │
│       ├── SpatialState         (197 lines — NEW extraction)      │
│       ├── ThreeJsFacade        (137 lines)                       │
│       ├── WebGLInputHandler    (114 lines)                       │
│       ├── WebGLCanvasRenderer  (102 lines)                       │
│       ├── SlideLayer           (85 lines)                        │
│       └── WebGLScene           (75 lines)                        │
└─────────────────────────────────────────────────────────────────┘
```

---

## Scoring

| Dimension | Score | Change | Notes |
|-----------|-------|--------|-------|
| **Modularity** | ★★★★☆ | — | Clean layering maintained; `api/support/` is a well-defined new module |
| **Extensibility (user-side)** | ★★★★★ | ↑ | `TickedSlide`/`TickedTransition` dramatically lower the barrier to creating custom animations |
| **Extensibility (library-side)** | ★★★★☆ | — | Feature registry works; new builders follow clear patterns |
| **Testability** | ★★★★☆ | ↑ | `forTest()` factories + `TestFrameworkShowcaseSpec` make testing ergonomic; JS gap remains |
| **Readability** | ★★★½☆ | ↑ | New support APIs are well-documented; JS code still opaque |
| **Onboarding (new contributor)** | ★★★½☆ | ↑ | `TestFrameworkShowcaseSpec` is an excellent onboarding document; dual-platform still adds complexity |

---

## Feature Introduction: Effort Analysis (Updated)

### Adding a new custom animated transition (user-side)

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Create transition with `TickedTransition.contextual[F]()` | Single new file | Trivial — ~30 lines |
| 2. Use `.buildProgress` with render function | Same file | Pattern is copy-paste from examples |
| 3. *(Optional)* Add `.withEasing()` | Same file | One method call |
| 4. *(Optional)* Add `.withSkipOnInput` | Same file | One method call |
| **Total** | **1 file** | **~30–50 lines** |

**Improvement from third review:** Previously required manually implementing `Transition[F]`, managing `Ticker` subscription, handling `Deferred` for completion, and coordinating `FixedStep`. Now a single builder call handles all lifecycle.

### Adding a new custom animated slide (user-side)

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Create slide with `TickedSlide.contextual[F]()` | Single new file | Trivial — ~40 lines |
| 2. Use `.buildWithGlide` for smooth motion | Same file | Pattern from `SimpleGlideSlide` |
| **Total** | **1 file** | **~40–60 lines** |

### Adding a new easing function (library-side)

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Add function to `Easing` object | `Easing.scala` | ~5 lines |
| **Total** | **1 file** | **Trivial** |

### Testing a transition/slide with the harness

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Create spec extending `CatsEffectSuite` | New test file | ~20 lines |
| 2. Use `SlideTestHarness.default()` + `TickedTransition.forTest(harness)` | Same file | 3 lines setup |
| 3. Call `harness.runWithTicking(transition)` | Same file | Pattern from showcase |
| **Total** | **1 file** | **~30 lines** |

---

## Maintainability Observations

### 1. ✅ TickedTransition / TickedSlide — Excellent Boilerplate Reduction

The new builder APIs represent the most impactful usability improvement since the project's inception. Before:

```scala
// Old pattern: ~80 lines for a simple wipe transition
class WipeTransition[F[_]: Temporal: NConsole] extends Transition[F] {
  def run(from: Screen, to: Screen, ticker: Ticker[F]): F[Unit] = {
    Deferred[F, Unit].flatMap { done =>
      val step = FixedStep(40.millis)
      ticker.subscribe { elapsed =>
        step.advance(elapsed) { progress =>
          // render logic
          if (progress >= 1.0) done.complete(()).void
          else Applicative[F].unit
        }
      }.flatMap(_ => done.get)
    }
  }
}
```

After:

```scala
// New pattern: ~30 lines
TickedTransition.contextual[F]()
  .buildProgress { case (from, to, progress, console) =>
    // render logic — just return ProgressResult.continue or .done
  }
```

**Lifecycle management**, **timing coordination**, **easing application**, and **early exit** are all handled internally. This is a textbook example of API design that exposes the _what_ (render at progress X) while hiding the _how_ (ticker subscription, deferred signalling, fixed-step accumulation).

### 2. ✅ GlideLayer — Smart Platform Abstraction

`GlideLayer` provides smooth sub-cell character interpolation on WebGL backends while gracefully degrading to grid-snapped rendering on terminals. Key design decisions:

- **Key-based matching** (`SmoothChar.key`) enables correct interpolation across frame boundaries even when characters are reordered
- **Fallback to index-based matching** when keys aren't provided
- **Wrap-around detection** prevents interpolation glitches when objects wrap screen edges
- **`renderOnto` / `renderOntoScrolled`** keep the rendering surface API generic

**One concern:** The `GlideLayer` at 327 lines contains both the interpolation logic _and_ the rendering dispatch. The interpolation math (lerp between positions, wrap detection, fractional progress calculation) could be extracted into a pure, testable utility. Currently, testing `GlideLayer` requires a full `NConsole` mock.

### 3. ✅ Contextual[F, A] — Clean Dependency Injection

The `Contextual` pattern elegantly solves the "builder needs runtime dependencies" problem without resorting to implicits or Reader monad:

```scala
TickedSlide.contextual[F]()  // returns Contextual[F, TickedSlideBuilder[F]]
// Framework unwraps it at construction time, injecting NConsole + Ticker + AnimationSettings
```

This keeps the user API clean while allowing the framework to provide console, ticker, and settings at the point where the slide is actually instantiated.

### 4. ✅ TestFrameworkShowcaseSpec — Living Documentation

At 631 lines, `TestFrameworkShowcaseSpec.scala` serves as both a test suite and an onboarding document. It covers:
- `TestConsole` (buffered output, input injection, resize simulation)
- `SimulatedClock` (deterministic time advancement)
- `FixedStep` (discrete step timing)
- `TestTicker` (manual tick triggering)
- `SlideTestHarness` (full-pipeline testing)
- Transition testing with `TickedTransition.forTest()`
- Overlay testing
- Interactive slide testing

**This is the single most valuable onboarding resource in the project.** New contributors should read this before any other file.

### 5. ⚠️ TickedTransition at 562 Lines — Growing Complexity

While the API surface is clean, the implementation has grown complex:

| Variant | Builder Method | Internal Complexity |
|---------|---------------|---------------------|
| `buildProgress` | Progress-based rendering | FixedStep + Easing + ProgressResult + skip-on-input |
| `buildStepped` | Step-by-step rendering | FixedStep + step counting + completion signal |
| `buildProgressWithGlide` | Progress + GlideLayer | All of above + GlideLayer lifecycle |
| `buildWithSetup` | Custom initialization | Setup function + render loop + cleanup |

The 4 variants share significant internal machinery (ticker subscription, deferred completion, fixed-step management) but each has slightly different wiring. The code uses nested closures and `flatMap` chains that make control flow hard to follow.

**Risk:** Medium — a bug in the shared completion logic (e.g., the `skipOnInput` race) would affect all 4 variants but be hard to trace through the nested closures.

**Suggestion:** Consider extracting the common "subscribe + advance + signal completion" loop into a private helper, with the 4 variants providing only their specific rendering callback and termination condition.

### 6. ⚠️ forTest() Integration — Implicit Threading

`TickedTransition.forTest(harness)` wires the builder to the harness's `TestConsole`, `TestTicker`, and `SimulatedClock`. However, the `AnimationClock[F]` is passed as an **implicit** derived from the harness:

```scala
def forTest(harness: SlideTestHarness[F])(implicit clock: AnimationClock[F]): TickedTransitionBuilder[F]
```

This means test code requires an implicit `AnimationClock[F]` in scope, which is provided by `import harness.implicits._` or similar. If the user forgets this import, the error message is cryptic.

**Risk:** Low — tests follow a clear pattern from the showcase, but implicit resolution failures produce unhelpful compiler errors.

**Suggestion:** Consider making `forTest()` accept the clock explicitly, or have the harness provide a method that returns a fully-wired builder without requiring implicit imports.

### 7. ⚠️ SpatialState Extraction — Partial, Not Complete

The `SpatialState.scala` (197 lines) extraction from `ThreeJsTerminal` addresses the mutable state concern from the third review, but:

- `ThreeJsTerminal` is still 533 lines (only 5 lines shorter than before)
- The terminal still contains the rendering pipeline inline
- `SpatialState` manages layer creation but the _usage_ of layers (rendering, camera coordination) remains in `ThreeJsTerminal`

**Risk:** Low — the extraction makes the state model explicit and testable in isolation, which is progress. But `ThreeJsTerminal` remains a complexity hotspot.

### 8. ⚠️ GlideLayer — No Direct Unit Tests

Despite being 327 lines of interpolation logic, `GlideLayer` has no dedicated unit tests. It's tested _indirectly_ through example specs like `SimpleGlideSlide`, but:

- The wrap-around detection logic is untested in isolation
- The key-based vs index-based matching fallback is untested
- Edge cases (empty frames, single-character frames, all characters wrapping) aren't covered

**Risk:** Medium — interpolation bugs produce subtle visual glitches that are hard to reproduce in integration tests.

**Suggestion:** Extract the pure interpolation logic (`lerpPosition`, `shouldWrap`, `matchByKey`) into a testable object and add property-based tests with ScalaCheck.

### 9. ⚠️ Easing Functions — No Tests

The 14+ easing functions in `Easing.scala` (133 lines) are pure mathematical functions with well-known expected behaviors:
- `easeInQuad(0.0)` should equal `0.0`
- `easeInQuad(1.0)` should equal `1.0`
- `easeInOutCubic(0.5)` should equal `0.5` (symmetry point)
- All functions should be monotonically non-decreasing (except elastic/bounce)
- Output should be bounded for input in `[0, 1]`

These are trivial to test with property-based testing and would catch typos in the math.

**Risk:** Low — the functions are simple, but a copy-paste error in one would produce subtle animation artifacts that are hard to diagnose.

**Suggestion:** Add a single property-based test that verifies boundary conditions and monotonicity for all easing functions.

### 10. ⚠️ FixedStep Accumulated Remainder — Subtle Semantics

`FixedStep` accumulates elapsed time and fires the callback once per step, carrying over the remainder. The `progress` value exposed to users is clamped to `[0.0, 1.0]` via the step fraction. However:

- If `elapsed` is much larger than `stepDuration` (e.g., tab was backgrounded), multiple steps fire in rapid succession
- The `TickedTransition` progress calculation depends on correct step counting
- There's no maximum-steps-per-tick guard

**Risk:** Low in practice (ticker fires at ~60fps), but if a browser tab is backgrounded and resumed, a burst of accumulated time could cause many rapid state transitions.

**Suggestion:** Add a configurable `maxStepsPerTick` parameter (default: 3–5) to prevent burst-firing after tab resume.

### 11. ⚠️ SharedAdvancedPresentation — 655 Lines, Growing

`SharedAdvancedPresentation.scala` at 655 lines is a single file defining an entire multi-slide presentation. While it's example code, it:
- Demonstrates every API feature in one place (good for discoverability)
- Is hard to navigate due to its length
- Mixes slide definitions, transition configurations, and layout logic

**Risk:** Low — it's example code. But new users looking at this as a template may produce similarly monolithic presentation files.

**Suggestion:** Consider splitting into a `slides/` package with one file per conceptual group, with the main presentation file just composing them.

### 12. Previous Concerns Still Open

The following concerns from the third review remain completely unaddressed:

| Concern | Severity | Why It Matters |
|---------|----------|----------------|
| Zero JS-only tests | High | 2,031 LOC (now including SpatialState) with no test coverage |
| `js.Dynamic` heavy usage | Medium-High | Runtime casts across all WebGL code |
| Particle spawning duplication | Low | Bug fix must be applied twice |
| Three.js version not pinned | Medium | CDN-loaded without integrity check |
| `null` in executor factory | Low | Un-idiomatic Scala |
| Resize handling | Low | Documented limitation |

---

## What's Easy to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| New animated transition (user-side) | **Trivial** | `TickedTransition` builder — ~30 lines, 1 file |
| New animated slide (user-side) | **Trivial** | `TickedSlide` builder — ~40 lines, 1 file |
| New easing function | **Trivial** | Pure function in `Easing.scala` |
| Testing any transition/slide | **Trivial** | `forTest()` + `runWithTicking()` |
| Tuning animation timing | **Trivial** | `AnimationSettings` case class |
| New custom overlay | Low | SPI unchanged |
| New rendering backend | Medium | `Terminal[F]` SPI boundary |
| Adding GlideLayer to existing transition | Low | Swap `buildProgress` → `buildProgressWithGlide` |

## What's Hard to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| Changing TickedTransition internals | Medium-High | 562 lines of nested closures; affects all 4 variants |
| Changing GlideLayer interpolation | Medium | No isolated tests; requires visual verification |
| Refactoring ThreeJsTerminal further | Medium-High | Still 533 lines; no JS tests |
| Changing the slide lifecycle (SPI) | High | Touches executor, SPI, all implementations, all builders |
| Adding JS-only tests retroactively | Medium | Must extract pure logic from DOM-dependent code first |
| Changing FixedStep semantics | Medium | All builders depend on it; subtle timing implications |
| Scala 3 migration | High | kind-projector removal, Contextual encoding changes |

---

## Dependency Health

| Dependency | Version | Platform | Risk |
|-----------|---------|----------|------|
| cats-core | 2.12.0 | Shared | Low — stable |
| cats-effect | 3.5.7 | Shared | Low — CE3 API frozen |
| cats-effect-testkit | 3.5.7 | Shared (test) | Low |
| jline | 3.27.1 | JVM | Low — mature |
| scalajs-dom | 2.8.0 | JS | Low — stable |
| sbt-scalajs | 1.17.0 | Build | Low — recent |
| sbt-crossproject | 1.3.2 | Build | Low |
| munit-cats-effect | 2.0.0 | Shared (test) | Low |
| munit | 1.0.3 | Shared (test) | Low |
| sbt-scoverage | 2.2.2 | Build | Low |
| sbt-scalafix | 0.13.0 | Build | Low |
| kind-projector | 0.13.3 | Shared (compiler) | Low — but blocks Scala 3 |
| better-monadic-for | 0.3.1 | Shared (compiler) | Low — but blocks Scala 3 |
| **Three.js** | **CDN (global)** | **JS (runtime)** | **⚠️ Medium — not version-pinned** |

No new dependency risks introduced. The build tooling is current.

---

## Test Architecture

**Strengths (improved since third review):**
- ~200+ tests covering shared layers
- `SlideTestHarness` now integrates with `TickedTransition.forTest()` and `TickedSlide.forTest()` for zero-ceremony test setup
- `TestFrameworkShowcaseSpec` (631 lines) is a comprehensive living-documentation test
- 8 example test files demonstrate testing patterns for the new builder APIs
- `SimulatedClock` + `TestTicker` provide deterministic timing
- Command pattern in executor enables isolated unit testing

**Gaps (mostly unchanged):**
- **Zero JS-only tests** — now 2,031 lines (including `SpatialState`) with no coverage
- **No unit tests for `GlideLayer` interpolation logic** — tested only indirectly
- **No unit tests for `Easing` functions** — trivial to add but missing
- **No unit tests for `FixedStep` edge cases** (burst-fire, zero-duration, negative elapsed)
- **No property-based testing** anywhere in the project
- **No stress test** for concurrent TickedTransition completion (skip-on-input race)
- **`Landscape3DSlide`** (673 lines) has no tests

**New testing pattern worth noting:**
The `forTest()` factory pattern is elegant and should be documented as the canonical way to test custom transitions:

```scala
test("wipe transition completes") {
  SlideTestHarness.default[IO](cols = 40, rows = 10).flatMap { harness =>
    val transition = SimpleWipeTransition.make(harness)
    harness.runWithTicking(transition, fromContent, toContent).map { frames =>
      assert(frames.last.contains(toContent))
    }
  }
}
```

---

## Recommendations

### Critical (Safety & Correctness)

1. **Add unit tests for Easing functions** — property-based tests verifying boundary values (f(0)=0, f(1)=1), monotonicity, and symmetry. ~30 minutes of work, high value.

2. **Add unit tests for GlideLayer interpolation** — extract `lerpPosition`, `shouldWrap`, and `matchByKey` as pure functions; test with property-based cases including wrap-around edges.

3. **Add unit tests for FixedStep** — verify accumulator behavior, burst-fire scenarios (large elapsed values), and zero/negative elapsed handling.

4. **Add tests for camera math** (from third review, still unaddressed) — `computeNormal`, `computeUp`, `easeInOut`, `transitionDuration` are pure and testable.

### Quick Wins

5. **Add `maxStepsPerTick` guard to FixedStep** — prevent burst-firing after browser tab resume.

6. **Extract TickedTransition common loop** — the 4 build variants share ~60% of their implementation; a private `subscribeAndAdvance` helper would reduce duplication and make the completion logic auditable.

7. **Pin Three.js version** in example HTML (same recommendation, third time).

8. **Replace `null` with default no-op** (same recommendation, fourth time).

### Medium-Term

9. **Add property-based testing** — introduce ScalaCheck or munit-scalacheck as a test dependency. Easing, GlideLayer, and FixedStep are all ideal candidates.

10. **Extract GlideLayer pure logic** — move interpolation math to a `GlideInterpolation` object that can be tested without `NConsole`.

11. **Continue ThreeJsTerminal decomposition** — the `SpatialState` extraction was a good start; next step is extracting the rendering dispatch (the `write()` method) into a `WebGLRenderer` that receives state rather than owning it.

12. **Add `SpatialState` unit tests** — it's now a standalone class managing layer deduplication and camera coordination. Test the deduplication logic and position-to-layer mapping.

### Long-Term

13. **Scala 3 migration** — same as previous reviews; additionally, the `Contextual[F, A]` encoding would benefit from Scala 3's context functions.

14. **CONTRIBUTING.md** — document the `api/support/` builder patterns and the `forTest()` integration as the recommended approach for extending the library.

15. **Consider splitting TickedTransition** — the 562-line file could be split into `TickedTransitionBuilder` (configuration) and `TickedTransitionRuntime` (execution), making the separation between build-time and run-time concerns explicit.

---

## Developer Experience Assessment

### Creating a New Transition (Before vs. After)

| Metric | Before (manual) | After (TickedTransition) |
|--------|-----------------|--------------------------|
| Lines of code | ~80–120 | ~30–50 |
| Files to create | 1 | 1 |
| Concepts to understand | Ticker, Deferred, FixedStep, NConsole, Screen | Progress (0→1), Screen, render function |
| Testing setup | Manual SimulatedClock + TestTicker wiring | `forTest(harness)` — 1 line |
| Easing support | Manual implementation | `.withEasing(Easing.easeInOutCubic)` |
| Early exit | Manual Deferred + input checking | `ProgressResult.done()` |
| GlideLayer integration | Manual subscription + lifecycle | `.buildProgressWithGlide` |

**Verdict:** The builder layer has transformed the library from "powerful but ceremony-heavy" to "powerful and ergonomic" for the common case. This is the single biggest maintainability improvement since the project's inception, because it means **fewer custom implementations of complex lifecycle patterns** in user code — and therefore fewer opportunities for user-introduced bugs.

### Understanding the Codebase (New Contributor Path)

| Step | Resource | Time |
|------|----------|------|
| 1. Understand the API | `TestFrameworkShowcaseSpec.scala` | 30 min |
| 2. Create a custom transition | Copy `SimpleWipeTransition` | 15 min |
| 3. Test it | Copy `SimpleWipeTransitionSpec` | 10 min |
| 4. Understand internals | `PresentationExecutorInterpreter` + `PlatformStrategy` | 1–2 hours |
| 5. Understand WebGL layer | `ThreeJsTerminal` + `SpatialState` + `CameraAnimator` | 3+ hours |

The **time-to-first-contribution** for shared-layer changes is now excellent (~1 hour). The JS layer remains significantly harder to approach.

---

## Overall Verdict

**The introduction of `TickedTransition`, `TickedSlide`, and `GlideLayer` represents a maturation of the library's public API from "toolkit" to "framework."**

The positive:
- **Builder pattern with `forTest()` integration** eliminates the #1 source of ceremony in both production and test code
- **`Contextual[F, A]`** cleanly solves dependency injection without polluting user-facing APIs
- **`Easing` library** provides a complete set of animation curves with zero external dependencies
- **`GlideLayer`** enables smooth motion on WebGL while gracefully degrading on terminal — true cross-platform abstraction
- **`TestFrameworkShowcaseSpec`** is outstanding living documentation that serves as onboarding, regression test, and API reference
- **`SpatialState` extraction** shows that the ThreeJsTerminal refactoring is progressing (slowly)

The negative:
- **TickedTransition at 562 lines** is growing into a complexity hotspot itself — the internal nesting makes it hard to audit the completion/cancellation logic
- **Zero tests for the new pure-logic APIs** (Easing, GlideLayer interpolation, FixedStep edge cases) — ironic given how testable they are
- **JS-side concerns from third review are all unchanged** — 2,031 lines without tests, heavy `js.Dynamic`, particle duplication
- **The gap between shared-layer quality and JS-layer quality is widening** — shared code is now well-tested and well-documented; JS code remains opaque and untested

**Maintenance confidence:**
- **Shared layer:** High (well-tested, well-documented, clean architecture, excellent builder APIs)
- **JS layer:** Medium-Low (unchanged from third review — no tests, mutable state, `js.Dynamic`)
- **New builder APIs:** Medium-High (clean design, good examples, but lacking unit tests for pure logic components)

**Priority recommendation:** Before adding more features, invest one sprint in adding unit tests for the pure-logic components introduced in this iteration: `Easing`, `GlideLayer` interpolation, `FixedStep`, and `SpatialState` deduplication. These tests are trivial to write (pure functions, no IO) and would significantly increase confidence in the animation infrastructure that all user-facing builders depend on.

