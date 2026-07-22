# Usability Review: Creating Animations, Transitions & Custom Slides (Follow-up)

**Perspective:** A developer using the lote library to build presentations  
**Scope:** Both terminal (JVM/JLine) and WebGL (browser/Three.js) backends  
**Date:** 2025-07-22  
**Previous Review:** [UsabilityReviewAnimations.md](UsabilityReviewAnimations.md) (2025-07-20)

---

## Executive Summary

Since the previous review two days ago, the library has addressed several of the highest-priority recommendations. The addition of `TickedTransition` (acting as the requested "TransitionLoop helper") and `GlideLayer.renderOnto` (providing platform-transparent compositing) represent significant usability improvements. The gap between "use a built-in transition" and "write your own" has narrowed considerably. This follow-up reassesses the animation authoring experience in light of these changes and identifies remaining friction points.

---

## 1. What Changed Since the Previous Review

| Previous Recommendation | Status | Implementation |
|------------------------|--------|----------------|
| Add a `TransitionLoop` helper | ✅ **Addressed** | `TickedTransition` with `buildProgress`, `buildStepped`, `buildProgressWithGlide`, `buildWithSetup` |
| Make `GlideLayer` platform-transparent | ✅ **Addressed** | `renderOnto(frame, chars)` handles WebGL overlay dispatch and terminal grid compositing internally |
| Provide a "minimum viable custom transition" example | ✅ **Addressed** | `SimpleWipeTransition` (~75 lines) and `SimpleSweepTransition` (~83 lines) |
| Add a `TickedSlide` helper for animated slides | ✅ **Addressed** | `TickedSlide` with `build`, `buildStepped`, `buildWithGlide` — handles ticker lifecycle, FixedStep, and GlideLayer cleanup |
| Simpler alternative to `Contextual` | ⚠️ **Partially addressed** | `TickedTransition.contextual[F] { builder => ... }` reduces exposure to raw `Contextual` but the pattern still exists |
| Document coordinate system for `.at(x, y, z)` | ❌ **Not addressed** | Units remain undocumented ("world units" is the only hint) |
| Add layout helpers for spatial mode | ❌ **Not addressed** | Still requires absolute coordinates |
| Architecture diagram | ❌ **Not addressed** | Pipeline flow (Ticker → FixedStep → GlideLayer → NConsole) remains undocumented visually |

---

## 2. TickedTransition — Assessment

### 2.1 What Works Well

| Aspect | Assessment |
|--------|-----------|
| **Boilerplate elimination** | ✅ Excellent. The `buildProgress` path reduces a custom transition from ~150 lines to ~35 lines of actual logic. Ticker subscribe/unsubscribe, FixedStep setup, Deferred completion, and final-frame rendering are all handled. |
| **Progressive complexity** | ✅ Four builder methods (`buildProgress` → `buildStepped` → `buildProgressWithGlide` → `buildWithSetup`) form a clear escalation path. Users start simple and graduate to more control only when needed. |
| **ScalaDoc quality** | ✅ Each method has clear examples in the doc comment. The class-level ScalaDoc shows all three usage patterns (progress, stepped, contextual). |
| **Contextual integration** | ✅ `TickedTransition.contextual[F] { builder => ... }` is a one-liner that hides `SlideContext` wiring entirely. |
| **Sensible defaults** | ✅ `totalSteps = 20` and `AnimationSettings.default` mean a transition works without tuning. |

### 2.2 Remaining Pain Points

#### The `implicit clock: Clock[F]` threading

Every `build*` method requires an `implicit Clock[F]`. Inside a `Contextual` block this comes from `ctx.clockInstance` (via the harness in tests) or is derived from `Temporal[F]` in production. But the implicit resolution path isn't documented:

```scala
// This compiles in production (Clock derived from Temporal[IO]):
TickedTransition.contextual[IO] { builder =>
  builder.buildProgress(20) { (from, to, progress) => ... }
}

// But in tests, users must thread harness.clockInstance explicitly:
implicit val clock = harness.clockInstance
SimpleSweepTransition.create[IO](3, harness.console, harness.ticker, harness.animationSettings)
```

The disconnect between "it just works in production" and "you must wire it manually in tests" is a minor but recurring source of confusion.

**Suggestion:** Document this explicitly in the testing section, or provide a `TickedTransition.forTest(harness)` convenience factory.

#### No cancellation / skip support

`TickedTransition` always runs to completion. There's no built-in way for `userInput` to skip/fast-forward the transition (e.g., pressing Space to jump to the end). The `userInput` method is hardcoded to `Monad[F].unit`. Users who want skip-on-keypress must use `buildWithSetup` and wire their own `Deferred` completion trigger — defeating much of the boilerplate savings.

**Suggestion:** Add an optional `skipOnInput: Boolean = false` parameter to the builder, or expose a `skipSignal: F[Unit]` in `StepContext`.

#### `buildProgress` assumes linear time

The progress value is always linear (0.0 → 1.0). There's no built-in easing. Users who want ease-in-out must apply their own easing function inside `renderFrame`:

```scala
builder.buildProgress(20) { (from, to, linearProgress) =>
  val eased = easeInOutCubic(linearProgress)  // user must provide this
  blendFrame(from, to, eased)
}
```

**Suggestion:** Consider adding `.withEasing(EasingFunction)` to the builder, or providing common easing functions in a companion object (e.g., `Easing.easeInOut`, `Easing.easeOutBounce`).

---

## 3. GlideLayer.renderOnto — Assessment

### 3.1 What Works Well

The `renderOnto` method directly addresses the biggest pain point from the previous review:

```scala
// Before (platform branching required):
if (supportsFloatingChars) {
  console.writeString(backgroundOnly) *> overlay.render(smoothChars)
} else {
  console.writeString(everythingOnGrid)
}

// After (platform-transparent):
overlay.renderOnto(frame, edgeChars).flatMap { composited =>
  console.clear() *> console.writeString(composited)
}
```

| Aspect | Assessment |
|--------|-----------|
| **API simplicity** | ✅ Single method call replaces the branching pattern |
| **Correct terminal fallback** | ✅ Characters are composited at integer positions onto the frame — no lost characters |
| **WebGL path preserved** | ✅ Dispatches to floating overlay with sub-pixel interpolation, returns frame unchanged |
| **`renderOntoScrolled` variant** | ✅ Handles the more complex case of sub-pixel scrolling + fixed rows in one call |

### 3.2 Remaining Concerns

#### Return type inconsistency

`renderOnto` returns `F[ScreenAdjusted]` (the composited frame), while `renderOntoScrolled` returns `F[Unit]` (writes directly to console). This asymmetry means:

- With `renderOnto`: caller must still call `console.writeString(composited)` after
- With `renderOntoScrolled`: it handles the write internally

The `SweepRightTransition` example shows the `renderOnto` pattern clearly, but a user encountering `renderOntoScrolled` first might be confused by the different contract.

**Suggestion:** Either document this clearly, or consider making `renderOntoScrolled` also return `F[ScreenAdjusted]` for consistency (letting the caller write).

#### `wrapThreshold` remains non-obvious

The previous review flagged this and it's still true: the relationship between `wrapThreshold` and `columnsPerStep` in `SweepRightTransition` (line 44: `.withGlideLayer(wrapThreshold = columnsPerStep)`) is only understandable after reading the GlideLayer internals. There's no guidance on "what value should I pick?"

**Suggestion:** Add a brief explanation to the `GlideLayer.make` ScalaDoc: "Set wrapThreshold to the maximum distance (in cells) a character might move in a single step. Characters that jump farther than this will snap rather than interpolate."

---

## 4. Minimum Viable Examples — Assessment

### 4.1 SimpleSweepTransition

**Strengths:**
- ✅ 83 lines total, with only ~15 lines of actual rendering logic in `blendFrame`
- ✅ Clear factory methods: `contextual[F]()` for builder integration, `create[F](...)` for direct/test use
- ✅ No GlideLayer, no platform branching — pure progress → frame mapping
- ✅ Well-commented with "compare this with the original" guidance

**Weaknesses:**
- ❌ Uses a hardcoded `estimatedWidth = 80` rather than reading actual screen dimensions. This means the animation timing doesn't adapt to narrow or wide terminals. The `buildProgress` path doesn't expose screen dimensions — you'd need `buildStepped` for that.
- ❌ Not linked from the README's "Custom Transition" section. Users must discover it by browsing `shared-examples/`.

### 4.2 SimpleWipeTransition

**Strengths:**
- ✅ Clear ScalaDoc with usage examples showing both builder integration and custom speed
- ✅ Three-phase explanation: "how it works" numbered list is excellent for teaching
- ✅ Demonstrates `require(rowsPerStep > 0, ...)` — input validation pattern
- ✅ Pure `renderFrame` method is easily unit-testable in isolation

**Weaknesses:**
- ❌ Same `estimatedHeight = 40` hardcoding issue
- ❌ No test file demonstrating how to test it with `SlideTestHarness`

### 4.3 Gap: No "Absolute Minimum" Example

Both examples are well-structured production code (~80–110 lines with ScalaDoc, factories, etc.). There's still room for a "bare minimum transition in 15 lines" snippet — perhaps in the README itself — showing just the essential pattern without factories or multiple overloads:

```scala
// Hypothetical absolute-minimum transition (README snippet):
TickedTransition(console, ticker, settings)
  .buildProgress(20) { (from, to, progress) =>
    val cols = (progress * 80).toInt
    ScreenAdjusted(
      (0 until 24).map { row =>
        to.content.split("\n").lift(row).getOrElse("").take(cols).padTo(80, ' ')
      }.mkString("\n")
    )
  }
```

---

## 5. Custom Slides — Reassessment

### 5.1 TickedSlide Helper — What Works Well

The library provides a `TickedSlide` helper that mirrors `TickedTransition`'s approach for the `Slide[F]` SPI. It offers three build variants of increasing power:

| Variant | Use case |
|---------|----------|
| `build(onTick, onInput, onStart)` | Simple tick callback, no FixedStep or GlideLayer |
| `buildStepped(onTick, onInput, onStart)` | FixedStep-driven with `(steps, progress)` |
| `buildWithGlide(onTick, onInput, onStart)` | FixedStep + GlideLayer, auto-cleared on `stopShow` |

**Strengths:**
- ✅ Handles ticker subscribe/unsubscribe and `TickerSubscription` ref management automatically
- ✅ Handles FixedStep creation, reset on `startShow`, and step consumption
- ✅ Handles GlideLayer creation and automatic `clear()` on `stopShow`
- ✅ Clean separation of concerns: user provides `onTick`, `onInput`, `onStart`; framework handles lifecycle
- ✅ Used consistently across examples: `SimpleCounterSlide`, `SimpleGlideSlide`, `SimpleScrollSlide`, `SimpleStaticMarkerScrollSlide`

**Example (`SimpleScrollSlide`):**
```scala
TickedSlide[F](console, ticker, animationSettings)
  .withGlideLayer(wrapThreshold = 20)
  .buildWithGlide(
    onTick = { (nrOfSteps, glide) =>
      // advance state, render background, call glide.renderOntoScrolled(...)
    },
    onInput = { case Character('a') => stateRef.update(_.copy(speed = -0.5)) ... },
    onStart = resetState
  )
```

This is ergonomic and parallel in structure to `TickedTransition`.

### 5.2 TickedSlide — Remaining Pain Points

#### No `contextual` factory on TickedSlide itself

Unlike `TickedTransition.contextual[F] { builder => ... }`, there's no `TickedSlide.contextual` wrapper. Users must manually write the `ContextualF` boilerplate:

```scala
def contextual[F[_]: Temporal: Ref.Make](): ContextualF[F, Slide[F]] =
  ContextualF { ctx =>
    create[F](ctx.console, ctx.ticker, ctx.animationSettings)
  }
```

**Suggestion:** Add `TickedSlide.contextual[F] { builder => F[Slide[F]] }` for symmetry with `TickedTransition.contextual`.

#### `buildWithGlide` discards fractional progress

The `buildWithGlide` callback receives `(steps: Int, glide)` but **not** the fractional progress toward the next step. Compare with `buildStepped` which gives `(steps, progress)`. This means slides using `buildWithGlide` can't do sub-step interpolation on their own state — they must rely entirely on GlideLayer for smoothness.

This works for `SimpleScrollSlide` (which uses wall-clock time for its orbit), but would be limiting for a slide that wants both FixedStep-driven interpolation *and* GlideLayer.

**Suggestion:** Change `buildWithGlide` signature to `(Int, Double, GlideLayer.Overlay[F]) => F[Unit]` to include fractional progress.

#### State management is still external

`TickedSlide` handles lifecycle boilerplate but doesn't help with state. Every slide example still creates its own `Ref[F].of(initialState)` outside the builder and threads it through closures. This is fine for Scala developers comfortable with `Ref`, but contrasts with `TickedTransition.buildProgress` where internal state (step counter) is fully managed.

### 5.3 The `content` Pull Model

The `Slide[F]` trait's `content: F[Option[ScreenAdjusted]]` is pulled by the framework, but animated slides push frames via `console.writeString`. This dual-path isn't immediately obvious:

- `content` is called once when the slide becomes active (to get initial content for layout/alignment)
- Subsequent frames are pushed directly to the console
- `TickedSlide` always returns `None` from `content`, delegating all rendering to the tick callback

The `Option[ScreenAdjusted]` return type (returning `None` means "I handle my own rendering") isn't prominently documented.

---

## 6. Testing — Still Excellent, Minor Gaps

### 6.1 What's Improved

The `SlideTestHarness` documentation in the README is comprehensive and example-driven. The progressive examples (manual `tick()` → `runWithTicking` → full session testing with `runWith`) form a clear learning path.

### 6.2 Remaining Gaps

#### No test example for TickedTransition-based transitions

The README shows testing with raw `MyTransition.create[IO](...)`, but doesn't show the pattern for testing a `TickedTransition.contextual`-based transition. Users must figure out that they need to resolve the `Contextual` wrapper manually:

```scala
// Not shown anywhere — users must discover this pattern:
val transition = SimpleSweepTransition.create[IO](
  columnsPerStep = 3,
  harness.console,
  harness.ticker,
  harness.animationSettings
)(harness.clockInstance)
```

**Suggestion:** Add a test example for `SimpleSweepTransition` or `SimpleWipeTransition` in the examples test directory, demonstrating the harness + contextual resolution pattern.

#### `runWithTicking` tick count

The default tick count in `runWithTicking` is 10, but there's no guidance on "how many ticks does my transition need?" For a transition with `totalSteps = 20` and default `animationStep = 16ms`, `tickStep = 5ms`, users must calculate: 20 steps × (16ms / 5ms) = ~64 ticks minimum. This arithmetic is non-obvious.

**Suggestion:** Either document the formula, or provide `runUntilComplete(task, maxTicks = 1000)` that ticks until the task completes (with a safety bound).

---

## 7. Documentation — Assessment

### 7.1 README Quality

The README is excellent — well-structured, progressive, humorous without sacrificing clarity. Key improvements since the previous review:

- ✅ The "Rendering Architecture" section explaining why not everything is a `SmoothChar` is particularly valuable
- ✅ The `renderOnto` pattern is documented inline with a clear code example
- ✅ Testing section is comprehensive with copy-paste-ready examples
- ✅ Animation Tuning Presets table is practical and memorable

### 7.2 Remaining Documentation Gaps

| Gap | Impact | Effort |
|-----|--------|--------|
| **No cookbook for common patterns** | Medium — "how do I fade text in word-by-word?" still requires assembling FixedStep + Ticker + ScreenAdjusted manipulation knowledge | Medium |
| **Coordinate system undocumented** | Medium — `.at(3600, 2400, 500)` is meaningless without knowing units | Low |
| **No architecture diagram** | Low — the README's text explanations are good enough for most users, but a visual would accelerate onboarding | Low |
| **`TickedTransition` not prominently linked from SPI section** | Medium — users reading the `Transition[F]` trait docs might not discover the helper exists | Low |
| **Minimum-viable snippet not in README** | Low — the examples exist in `shared-examples/` but aren't surfaced in the main "Custom Transitions" README section | Low |

---

## 8. Platform Branching — Largely Resolved

The previous review's central complaint — that every transition must branch on `PlatformCapability.Effects` — is now largely resolved by `GlideLayer.renderOnto`. The `SweepRightTransition` example demonstrates the unified pattern:

```scala
rendered <- gridLayer.renderOnto(frame, sweepEdgeChars(...))
_ <- console.clear()
_ <- console.writeString(rendered)
```

No `if (supportsFloatingChars)` branch anywhere. This is a significant improvement.

**One remaining edge case:** Transitions that want fundamentally different visual strategies per platform (e.g., GPU particle effects on WebGL, character-based animation on terminal) still need to branch via `Contextual { ctx => if (ctx.supports(...)) ... else ... }`. But this is now the exception rather than the rule, and the README documents the pattern clearly.

---

## 9. WebGL-Specific Concerns — Unchanged

The WebGL-specific issues from the previous review remain:

| Issue | Status |
|-------|--------|
| No spatial preview on terminal | ❌ Unchanged |
| Units are magic numbers | ❌ Unchanged |
| No relative positioning / layout helpers | ❌ Unchanged |
| RenderEffect parameters undocumented visually | ❌ Unchanged |
| Cell dimensions affect layout non-obviously | ❌ Unchanged |

These are lower priority given that most users will target terminal first, but they become significant pain points for anyone building a spatial WebGL presentation.

---

## 10. New Observations

### 10.1 The `Clock[F]` Import Confusion

The library defines its own `com.github.morotsman.lote.api.support.Clock[F]` type (distinct from `cats.effect.Clock`). This is a potential source of confusion:

```scala
import cats.effect.Temporal  // provides cats.effect.Clock
import com.github.morotsman.lote.api.support.Clock  // the library's Clock
```

Users might accidentally import the wrong `Clock` and get cryptic implicit resolution errors. The library's `Clock` is presumably a narrower interface suited to `FixedStep`'s needs, but the naming collision with a core Cats Effect type is unfortunate.

**Suggestion:** Consider renaming to `AnimationClock` or `StepClock`, or at minimum document the distinction prominently.

### 10.2 `buildProgress` Doesn't Expose Screen Dimensions

The simplest helper (`buildProgress`) gives `(from, to, progress) => ScreenAdjusted`, but many transitions need screen dimensions to render correctly. Both `SimpleSweepTransition` and `SimpleWipeTransition` work around this by hardcoding estimated dimensions (`estimatedWidth = 80`, `estimatedHeight = 40`).

This means the "simplest" path produces transitions that don't adapt to actual terminal size. To get real dimensions, users must upgrade to `buildStepped` (which provides `StepContext.screenWidth/screenHeight`), adding complexity.

**Suggestion:** Change `buildProgress` signature to include screen dimensions:
```scala
def buildProgress(totalSteps: Int = 20)(
    renderFrame: (ScreenAdjusted, ScreenAdjusted, Double, Int, Int) => ScreenAdjusted
    //                                            progress  width  height
)
```

Or provide them on a context object to avoid positional parameter explosion.

### 10.3 `Ref.Make[F]` Context Bound

Every `TickedTransition` method requires `Ref.Make[F]` as a context bound. This is always satisfied for `IO` but:
- It clutters factory method signatures
- Users unfamiliar with Cats Effect internals may not understand why it's needed
- It's arguably an implementation detail (TickedTransition needs Refs internally)

This is minor but adds to the "implicit parameter noise" that FP-heavy libraries accumulate.

---

## 11. Recommendations Summary

| Priority | Recommendation | Effort |
|----------|---------------|--------|
| **High** | Add `buildProgress` overload that provides screen dimensions (width, height) — eliminates the hardcoded-estimate anti-pattern in both minimum-viable examples | Low |
| **High** | Add skip/cancel support to `TickedTransition` — at minimum a `skipOnInput` flag | Medium |
| **High** | Link `SimpleSweepTransition` / `SimpleWipeTransition` from the README's "Custom Transitions" section | Low |
| **Medium** | Add `TickedSlide.contextual[F]` factory for symmetry with `TickedTransition.contextual` | Low |
| **Medium** | Document the `Clock[F]` import distinction (library Clock vs cats.effect.Clock) | Low |
| **Medium** | Add easing function support to `TickedTransition.Builder` | Low |
| **Medium** | Document `wrapThreshold` semantics on `GlideLayer.make` ScalaDoc | Low |
| **Medium** | Document `.at(x, y, z)` coordinate units | Low |
| **Medium** | Add a test example for `TickedTransition.contextual`-based transitions | Low |
| **Low** | Consider renaming library `Clock[F]` to avoid collision with `cats.effect.Clock` | Medium (breaking) |
| **Low** | Add `renderOntoScrolled` return-type documentation explaining the write-internally contract | Low |
| **Low** | Provide spatial layout helpers (grid, circle) for WebGL mode | High |
| **Low** | Add `runUntilComplete` to `SlideTestHarness` that auto-calculates required tick count | Medium |

---

## 12. Comparison with Previous Review Ratings

| Dimension | Previous (Terminal) | Current (Terminal) | Previous (WebGL) | Current (WebGL) | Change |
|-----------|--------------------|--------------------|------------------|-----------------|--------|
| **Using built-in components** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | — |
| **Writing custom transitions** | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐½ | ⭐⭐⭐⭐ | ↑ Significant improvement |
| **Writing custom slides** | ⭐⭐⭐½ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐½ | ↑ TickedSlide helper |
| **Animation tuning** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | — |
| **Testing** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | — |
| **Documentation** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐½ | ⭐⭐⭐½ | ⭐⭐⭐½ | ↑ Terminal docs improved |

---

## 13. Overall Verdict

The library has made strong progress on the "complexity cliff" problem identified in the previous review. The introduction of `TickedTransition` and `GlideLayer.renderOnto` together eliminate the two largest sources of boilerplate and cognitive load when writing custom transitions. A developer can now go from zero to a working custom transition in ~35 lines of logic, compared to ~150 lines previously.

The remaining gaps are:
1. **Screen dimensions** aren't available in the simplest transition path (`buildProgress`)
2. **Skip/cancel** isn't built into either helper
3. **`TickedSlide` lacks a `contextual` factory** and discards fractional progress in `buildWithGlide`
4. **WebGL spatial features** remain underdocumented

The library is now at a point where writing a basic custom transition or animated slide is approachable for someone familiar with Scala and basic FP concepts. Both the `TickedTransition` and `TickedSlide` helpers successfully eliminate the lifecycle boilerplate that previously made custom components intimidating. The jump from "consumer" to "creator" has gone from a cliff to a manageable step.

