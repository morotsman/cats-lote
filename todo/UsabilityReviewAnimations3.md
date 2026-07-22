# Usability Review: Creating Animations, Transitions & Custom Slides (Third Review)

**Perspective:** A developer using the lote library to build presentations  
**Scope:** Both terminal (JVM/JLine) and WebGL (browser/Three.js) backends  
**Date:** 2026-07-22  
**Previous Review:** [UsabilityReviewAnimations2.md](UsabilityReviewAnimations2.md) (2025-07-22)

---

## Executive Summary

One year after the previous review, the library has addressed nearly every high-priority and medium-priority recommendation. The animation authoring experience is now mature and cohesive. The additions of easing functions, skip-on-input support, `ProgressContext` with screen dimensions, `TickedSlide.contextual`, `AnimationClock` (renamed from `Clock`), coordinate system documentation, and duration-based `buildProgress` collectively eliminate the remaining friction points identified in the previous review. The library now offers a genuinely progressive complexity ladder from "10-line custom transition" to "full spatial WebGL animation" without any cliffs along the way.

---

## 1. What Changed Since the Previous Review

| Previous Recommendation | Status | Implementation |
|------------------------|--------|----------------|
| Add `buildProgress` overload with screen dimensions | ✅ **Addressed** | `ProgressContext(progress, screenWidth, screenHeight)` is the callback parameter |
| Add skip/cancel support to `TickedTransition` | ✅ **Addressed** | `.withSkipOnInput` on `Builder`; any user input fast-forwards to end |
| Link examples from README's "Custom Transitions" section | ✅ **Addressed** | README line 315 links `SimpleSweepTransition`, `SimpleWipeTransition`, and `SweepRightTransition` |
| Add `TickedSlide.contextual[F]` factory | ✅ **Addressed** | `TickedSlide.contextual[F] { builder => F[Slide[F]] }` with ScalaDoc examples |
| Document `Clock[F]` import distinction | ✅ **Addressed** | Renamed to `AnimationClock[F]` with explicit `@implicitNotFound` annotation |
| Add easing function support | ✅ **Addressed** | `Easing` object with 12 functions; `.withEasing(Easing.easeInOutCubic)` on builder |
| Document `wrapThreshold` semantics | ✅ **Addressed** | README documents: "Set it to the maximum distance a character might legitimately move in a single simulation step" |
| Document `.at(x, y, z)` coordinate units | ✅ **Addressed** | `SlidePosition` ScalaDoc with full coordinate system; README `SlidePosition` type row explains CSS-pixel-scale units |
| Add test example for `TickedTransition.contextual` transitions | ✅ **Addressed** | README section "Testing `TickedTransition`-Based Transitions" with complete example; `SimpleSweepTransitionSpec` and `SimpleWipeTransitionSpec` in repo |
| Rename library `Clock[F]` to avoid collision | ✅ **Addressed** | Now `AnimationClock[F]` |
| Add `renderOntoScrolled` return-type documentation | ⚠️ **Partially addressed** | `wrapThreshold` is documented; return-type asymmetry (`F[Unit]` vs `F[ScreenAdjusted]`) not explicitly called out |
| Provide spatial layout helpers | ❌ **Not addressed** | Still requires absolute coordinates |
| Add `runUntilComplete` to `SlideTestHarness` | ⚠️ **Partially addressed** | Not added, but README now documents the formula: "you need at least `duration / tickStep` ticks" with worked examples |
| `buildWithGlide` should include fractional progress | ❌ **Not addressed** | `TickedSlide.buildWithGlide` callback is still `(Int, GlideLayer.Overlay[F]) => F[Unit]` — fractional progress discarded |

**Summary: 10 of 13 recommendations fully addressed, 2 partially addressed, 1 not addressed.**

---

## 2. Easing Functions — Assessment

### 2.1 What Works Well

| Aspect | Assessment |
|--------|-----------|
| **Breadth of functions** | ✅ Excellent. 12 functions across 6 families (linear, quadratic, cubic, sine, exponential, bounce, elastic). Covers all common animation needs. |
| **Integration with builder** | ✅ Seamless. `.withEasing(Easing.easeInOutCubic)` is a one-liner that applies transparently to `buildProgress` and `buildProgressWithGlide`. |
| **Transparency** | ✅ `ProgressContext.progress` is already eased — the user doesn't need to apply it manually. The ScalaDoc states this clearly. |
| **ScalaDoc quality** | ✅ Class-level example shows the full pattern. Each function has a brief description of the curve character. |
| **Type safety** | ✅ `Double => Double` is simple and composable. Users can provide custom easing functions without implementing a trait. |

### 2.2 Remaining Observations

#### No easing preview / visualization

There's no way to preview what an easing curve looks like without running the transition. This is standard for code-only libraries, but a "visual easing reference" in the README (even ASCII-art graphs) would help users choose the right function without trial-and-error.

**Impact:** Low — experienced animators know these curves by name; newcomers can refer to external references like easings.net.

#### `.withEasing` has no effect on `buildStepped` / `buildWithSetup`

The ScalaDoc documents this clearly ("Has no effect on `buildStepped` or `buildWithSetup`"), but users who switch from `buildProgress` to `buildStepped` might forget to apply easing manually to their custom progress calculation. A compile-time warning would be ideal but impractical; the documentation is sufficient.

#### No easing composition

There's no helper for composing easing functions (e.g., applying one curve for the first half and another for the second). This is a niche use case and users can compose functions themselves (`t => if (t < 0.5) f(t*2)/2 else 0.5 + g((t-0.5)*2)/2`), so this is not a real gap.

---

## 3. Skip-on-Input — Assessment

### 3.1 What Works Well

| Aspect | Assessment |
|--------|-----------|
| **Simplicity** | ✅ `.withSkipOnInput` is a single method call — zero configuration |
| **Coverage** | ✅ Works with all four builder methods (`buildProgress`, `buildStepped`, `buildProgressWithGlide`, `buildWithSetup`) |
| **Final-frame guarantee** | ✅ After skipping, the `to` content is always rendered as the final frame |
| **Implementation quality** | ✅ Uses `Deferred.race` with a `skipRef` — clean cancellation semantics with guaranteed cleanup |
| **Thread safety** | ✅ `AtomicReference[Deferred[F, Unit]]` handles concurrent access correctly |

### 3.2 Remaining Observations

#### No selective skip (e.g., only on Space/Enter)

`withSkipOnInput` skips on **any** user input. There's no way to specify "skip only on Space" or "skip only on Enter". Users who want selective skip must use `buildWithSetup` and wire their own completion trigger.

**Impact:** Low — the all-or-nothing approach matches audience expectations (press any key to skip). Selective filtering is an edge case.

#### No "fast-forward" mode (accelerated rather than instant)

The skip jumps instantly to the end. There's no intermediate "play at 4× speed" mode. This would be complex to implement and is not something most presentation libraries offer, so this is more of a "nice to have someday" than a gap.

---

## 4. ProgressContext & Duration-Based API — Assessment

### 4.1 What Works Well

The previous review's highest-priority recommendation was to provide screen dimensions in the simplest transition path. This is now fully implemented:

```scala
builder.buildProgress(500.millis) { (from, to, ctx) =>
  val cols = (ctx.progress * ctx.screenWidth).toInt
  // No hardcoded estimatedWidth = 80!
  ...
}
```

| Aspect | Assessment |
|--------|-----------|
| **Screen dimensions accessible** | ✅ `ctx.screenWidth` and `ctx.screenHeight` eliminate all hardcoded estimates |
| **Duration-based API** | ✅ `buildProgress(500.millis)` is more intuitive than `buildProgress(totalSteps = 20)` — users think in time, not steps |
| **ProgressResult** | ✅ `ProgressResult.done(frame)` / `ProgressResult.continue(frame)` cleanly supports early completion for short content |
| **Eased progress** | ✅ `ctx.progress` is already eased (if easing is set) — no double-application risk |

### 4.2 The Duration vs Steps Inconsistency

`buildProgress` takes a `FiniteDuration`, while `buildProgressWithGlide` still takes `totalSteps: Int = 20`. This inconsistency means:

- `buildProgress(500.millis)` — clear intent, computes steps internally
- `buildProgressWithGlide(totalSteps = 20)` — user must know the relationship between steps and wall-clock time

**Suggestion:** Consider adding a duration-based overload to `buildProgressWithGlide` for consistency. The current step-based API works but breaks the progressive-complexity principle (the "more advanced" method uses a less intuitive parameter).

---

## 5. AnimationClock — Assessment

### 5.1 What Works Well

The rename from `Clock[F]` to `AnimationClock[F]` directly addresses the naming collision identified in the previous review:

| Aspect | Assessment |
|--------|-----------|
| **Name clarity** | ✅ `AnimationClock` is unambiguous — no collision with `cats.effect.Clock` |
| **`@implicitNotFound` annotation** | ✅ Excellent error message: "No implicit AnimationClock[${F}] found. An AnimationClock instance is derived automatically from Temporal[${F}], or you can provide a SimulatedClock in tests via SlideTestHarness." |
| **Auto-derivation** | ✅ Low-priority implicit from `Temporal[F]` means users never need to think about it in production |
| **Test story** | ✅ `harness.clockInstance` provides the simulated version; `TickedTransition.forTest(harness)` eliminates even that wiring |
| **ScalaDoc** | ✅ Clear explanation of when it comes from `Temporal` vs when to use `SimulatedClock` |

### 5.2 No Remaining Concerns

The `AnimationClock` design is clean and the naming confusion is fully resolved. The `forTest(harness)` convenience factory means test authors never interact with `AnimationClock` directly. This is a textbook example of "make the common case easy."

---

## 6. TickedSlide.contextual — Assessment

### 6.1 What Works Well

```scala
TickedSlide.contextual[F] { builder =>
  for {
    stateRef <- Ref[F].of(0)
    slide    <- builder.build(
      onTick  = renderLogic(stateRef),
      onInput = inputHandler(stateRef),
      onStart = stateRef.set(0)
    )
  } yield slide
}
```

| Aspect | Assessment |
|--------|-----------|
| **Symmetry with TickedTransition** | ✅ Same pattern: `Xxx.contextual[F] { builder => ... }` |
| **Effectful construction** | ✅ `F[Slide[F]]` return type allows `Ref` allocation inside the block |
| **ScalaDoc** | ✅ Complete example showing state management pattern |

### 6.2 Remaining: `buildWithGlide` Still Discards Fractional Progress

The previous review noted that `buildWithGlide` receives `(steps: Int, GlideLayer.Overlay[F])` but not the fractional progress. Looking at the implementation:

```scala
// Line 222 of TickedSlide.scala:
case (steps, _) => onTick(steps, glide)  // underscore discards fractional progress
```

This means slides using `buildWithGlide` can't do their own sub-step interpolation alongside GlideLayer. They must rely entirely on GlideLayer for smoothness. For most use cases this is fine (GlideLayer handles interpolation), but it limits advanced patterns where custom state also needs smooth interpolation.

**Impact:** Low — GlideLayer exists precisely to handle the interpolation. Users needing both can use `buildStepped` with manual GlideLayer creation.

---

## 7. Documentation — Assessment

### 7.1 README Quality

The README is now comprehensive, well-organized, and addresses virtually every documentation gap identified in the previous reviews:

| Previously Missing | Current Status |
|-------------------|----------------|
| Coordinate system documentation | ✅ `SlidePosition` type row + ScalaDoc |
| `wrapThreshold` explanation | ✅ Dedicated paragraph with worked example |
| Testing `TickedTransition.contextual` | ✅ Full section with code example |
| Tick count calculation for tests | ✅ Documented with formula and examples |
| Examples linked from Custom Transitions section | ✅ Three examples linked with line-count guidance |
| Rendering architecture explanation | ✅ Detailed section with performance rationale |
| `renderOnto` pattern | ✅ Code example and platform-behavior explanation |

### 7.2 Remaining Documentation Gaps

| Gap | Impact | Effort |
|-----|--------|--------|
| **No cookbook for multi-step patterns** | Low — individual examples are clear enough; combining them is straightforward | Medium |
| **`renderOntoScrolled` vs `renderOnto` contract difference** | Low — advanced users only | Low |
| **No architecture diagram** | Low — text explanations are now comprehensive enough | Low |
| **Easing function visual reference** | Low — users can refer to easings.net | Low |
| **`buildProgressWithGlide` uses steps, not duration** | Low — inconsistency documented by different parameter name | Low |

### 7.3 ScalaDoc Quality

The ScalaDoc across the animation support code is consistently excellent:

- **`TickedTransition`:** 86 lines of class-level ScalaDoc covering all 5 usage patterns (progress, stepped, contextual, easing, skip, testing)
- **`TickedTransition.Builder.buildProgress`:** Full `@param` documentation with worked example
- **`Easing`:** Class-level usage example + per-function descriptions
- **`AnimationClock`:** Clear `@implicitNotFound` with resolution guidance
- **`TickedSlide`:** Three usage examples covering all complexity levels
- **`SlidePosition`:** Complete coordinate system explanation with example layout

---

## 8. Testing — Fully Mature

### 8.1 What's Improved

The testing story is now best-in-class for a Scala FP library:

| Feature | Assessment |
|---------|-----------|
| `TickedTransition.forTest(harness)` | ✅ Eliminates all implicit wiring in tests |
| `SlideTestHarness.fixedSlide` | ✅ Zero-configuration stub slides |
| Tick count guidance in README | ✅ Formula documented with worked example |
| Test examples for TickedTransition | ✅ `SimpleSweepTransitionSpec`, `SimpleWipeTransitionSpec` |
| Complete showcase spec | ✅ `TestFrameworkShowcaseSpec.scala` covers every test utility |

### 8.2 The `runUntilComplete` Gap — Acceptable Tradeoff

The previous review suggested `runUntilComplete(task, maxTicks)` that ticks until completion. This wasn't implemented, but the documentation now makes the manual calculation trivial:

> "For a duration-based transition (e.g., `buildProgress(500.millis)`) with `tickStep = 5.millis`, you need at least `500 / 5 = 100` ticks."

Given the simplicity of the formula and the test examples demonstrating it, this is an acceptable tradeoff. Explicit tick counts make tests more predictable and easier to debug when they fail.

---

## 9. Platform Branching — Fully Resolved for Common Cases

The platform branching story is now complete for the vast majority of use cases:

- **Simple transitions** → `buildProgress` with `ProgressContext` — no platform awareness needed
- **Smooth-edge transitions** → `buildProgressWithGlide` with `GlideLayer.renderOnto` — platform-transparent
- **Advanced transitions** → `buildWithSetup` for full control when needed

The `SimpleEasedWipeTransition` example demonstrates that a polished, eased transition with early completion requires zero platform branching code. This is a significant milestone for the library.

---

## 10. WebGL-Specific Concerns — Partially Addressed

| Issue | Status | Notes |
|-------|--------|-------|
| Coordinate system documented | ✅ Resolved | `SlidePosition` ScalaDoc with units, spacing guidelines, and example layout |
| Units are magic numbers | ✅ Resolved | "CSS-pixel-scale world units" is now explicitly stated |
| No spatial preview on terminal | ❌ Unchanged | WebGL spatial mode has no terminal equivalent |
| No relative positioning / layout helpers | ❌ Unchanged | Still requires absolute coordinates |
| RenderEffect parameters undocumented visually | ❌ Unchanged | Effects are listed in README type table but not visually demonstrated |

The coordinate system documentation is a significant improvement. Users now know that `.at(3600, 2400, 500)` means "two columns right (2×1800), one row down, slightly toward camera" without reading source code.

**Remaining gap:** Spatial layout helpers (grid, circle, spiral arrangements) would make multi-slide spatial presentations easier to author. Currently users must calculate positions manually, which is tedious for presentations with many slides.

---

## 11. New Observations

### 11.1 The `ProgressResult` Pattern is Excellent

The `ProgressResult.done(frame)` / `ProgressResult.continue(frame)` sealed trait is a subtle but significant usability improvement over a bare `ScreenAdjusted` return. It allows transitions to signal early completion without the user needing to understand `Deferred` or track step counts manually:

```scala
val lastDiff = lastDifferingRow(from, to, ctx.screenHeight)
if (clearedRows > lastDiff) ProgressResult.done(frame)
else ProgressResult.continue(frame)
```

This pattern means a wipe transition over 3 lines of content completes in ~150ms even if the configured duration is 5 seconds. The framework handles it — no special logic needed. This is exactly the kind of "smart default" that separates good APIs from great ones.

### 11.2 `SimpleEasedWipeTransition` as a Teaching Example

The `SimpleEasedWipeTransition` is the best "copy-and-modify" starting point in the library:

- 118 lines total, with only ~20 lines of rendering logic
- Demonstrates easing integration (one line: `.withEasing(Easing.easeInOutCubic)`)
- Demonstrates early completion (`ProgressResult.done`)
- Demonstrates screen-dimension-aware rendering (`ctx.screenHeight`)
- Demonstrates content-aware completion (`lastDifferingRow`)

**Suggestion:** Consider highlighting this as the "recommended starting point" for custom transitions in the README, ahead of `SimpleSweepTransition`. The wipe-with-easing pattern teaches more concepts in a more elegant way.

### 11.3 The Progressive Complexity Ladder is Complete

The library now offers a clean escalation path with no gaps:

| Level | Tool | Lines of Logic | What You Get |
|-------|------|---------------|--------------|
| 1 | `buildProgress(duration)` | ~10 | Progress 0→1, screen dimensions, early completion |
| 2 | `buildProgress` + `.withEasing` | ~11 | + smooth curves |
| 3 | `buildProgress` + `.withSkipOnInput` | ~11 | + audience can skip |
| 4 | `buildProgressWithGlide(steps)` | ~20 | + sub-pixel smooth characters |
| 5 | `buildStepped` | ~30 | + full tick-level control |
| 6 | `buildWithSetup` | ~50 | + custom initialization and cleanup |
| 7 | Raw `Transition[F]` | ~150 | Total freedom, total responsibility |

Each level adds exactly one concept and one method call. There are no cliffs between levels 1–4, and the jump from 4 to 5 is well-documented. The "complexity cliff" identified in the original review is now fully resolved.

### 11.4 Minor: `Ref.Make[F]` Still Clutters Signatures

Every `build*` method still requires `Ref.Make[F]` as an implicit:

```scala
def buildProgress(duration: FiniteDuration)(
    renderFrame: ...
)(implicit F: Temporal[F], refMake: Ref.Make[F], clock: AnimationClock[F]): Transition[F]
```

This is always satisfied by `IO` and is purely an implementation detail. For Cats Effect 3 users on `IO`, this is invisible. For users on custom effect types, it's one more context bound to thread. This is a minor ergonomic cost that's inherent to the tagless-final style and not worth changing.

### 11.5 The `forTest(harness)` Pattern Sets a Standard

`TickedTransition.forTest(harness)` is a pattern that more FP Scala libraries should adopt. It completely eliminates the "production code just works, but tests need manual wiring" disconnect that the previous review identified. The test code is now as clean as production code:

```scala
val transition = TickedTransition.forTest(harness)
  .withEasing(Easing.easeInOutCubic)
  .buildProgress(500.millis) { (from, to, ctx) => ... }
```

No implicit threading, no `harness.clockInstance`, no ceremony. This is exemplary.

---

## 12. Recommendations Summary

| Priority | Recommendation | Effort | Notes |
|----------|---------------|--------|-------|
| **Medium** | Add duration-based overload to `buildProgressWithGlide` for consistency with `buildProgress` | Low | Maintains progressive complexity principle |
| **Medium** | Highlight `SimpleEasedWipeTransition` in README as recommended starting point | Low | Better teaching example than `SimpleSweepTransition` |
| **Low** | Document `renderOntoScrolled` return-type contract (`F[Unit]`) vs `renderOnto` (`F[ScreenAdjusted]`) | Low | Avoids confusion for advanced users |
| **Low** | Add fractional progress to `TickedSlide.buildWithGlide` callback signature | Low | `(Int, Double, GlideLayer.Overlay[F]) => F[Unit]` |
| **Low** | Provide spatial layout helpers (grid, circle, spiral) for WebGL presentations | High | Major feature; not a usability bug |
| **Low** | Add ASCII-art easing curve reference to `Easing` ScalaDoc | Low | Nice-to-have for discoverability |

---

## 13. Comparison with Previous Review Ratings

| Dimension | Previous (Terminal) | Current (Terminal) | Previous (WebGL) | Current (WebGL) | Change |
|-----------|--------------------|--------------------|------------------|-----------------|--------|
| **Using built-in components** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | — |
| **Writing custom transitions** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐½ | ↑ Easing, skip, duration API, ProgressResult |
| **Writing custom slides** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐½ | ⭐⭐⭐½ | ⭐⭐⭐⭐ | ↑ `TickedSlide.contextual` |
| **Animation tuning** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐½ | ↑ Easing functions + wrapThreshold docs |
| **Testing** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | — (was already excellent) |
| **Documentation** | ⭐⭐⭐⭐½ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐½ | ⭐⭐⭐⭐ | ↑ Coordinate docs, testing docs, examples linked |

---

## 14. Overall Verdict

The animation authoring experience in lote has matured from "good with known gaps" to "genuinely excellent." The library now provides:

1. **A complete progressive complexity ladder** — from 10-line progress-based transitions to full custom implementations, with no cliffs between levels
2. **Built-in easing** — eliminating the most common "I need to write my own math" complaint
3. **Skip-on-input** — a one-method-call solution for audience-friendly presentations
4. **Screen-dimension-aware rendering** — eliminating hardcoded estimates in the simplest API path
5. **Clean naming** — `AnimationClock` resolves the `Clock` confusion completely
6. **Best-in-class testing** — `forTest(harness)` makes test code as clean as production code
7. **Comprehensive documentation** — coordinate systems, tick count formulas, wrapThreshold semantics, and complete working examples

The remaining gaps (spatial layout helpers, `buildWithGlide` fractional progress, `renderOntoScrolled` documentation) are minor and affect only advanced WebGL use cases. For the primary terminal and basic WebGL paths, the animation API is now production-ready and approachable for any Scala developer familiar with basic FP concepts.

**The library has graduated from "impressive but has friction" to "this is how animation APIs should work in a typed FP context."**

