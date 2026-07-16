# cats-lote — Maintainability Review

_Reviewed: July 2026_

---

## Summary

The project is well-layered with clear separation between public API, internal model, algebras, and interpreters. Adding new **slides** or **overlays** is straightforward because the SPIs are simple and self-contained. However, adding new **cross-cutting features** (transitions, builder options, middleware concerns) touches multiple layers and requires coordinated changes in 3–5 files. The codebase is small enough (~50 source files, ~3500 LOC in `lote/`) that this remains manageable, but awareness of the coupling points is important.

---

## Architecture at a Glance

```
┌─────────────────────────────────────────────────────┐
│  Public API  (api/)                                 │
│  ├── builders/  SessionBuilder, BuilderDsl traits   │
│  ├── spi/       Slide, Transition, Overlay, etc.    │
│  └── data types (Alignment, Screen, UserInput, ...) │
├─────────────────────────────────────────────────────┤
│  Internal Model (internal/model/)                   │
│  └── Presentation, SlideSpecification               │
├─────────────────────────────────────────────────────┤
│  Algebras (internal/algebra/)                       │
│  └── PresentationExecutor, Middleware, IdleDetector │
├─────────────────────────────────────────────────────┤
│  Interpreters (internal/interpreter/)               │
│  ├── nconsole/    JLine terminal implementation     │
│  ├── ticker/      Centralized tick scheduler        │
│  ├── middleware/  Overlay composition engine        │
│  └── transition/  Built-in transition animations    │
├─────────────────────────────────────────────────────┤
│  Internal Builders (internal/builders/)             │
│  └── SlideBuilder, TextSlideBuilder,                │
│      SlideMetadataBuilderOps                        │
└─────────────────────────────────────────────────────┘
```

---

## Scoring

| Dimension | Score | Notes |
|-----------|-------|-------|
| **Modularity** | ★★★★☆ | Clean layering; low coupling between most components |
| **Extensibility (user-side)** | ★★★★★ | SPIs make it trivial to add custom slides, transitions, overlays |
| **Extensibility (library-side)** | ★★★☆☆ | New transitions/builder features require coordinated multi-file changes |
| **Testability** | ★★★★☆ | Good test coverage (200 tests); algebra/interpreter split aids mocking |
| **Readability** | ★★★★☆ | Well-named, small files; some complexity in the executor loop and middleware |
| **Onboarding (new contributor)** | ★★★☆☆ | Requires Cats Effect fluency; indirection can be hard to follow initially |

---

## Feature Introduction: Effort Analysis

### Adding a new built-in transition (e.g. "WipeTransition")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Implement transition logic | `internal/interpreter/transition/WipeTransition.scala` (new) | Medium — self-contained |
| 2. Add convenience method to internal builder ops | `internal/builders/SlideMetadataBuilderOps.scala` | 1 method (~10 lines) |
| 3. Add method to public DSL trait | `api/builders/SlideMetadataDsl.scala` | 1 abstract method (~5 lines) |
| 4. Wire through adapter | `api/builders/BuilderDslAdapters.scala` | 2 `applyX` overrides (~10 lines each in 2 adapter traits) |
| **Total** | **4 files** | **~40 lines** |

This is the main pain point. The `BuilderDslAdapters` layer exists to keep the public API clean (no phantom types, no implicit soup), but every new transition convenience must be threaded through it.

### Adding a new overlay (e.g. "SpeakerNotesOverlay")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Implement `Overlay[F]` | `internal/interpreter/middleware/SpeakerNotes.scala` (new) | Self-contained |
| 2. Add builder method to SessionBuilder | `api/builders/SessionBuilder.scala` | ~10 lines (config field + builder method) |
| 3. Wire in `run()` method | `api/builders/SessionBuilder.scala` | ~5 lines |
| **Total** | **2 files** | **~30 lines** |

Overlays are easy — the `Overlay[F]` SPI and middleware composition make this almost plug-and-play.

### Adding a new builder option (e.g. "background color per slide")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Add field to internal builder | `internal/builders/TextSlideBuilder.scala` + `SlideBuilder.scala` | ~5 lines each |
| 2. Add method to public DSL traits | `api/builders/TextSlideBuilderDsl.scala` + `SlideBuilderDsl.scala` | ~2 lines each |
| 3. Wire through adapters | `api/builders/BuilderDslAdapters.scala` | ~4 lines per impl class |
| 4. Thread value through to `SlideSpecification` | `internal/model/SlideSpecification.scala` | 1 field |
| 5. Consume in executor/renderer | `internal/interpreter/PresentationExecutorInterpreter.scala` | Varies |
| **Total** | **6–7 files** | **~40–60 lines** |

This is the most expensive kind of change — it cuts across every layer.

### Adding a new SPI method (e.g. `Slide.onResize`)

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Add method to SPI trait | `api/spi/Slide.scala` | 1 line |
| 2. Implement in `TextSlide` | `internal/TextSlide.scala` | ~3 lines |
| 3. Call from executor | `internal/interpreter/PresentationExecutorInterpreter.scala` | ~5 lines |
| 4. Update all user-facing examples | `examples/` | Per-file |
| **Total** | **3+ files** | Low effort per file but wide blast radius |

---

## Identified Maintainability Risks

### 1. `PresentationExecutorInterpreter` — Complexity Hotspot

The execution loop (lines 39–123) is a single `tailRecM` with deeply nested pattern matching and interleaved fiber/state operations. It handles:
- Slide switching (forward/back)
- Transition playback with race-based interruption
- External navigation (`setSlide` from QuickNavigation)
- User input delegation to current slide
- State bookkeeping

**Risk:** Any new navigation mode, lifecycle hook, or input handling rule must be threaded into this monolithic loop. Bug surface area is high.

**Suggestion:** Extract a state machine or command pattern. Each input/event becomes a command; the loop dispatches without deep nesting.

### 2. `SessionBuilder.run()` — Orchestration Complexity

The `run()` method (lines 238–319) manually wires 8+ components with conditional logic. Adding a new optional feature requires:
- A new config field
- Conditional instantiation in `run()`
- Wiring into the slide-change callback chain
- Potential ordering dependencies with other overlays

**Risk:** This method grows linearly with every new feature. It's already 80 lines of imperative wiring.

**Suggestion:** Consider a plugin/feature registry pattern where each feature registers itself, rather than being manually assembled.

### 3. Builder DSL Adapter Boilerplate

`BuilderDslAdapters.scala` (~240 lines) is pure mechanical delegation. Every new metadata operation requires:
1. Abstract method in `MetadataDslAdapter`
2. Implementation in `SlideMetadataAdapter`
3. Implementation in `TextMetadataAdapter`

**Risk:** Tedious but low-risk — the compiler catches any missed delegation. The real cost is developer time and code review noise.

**Mitigation (already done):** Splitting into 4 focused files helps readability. The adapter boilerplate is a justified cost for the API quality it enables.

### 4. Ticker as Single Coordination Point

All animations (transitions, overlays, idle detection) coordinate through a single `Ticker` instance. The ticker callback list is processed sequentially.

**Risk:** If an overlay's `onTick` blocks or throws, it can stall the entire render pipeline. No isolation or error boundaries exist between subscribers.

**Suggestion:** Consider per-subscriber error handling (`attempt` + logging) in the tick loop.

### 5. Internal Visibility Leaks

`SlideSpecification` and `Presentation` are `private[lote]` but are structurally exposed through `buildSpec()` on the public traits. This creates an implicit contract — internal model changes can subtly break the public API boundary.

**Risk:** Low in practice (the models are simple case classes), but worth noting for future refactoring.

---

## What's Easy to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| New custom slide (user-side) | Trivial | `Slide[F]` SPI is minimal and self-contained |
| New custom transition (user-side) | Trivial | `Transition[F]` SPI is 1 method |
| New custom overlay (user-side) | Trivial | `Overlay[F]` SPI is clean |
| New built-in overlay | Low | Plug-and-play via middleware composition |
| New example | Trivial | Self-contained files with no library changes |
| Bug fix in a specific transition | Low | Transitions are isolated implementations |
| Tuning animation defaults | Trivial | Single constant changes |

## What's Hard to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| New navigation mode (beyond arrow keys) | High | Deeply embedded in executor loop |
| Changing the slide lifecycle | High | Touches executor, Slide SPI, all implementations |
| Adding per-slide configuration that affects rendering | High | Cuts through all layers |
| Swapping terminal backend (away from JLine) | Medium-High | `NConsoleInterpreter` + `JLineTerminal` are tightly coupled |
| Changing how overlays interact with each other | Medium | Middleware composition is sequential; priority/conflict resolution would require redesign |

---

## Dependency Health

| Dependency | Version | Risk |
|-----------|---------|------|
| cats-core | 2.12.0 | Low — stable, well-maintained |
| cats-effect | 3.5.7 | Low — CE3 is stable; API frozen |
| jline | 3.27.1 | Low — mature terminal library |
| munit-cats-effect | 2.0.0 | Low — test-only |

No dependency concerns. All are mature, stable libraries with predictable release cadences.

---

## Test Architecture

**Strengths:**
- 200 tests covering builders, interpreters, middleware, transitions, and utilities
- Algebra/interpreter split enables clean mocking (mock `NConsole`, mock `Ticker`)
- Tests are focused and fast (no real terminal needed)

**Gaps:**
- `PresentationExecutorInterpreter` has limited direct testing for edge cases (e.g., rapid slide switching, concurrent `setSlide` + user input)
- No integration test that exercises the full `SessionBuilder.run()` pipeline end-to-end
- No property-based testing for animation calculations

---

## Recommendations

### Quick Wins
1. **Add error boundaries in ticker loop** — `_.callback.attempt` prevents one bad subscriber from stalling rendering
2. **Extract executor state machine** — separate navigation logic from the `tailRecM` loop for testability
3. **Add `@implicitNotFound` annotations** — improve compiler error messages for phantom-type constraints

### Medium-Term
4. **Consider a feature registry in `SessionBuilder`** — each overlay/feature registers itself rather than being manually wired in `run()`
5. **Add integration tests** — a `TestTerminal` that captures output + simulates input would allow full pipeline testing
6. **Document the "adding a new transition" workflow** — a CONTRIBUTING.md guide would help new contributors navigate the multi-file ceremony

### Long-Term
7. **Evaluate Scala 3 migration** — would simplify phantom types (opaque types, match types) and reduce boilerplate
8. **Consider decoupling from JLine** — a `Terminal` algebra with a JLine interpreter would make alternative backends (e.g., web-based) feasible

---

## Overall Verdict

**Well-structured for its size, with predictable maintenance costs.** The layering is sound and the SPIs keep extension points clean. The main maintenance burden comes from cross-cutting features that must be threaded through the builder DSL adapter layer and the central executor loop. Both are manageable at the current scale (~50 files) but would benefit from extraction patterns if the feature set grows significantly. The codebase is a good example of how tagless final + clean SPIs can enable extensibility while keeping internals private — the trade-off is the adapter ceremony, which is an acceptable cost.

