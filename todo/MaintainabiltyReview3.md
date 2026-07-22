# cats-lote — Maintainability Review (Third Pass)

_Reviewed: July 2026_

---

## Summary

Since the second review, a major cross-cutting feature has been introduced: **WebGL support with 3D spatial slide navigation**. The project has grown from a JVM-only terminal presentation tool into a cross-platform library (JVM + Scala.js) with a Three.js-based WebGL rendering backend, 3D camera animations, GPU-accelerated visual effects, and scene-aware slides.

The codebase has grown from ~57 source files to **102 main source files** (~12,750 LOC) and **33 test files** (~6,740 LOC). Despite this significant expansion, the architecture has held up well — the clean `Terminal[F]` SPI boundary meant the entire WebGL backend could be added without touching the presentation execution logic. The introduction of a `PlatformStrategy` algebra cleanly separates platform-specific concerns from the executor loop. However, the new JS-side code introduces several maintainability risks that warrant attention.

---

## Changes Since Second Review

| Previous Concern | Status | What Changed |
|------------------|--------|--------------|
| Null usage in executor factory | ⚠️ Unchanged | `onSlideChange: Int => F[Unit] = null` still present |
| Builder DSL adapter boilerplate | ⚠️ Unchanged | Still ~288 lines, plus new `.at()` / `.rotatedBy()` / `.transparentBackground()` methods |
| Single TODO in codebase (idle + animated slides) | ⚠️ Unchanged | Still unresolved |
| Race condition in `handleDelegateInput` | ⚠️ Unchanged | `stateRef.get` followed by conditional `set` still not atomic |
| `IdleAwareNConsole` not testable via `runWith()` | ⚠️ Unchanged | Documented but still a gap |
| Terminal algebra decoupling (long-term) | ✅ Addressed | `Terminal[F]` SPI now has 3 implementations: `JLineTerminal`, `ThreeJsTerminal`, and the original vision of a `Terminal` algebra is realised |
| Scala 3 migration (long-term) | ⚠️ Unchanged | Still on Scala 2.13 with kind-projector and better-monadic-for |

### Major New Features

| Feature | Impact | Key Files |
|---------|--------|-----------|
| **Cross-platform build (JVM + JS)** | `sbt-crossproject`, `sbt-scalajs`, `scalajs-dom` dependency | `build.sbt`, `project/plugins.sbt` |
| **WebGL rendering backend** | 8 new JS-only source files (~1,820 LOC) | `lote/js/src/main/scala/…/nconsole/` |
| **3D spatial slide positioning** | `.at(x, y, z)`, `.rotatedBy(rx, ry, rz)` in builder DSL | `SlidePosition.scala`, builder adapters |
| **Camera animation system** | Smooth fly-through with ease-in-out, interruptible | `CameraAnimator.scala` (461 lines) |
| **GPU visual effects** | Dissolve, smoke, glow, fade — per-character particles | `WebGLEffectRenderer.scala` (316 lines) |
| **Platform capability model** | `PlatformCapability` enum, `EffectfulTerminal` trait | `PlatformCapability.scala`, `EffectfulTerminal.scala` |
| **Platform strategy pattern** | `PlatformStrategy[F]` algebra with terminal/spatial impls | `PlatformStrategy.scala`, `SpatialPlatformStrategy.scala` |
| **Scene-aware slides** | `Scene3DRef` for slides that inject 3D geometry | `Scene3DRef.scala`, `Landscape3DSlide.scala` |
| **Shared examples module** | Cross-platform presentation code | `shared-examples/` |
| **WebGL configuration** | `WebGLConfig` case class with validation | `WebGLConfig.scala` |
| **Fallback transitions** | `.withFallback()` for graceful degradation | `WithFallbackTransition.scala`, `TransitionType.scala` |
| **Removed FlipTransition & RotateTransition** | Incompatible with spatial mode | N/A |

---

## Architecture at a Glance

```
┌─────────────────────────────────────────────────────────────────┐
│  Public API  (shared/api/)                                      │
│  ├── builders/  SessionBuilder, BuilderDslAdapters, SlideContext │
│  ├── spi/       Terminal, EffectfulTerminal, NConsole, Slide,    │
│  │              Transition, Overlay, Ticker                      │
│  ├── support/   FixedStep, Clock                                 │
│  └── data types (SlidePosition, RenderEffect, PlatformCapability,│
│                  WebGLConfig, TransitionType, Screen, etc.)      │
├─────────────────────────────────────────────────────────────────┤
│  Testkit (shared/testkit/)                                       │
│  └── SlideTestHarness, TestConsole, TestTicker, SimulatedClock   │
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
│       ├── ThreeJsTerminal      (538 lines — main entry point)    │
│       ├── CameraAnimator       (461 lines — 3D camera flights)   │
│       ├── WebGLEffectRenderer  (316 lines — GPU effects)         │
│       ├── WebGLScene           (75 lines — scene/renderer)       │
│       ├── WebGLCanvasRenderer  (102 lines — ANSI→Canvas2D)       │
│       ├── SlideLayer           (85 lines — offscreen canvas+mesh)│
│       ├── WebGLInputHandler    (114 lines — DOM→Queue bridge)    │
│       └── ThreeJsFacade        (137 lines — JS interop types)    │
└─────────────────────────────────────────────────────────────────┘
```

---

## Scoring

| Dimension | Score | Change | Notes |
|-----------|-------|--------|-------|
| **Modularity** | ★★★★☆ | — | Clean layering maintained; platform split via `PlatformStrategy` is well-designed |
| **Extensibility (user-side)** | ★★★★★ | — | SPIs remain minimal; new `PlatformCapability` + fallback mechanism enables graceful degradation |
| **Extensibility (library-side)** | ★★★★☆ | — | Feature registry still works; new platform strategy pattern simplifies adding backends |
| **Testability** | ★★★☆☆ | ↓ | JS-side code (1,820 LOC) has **zero tests**; `js.Dynamic` usage makes unit testing difficult |
| **Readability** | ★★★☆☆ | ↓ | Heavy `js.Dynamic` casting in WebGL code reduces type safety and readability |
| **Onboarding (new contributor)** | ★★★☆☆ | ↓ | Dual-platform build + Three.js knowledge + `js.Dynamic` patterns raise barrier |

---

## Feature Introduction: Effort Analysis (Updated)

### Adding a new built-in transition (e.g. "WipeTransition")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Implement transition logic | `shared/…/transition/WipeTransition.scala` (new) | Medium — self-contained |
| 2. Add convenience method to internal builder ops | `SlideMetadataBuilderOps.scala` | 1 method (~10 lines) |
| 3. Add method to public DSL trait | `SlideMetadataDsl.scala` | 1 abstract method (~5 lines) |
| 4. Wire through adapter | `BuilderDslAdapters.scala` | `applyX` override (~10 lines) |
| 5. *(New)* Add `TransitionType` variant + factory case | `TransitionType.scala`, `TransitionFactory.scala` | ~5 lines each |
| **Total** | **5–6 files** | **~45 lines** |

Slightly more than before due to `TransitionType` and factory wiring for `.withFallback()` support.

### Adding a new GPU-accelerated effect (e.g. "Shatter")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Add variant to `RenderEffect` | `RenderEffect.scala` | ~5 lines |
| 2. Implement in `WebGLEffectRenderer` | `WebGLEffectRenderer.scala` | Medium — self-contained |
| 3. Create shared transition wrapper | `ShatterTransition.scala` (new) | Copy pattern from `SmokeTransition` |
| 4. Wire into builder DSL | `SlideMetadataDsl.scala`, `BuilderDslAdapters.scala`, etc. | ~25 lines |
| **Total** | **5–6 files** | **~60 lines** |

The `WebGLEffectRenderer` is well-structured as an extension point — new effects can be added without touching `ThreeJsTerminal`.

### Adding a new rendering backend (e.g. "CanvasTerminal")

| Step | File(s) | Effort |
|------|---------|--------|
| 1. Implement `Terminal[F]` | `CanvasTerminal.scala` (new) | Medium-High |
| 2. Add entry point to `TerminalPlatform` | `TerminalPlatform.scala` | ~5 lines |
| **Total** | **2 files** | Self-contained |

The `Terminal[F]` SPI boundary makes this straightforward — no changes to shared code needed.

---

## Maintainability Observations

### 1. ⚠️ ThreeJsTerminal — 538-Line God Object

`ThreeJsTerminal.scala` is the largest file in the project and serves as the single entry point for all WebGL functionality. It creates an anonymous class that implements both `Terminal[F]` and `EffectfulTerminal[F]`, closing over 6 mutable `var` fields.

**Concerns:**
- The `initSpatialMode()` method is 80 lines of imperative setup embedded inside a closure
- 6 mutable `var` fields (`slideLayers`, `slideToLayerIndex`, `activeLayerIndex`, `sharedSceneRef`, plus implicit `glScene`, `cameraAnimator`) are captured by closures
- The `write()` method contains the hot-path rendering logic inline rather than delegating to a renderer

**Risk:** Medium — the closures make it hard to test individual pieces, and any change to the rendering pipeline requires understanding the entire 538-line file.

**Suggestion:** Extract the spatial state into a dedicated `SpatialState` class that manages layers, the slide-to-layer mapping, and the active layer index. This would make the state transitions explicit, testable, and independent of the `Terminal` implementation.

### 2. ⚠️ Zero Test Coverage for JS-Only Code

The `lote/js/src/test/` directory is **empty**. The entire WebGL rendering pipeline — 1,820 lines across 8 files — has no automated tests:

| File | Lines | Test Coverage |
|------|-------|---------------|
| `ThreeJsTerminal.scala` | 538 | ❌ None |
| `CameraAnimator.scala` | 461 | ❌ None |
| `WebGLEffectRenderer.scala` | 316 | ❌ None |
| `ThreeJsFacade.scala` | 137 | N/A (type definitions) |
| `WebGLInputHandler.scala` | 114 | ❌ None |
| `WebGLCanvasRenderer.scala` | 102 | ❌ None |
| `SlideLayer.scala` | 85 | ❌ None |
| `WebGLScene.scala` | 75 | ❌ None |

**Risk:** High — the camera math (`computeNormal`, `computeUp`, `cameraPositionFor`, `easeInOut`, `transitionDuration`), layer deduplication logic, and input translation are all untested. These are pure functions that could be tested without a browser environment.

**Suggestion:**
- Extract pure math functions (easing, lerp, normal/up computation, duration calculation, position deduplication) into a shared or JS-testable module
- Add unit tests for `WebGLInputHandler.keyEventToChars` (input mapping is critical for cross-platform parity)
- Add tests for the layer deduplication logic in `initSpatialMode`

### 3. ⚠️ Heavy `js.Dynamic` Usage — Loss of Type Safety

The WebGL code uses `scalajs.js.Dynamic` extensively for Three.js interop. While the `ThreeJsFacade.scala` provides typed facades for core Three.js classes, many operations bypass them:

```scala
// From WebGLEffectRenderer.scala
p.userData = scalajs.js.Dynamic.literal(
  startX = sceneX, startY = sceneY,
  driftX = (Math.random() - 0.5) * 120.0, ...
)
// Later:
val startX = ud.startX.asInstanceOf[Double]  // ← runtime cast, no compile-time safety
```

```scala
// From ThreeJsTerminal.scala
val meshDyn = mesh.asInstanceOf[scalajs.js.Dynamic]
ref.updateCenter(
  meshDyn.position.x.asInstanceOf[Double], ...  // ← runtime cast
)
```

```scala
// From Landscape3DSlide.scala (673 lines of js.Dynamic)
val THREE = g.THREE
val landscapeGroup = js.Dynamic.newInstance(THREE.Group)()
```

**Risk:** Medium-High — typos in property names, incorrect casts, and missing properties are all runtime errors with no compile-time feedback. The `Landscape3DSlide` (673 lines) is almost entirely untyped `js.Dynamic` code.

**Suggestion:**
- Extend `ThreeJsFacade.scala` to cover the Three.js types used by `WebGLEffectRenderer` (particles, user data)
- For `Landscape3DSlide`, consider a small typed facade for the geometry creation helpers (`Group`, `CylinderGeometry`, `ConeGeometry`, etc.) used repeatedly

### 4. ⚠️ Duplicated Particle Spawning Code

`WebGLEffectRenderer` contains two nearly identical particle-spawning methods:

| Method | Lines | Purpose |
|--------|-------|---------|
| `spawnSmokeParticles()` | ~60 lines | Creates per-character meshes for smoke effect |
| `spawnDissolveParticles()` | ~50 lines | Creates per-character meshes for dissolve effect |

Both methods:
1. Iterate over the active frame
2. Skip whitespace characters
3. Create an offscreen canvas per character
4. Copy the character's pixel data from the main canvas
5. Create a Three.js texture, material, geometry, and mesh
6. Position the mesh in world space
7. Store per-particle metadata in `userData`

The only differences are the `userData` fields (smoke has drift/rotation/shrink; dissolve has fade delay/speed).

**Risk:** Low — the code works, but a bug fix in one method (e.g., fixing the world-space offset calculation) must be applied to both.

**Suggestion:** Extract a shared `spawnCharacterParticles(makeUserData: (Int, Int) => js.Dynamic)` method that handles the common canvas/texture/mesh setup and delegates the per-particle metadata to a callback.

### 5. ⚠️ Landscape3DSlide — 673 Lines of Untyped JS Interop

`Landscape3DSlide.scala` is the single largest file in the project and is almost entirely imperative `js.Dynamic` code creating Three.js geometry. It mixes:
- 3D geometry construction (terrain, trees, castles, water, stars, figures, fireflies)
- Camera orbit logic
- Animation tick handler
- Input handling (WASD controls)
- Lifecycle management (start/stop/ensureGeometry)

**Risk:** Medium — this is example code, not library code, so the blast radius is limited. However, it demonstrates a pattern that users might follow when building their own scene-aware slides, and that pattern is fragile.

**Suggestion:**
- Consider providing a small typed helper library for common Three.js operations (creating meshes, lights, groups) to reduce the `js.Dynamic` surface area
- The terrain height function is duplicated 5 times in the file — extract it to avoid drift between the terrain and objects placed on it

### 6. ✅ PlatformStrategy Pattern — Well Designed

The `PlatformStrategy[F]` algebra is the cleanest architectural addition. It:
- Keeps the executor loop completely platform-agnostic
- Auto-selects based on `NConsole.capabilities` (no boolean flags)
- Has a trivially simple terminal implementation (3-line no-op)
- Has a well-structured spatial implementation with proper `Ref`-based state and interruptible camera navigation

**No action needed.** This is a textbook example of the Strategy pattern applied in a tagless final context.

### 7. ✅ Graceful Degradation Model

The `PlatformCapability` + `WithFallbackTransition` + `TransitionType` combination provides a clean degradation story:

```scala
.smokeTransition()                        // GPU smoke on WebGL
.withFallback(TransitionType.Grab())      // Grab on terminal
```

Transitions internally check `console.capabilities` and choose the right code path. The `TransitionType` ADT prevents users from needing to construct `Transition[F]` instances just for fallback specification.

**No action needed.** This design cleanly separates the "what" (transition type) from the "how" (platform-specific implementation).

### 8. ⚠️ Resize Handling Incomplete

`WebGLScene` listens for `window.resize` events and updates `cols`/`rows`, but:
- `SlideLayer` dimensions are fixed at creation time — resize does not recreate or resize the offscreen canvases
- The `CameraAnimator` uses the initial viewport dimensions for framing calculations; these are not updated on resize
- `ThreeJsTerminal` does not re-render existing layers after resize

**Risk:** Low in practice (presentations typically run full-screen without resizing), but could produce visual artifacts if the browser window is resized during a presentation.

**Suggestion:** Document that resize during a presentation is not supported, or implement a full resize handler that recreates layers with new dimensions.

### 9. ⚠️ Scene3DRef Uses `Option[Any]` Return Type

The `Terminal.scene3DRef` method returns `Option[Any]`, which forces users to cast:

```scala
val sceneRef = ctx.scene3DRef.map(_.asInstanceOf[Scene3DRef])
```

**Risk:** Low — this is a deliberate trade-off to keep `Scene3DRef` (a JS-only type) out of the shared API. But it means the cast is unchecked and will fail at runtime if misused.

**Suggestion:** Consider a platform-specific extension method or a phantom-typed capability token that provides a safe cast path without polluting the shared API with JS types.

### 10. Previous Concern: Null in Executor Factory (Still Present)

```scala
def make[F[_]: Temporal: NConsole](
    presentation: Presentation[F],
    onSlideChange: Int => F[Unit] = null  // ← still null in Scala
): F[PresentationExecutor[F]]
```

**Risk:** Same as before — low, internal only, but un-idiomatic.

---

## What's Easy to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| New custom slide/transition/overlay (user-side) | Trivial | SPIs remain minimal |
| New built-in feature (overlay + lifecycle) | Low | `Feature[F]` registry + `registerFeature()` |
| New GPU effect | Low-Medium | `WebGLEffectRenderer` is a clean extension point |
| New rendering backend | Medium | `Terminal[F]` SPI is the only interface to implement |
| New example | Trivial | Self-contained, no library changes |
| Bug fix in a specific transition/overlay | Low | Isolated implementations |
| Tuning animation defaults / camera easing | Trivial | `WebGLConfig` case class with defaults |
| Adding a test for shared components | Low | Testkit provides deterministic harnesses |

## What's Hard to Change

| Change Type | Effort | Why |
|-------------|--------|-----|
| New built-in transition convenience | Medium | 5–6 file ceremony (adapter + `TransitionType`) |
| Per-slide config that affects rendering | High | Cuts through all layers |
| Changing the slide lifecycle (SPI) | High | Touches executor, SPI, all implementations |
| Refactoring ThreeJsTerminal internals | Medium-High | Closures over mutable vars; no tests to catch regressions |
| Adding tests for WebGL code | Medium | Need to extract pure logic from DOM-dependent code first |
| Changing 3D coordinate system or camera model | High | `CameraAnimator` + `SlideLayer` + `SpatialPlatformStrategy` all coupled to current conventions |
| Browser resize support | High | Requires recreating layers, updating camera, re-rendering all active content |

---

## Dependency Health

| Dependency | Version | Platform | Risk |
|-----------|---------|----------|------|
| cats-core | 2.12.0 | Shared | Low — stable |
| cats-effect | 3.5.7 | Shared | Low — CE3 API frozen |
| cats-effect-testkit | 3.5.7 | Shared (test) | Low |
| jline | 3.27.1 | JVM | Low — mature |
| scalajs-dom | 2.8.0 | JS | Low — stable |
| munit-cats-effect | 2.0.0 | Shared (test) | Low |
| munit | 1.0.3 | Shared (test) | Low |
| kind-projector | 0.13.3 | Shared (compiler) | Low — but blocks Scala 3 migration |
| better-monadic-for | 0.3.1 | Shared (compiler) | Low — but blocks Scala 3 migration |
| **Three.js** | **CDN (global)** | **JS (runtime)** | **⚠️ Medium — not version-pinned** |

**Three.js risk note:** Three.js is loaded via `<script>` tag from CDN and accessed as `window.THREE`. There is no version pin in the build — the facade types in `ThreeJsFacade.scala` may break if Three.js makes a breaking API change. Consider documenting the minimum supported Three.js version and/or pinning it in the example HTML files.

---

## Test Architecture

**Strengths (unchanged from second review):**
- ~200+ tests covering shared layers
- `SlideTestHarness` enables deterministic full-pipeline testing via `runWith()`
- `SimulatedClock` + `TestTicker` remove timing non-determinism
- Command pattern in executor enables isolated unit testing

**New gaps (introduced with WebGL):**
- **Zero JS-only tests** — 1,820 lines of rendering code with no test coverage
- Camera math (normals, up vectors, easing, duration calculation) is pure and testable but untested
- Input mapping (`keyEventToChars`) is critical for cross-platform parity but untested
- Layer deduplication logic in `initSpatialMode` is complex but untested
- No integration test that exercises `SpatialPlatformStrategy` with a mock `EffectfulTerminal`
- `Landscape3DSlide` has no tests (acceptable for example code, but the 673 LOC is a risk)

**Previous gaps (still present):**
- No property-based testing for animation math
- No stress/chaos test for concurrent `setSlide` + rapid user input
- Idle detection TODO still unresolved

---

## Recommendations

### Critical (Safety & Correctness)
1. **Add tests for camera math** — `computeNormal`, `computeUp`, `easeInOut`, `transitionDuration`, `euclidean` are pure functions. Extract them from `CameraAnimator` into a testable object (can be in shared or JS test scope)
2. **Add tests for input mapping** — `WebGLInputHandler.keyEventToChars` determines cross-platform input parity. Test the mapping table against the JLine equivalents
3. **Add tests for layer deduplication** — the `initSpatialMode` position-to-layer mapping logic is critical and non-trivial

### Quick Wins
4. **Extract particle spawning** — deduplicate `spawnSmokeParticles` / `spawnDissolveParticles` into a shared helper
5. **Pin Three.js version** — add a version comment or `integrity` attribute to the CDN `<script>` tags in example HTML files
6. **Extract terrain height function** in `Landscape3DSlide` — currently duplicated 5 times
7. **Replace `null` with default no-op** in `PresentationExecutorInterpreter.make()` — same recommendation as second review

### Medium-Term
8. **Break up ThreeJsTerminal** — extract spatial state management into a `SpatialState` class, reducing the 538-line closure to a simpler delegation structure
9. **Extend ThreeJsFacade** — cover particle-related types to reduce `js.Dynamic` surface in `WebGLEffectRenderer`
10. **Add `SpatialPlatformStrategy` integration test** — create a mock `EffectfulTerminal` that records applied `RenderEffect`s and verify the strategy's camera navigation + layer activation sequence
11. **Document resize limitations** — or implement proper resize handling

### Long-Term
12. **Scala 3 migration** — same as before; additionally, Scala 3's union types could improve the `scene3DRef: Option[Any]` pattern
13. **Consider typed 3D helpers** — a small utility library for common Three.js patterns would improve the experience of building scene-aware slides and reduce `js.Dynamic` usage in user code
14. **CONTRIBUTING.md** — document the cross-platform build workflow, how to run browser examples, and the "adding a new effect" ceremony

---

## Overall Verdict

**The WebGL introduction is architecturally sound but introduces a testing gap and readability debt on the JS side.**

The positive:
- The **`PlatformStrategy` pattern** cleanly separates platform concerns from the executor — the executor loop didn't change at all
- The **`PlatformCapability` + fallback** model enables transitions to work across all backends without `if-browser-then-else` logic in user code
- The **`Terminal[F]` SPI** proved its value: the entire Three.js backend was added as a new implementation with zero changes to shared presentation logic
- The **`WebGLConfig`** case class with validation makes the renderer tunable without touching internals
- The **shared-examples module** demonstrates code reuse across JVM and browser targets

The negative:
- **1,820 lines of JS code with zero tests** — the camera math, input mapping, and layer deduplication logic are all candidates for regression bugs
- **Heavy `js.Dynamic` usage** reduces type safety in the rendering layer; runtime `ClassCastException`s are possible on Three.js API changes
- **ThreeJsTerminal** at 538 lines is a complexity hotspot that's hard to refactor without tests
- **Landscape3DSlide** at 673 lines shows that the scene-aware slide pattern pushes users toward large, untyped JS interop files

The codebase has grown from ~57 to ~102 source files, with the JS-only portion (~11 files, ~1,990 LOC including API types) representing about 16% of the total codebase but 0% of the test coverage. Addressing the testing gap for the pure-logic portions of the WebGL code should be the top priority before the next round of features.

**Maintenance confidence: Medium-High** for shared code (well-tested, clean architecture), **Medium-Low** for JS-only code (no tests, heavy `js.Dynamic`, mutable closures). A new contributor with Cats Effect + Scala.js + Three.js experience could be productive, but the Three.js-specific portions require JavaScript graphics programming knowledge that significantly narrows the contributor pool.

