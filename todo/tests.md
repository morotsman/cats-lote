# Test Coverage Gap Analysis

## Well-Covered Areas (shared code)

- **Transitions**: Grab, Morph, Replace, FallingCharacters, Characters — all have dedicated specs
- **Console internals**: Aligner, AnsiParser, NConsoleInterpreter, AnsiFrameRenderer, IdleAwareNConsole
- **Middleware**: Timer, ProgressBar, Idle overlay, Middleware composition
- **API builders**: SessionBuilder, TextSlideBuilder, SlideBuilder
- **API support**: TickedSlide, GlideLayer, FixedStep, Easing
- **Testkit**: TestConsole, TestTicker, SimulatedClock, SlideTestHarness (all exercised in `TestFrameworkShowcaseSpec`)
- **Core interpreter**: PresentationExecutor (navigation, commands, lifecycle)
- **Core model**: Screen, Alignment, UserInput
- **Layout**: Grid, circle, spiral, and line algorithms — `LayoutSpec` (35 tests)
- **Feature**: Lifecycle hooks, `fromOverlay` factory, composition — `FeatureSpec` (11 tests)
- **WebGLConfig**: All validation rules and boundary conditions — `WebGLConfigSpec` (16 tests)

## Gaps in Shared Code (`lote/shared/src/main`)

| File | Description | Risk |
|---|---|---|
| ~~**`Layout.scala`**~~ | ~~Grid, circle, spiral, and line layout algorithms~~ | ✅ Resolved — `LayoutSpec` |
| ~~**`Feature.scala`**~~ | ~~Feature lifecycle hooks~~ | ✅ Resolved — `FeatureSpec` |
| **`PlatformStrategy.scala`** | Platform-specific rendering strategy (setup, activate, navigate) | Medium — internal trait, but behavior varies by platform |
| **`AnimationSettings.scala`** | Animation step duration config | Low — simple case class |
| **`PlatformCapability.scala`** | Sealed trait (CharacterGrid, SubPixelRendering, Effects, Transforms3D) | Low — pure data |
| **`TransitionType.scala`** | Sealed trait (Grab, Morph, Replace, etc.) | Low — pure data |
| **`Clock.scala`** | AnimationClock trait with implicit derivation | Low — indirectly tested via SimulatedClock |

## Gaps in JVM Code (`lote/jvm/src/main`)

| File | Description | Risk |
|---|---|---|
| **`TerminalPlatform.scala`** | JVM terminal platform setup | Medium — platform-specific |
| **`LoteApp.scala`** | JVM app entry point | Low — integration/wiring |
| **`JLineTerminal.scala`** | Only signal handling tested; core terminal I/O untested | Medium |

## Gaps in JS/WebGL Code (`lote/js/src/main`)

| File | Description | Risk |
|---|---|---|
| **`WebGLScene.scala`** | 3D scene management | **High** |
| **`WebGLEffectRenderer.scala`** | Effect rendering pipeline | **High** |
| **`WebGLCanvasRenderer.scala`** | Canvas rendering | **High** |
| **`CameraAnimator.scala`** | Camera animation logic | **High** |
| **`WebGLInputHandler.scala`** | Input event handling | Medium |
| **`SlideLayer.scala`** | 3D slide layer management | Medium |
| **`SpatialState.scala`** | Spatial positioning state | Medium |
| ~~**`WebGLConfig.scala`**~~ | ~~Configuration~~ | ✅ Resolved — `WebGLConfigSpec` |
| **`Scene3DRef.scala`** | Scene reference wrapper | Low |
| **`ThreeJsFacade.scala`** | JS interop facade | Low (hard to unit test) |
| **`ThreeJsTerminal.scala`** | Terminal abstraction for Three.js | Medium |
| **`RafTickerInterpreter.scala`** | requestAnimationFrame ticker | Low (browser API) |
| **`TerminalPlatform.scala` (JS)** | JS platform setup | Medium |
| **`LoteApp.scala` (JS)** | JS app entry point | Low |

**`CameraMath.scala`** has excellent coverage (`CameraMathSpec`, 40+ tests).

## JS/WebGL Testability — Facade & Abstraction Analysis

Browser-based testing tools (Cypress, Playwright) are **not recommended** for this project:
- The rendering targets WebGL canvas / offscreen Canvas 2D — DOM assertion tools can't inspect canvas content
- No npm ecosystem exists (no `package.json`, no bundler) — significant infrastructure to set up
- Each test run requires `sbt fastLinkJS` compilation — slow feedback loop
- The high-value logic is pure math that's already testable in Scala.js MUnit

The recommended approach is **introducing thin abstractions (traits/facades) over browser and Three.js APIs**, allowing the real logic to be tested against stub implementations in the existing Scala.js MUnit suite.

### Current coupling inventory

| File | Mutable vars | Three.js types used | DOM/Browser APIs used |
|---|---|---|---|
| **CameraMath** | 0 | 0 | 0 |
| **WebGLConfig** | 0 | 0 | 0 |
| **SpatialState** | 4 | `ThreeMesh` | `HTMLCanvasElement` |
| **WebGLEffectRenderer** | 6 | `ThreeMesh`, `Material`, `Texture`, `Geometry`, `Color` | `Canvas2D`, `devicePixelRatio`, `document.createElement` |
| **CameraAnimator** | 9 | `ThreePerspectiveCamera`, `ThreeVector3` | `requestAnimationFrame`, `performance.now`, `document.addEventListener` |
| **WebGLScene** | 2 | `ThreeScene`, `ThreeColor`, `ThreeWebGLRenderer` | `HTMLElement`, `devicePixelRatio`, `window.addEventListener` |
| **SlideLayer** | 3 | `ThreeCanvasTexture`, `ThreeMesh`, `Geometry`, `Material` | `Canvas2D`, `devicePixelRatio`, `document.createElement` |
| **WebGLCanvasRenderer** | 0 | 0 | `CanvasRenderingContext2D` (parameter) |
| **WebGLInputHandler** | 0 | 0 | `HTMLCanvasElement`, `KeyboardEvent`, `MouseEvent`, `addEventListener` |
| **ThreeJsTerminal** | 1 | `ThreeCanvasTexture`, `ThreeMesh`, `Material`, `Geometry` | `Canvas2D`, `devicePixelRatio`, `document.createElement` |
| **RafTickerInterpreter** | 0 (Ref) | 0 | `requestAnimationFrame` |

### Proposed abstractions

#### 1. `SceneBackend` trait — abstract over Three.js scene graph

**What it replaces:** Direct use of `ThreeScene`, `ThreeMesh`, `ThreeColor`, `ThreeWebGLRenderer`, `ThreePerspectiveCamera` across `WebGLScene`, `SpatialState`, `WebGLEffectRenderer`, `CameraAnimator`, `SlideLayer`.

```scala
private[nconsole] trait SceneBackend {
  type MeshRef
  type CameraRef

  def createPlaneMesh(width: Double, height: Double, textureSource: CanvasRef): MeshRef
  def addToScene(mesh: MeshRef): Unit
  def removeFromScene(mesh: MeshRef): Unit
  def setMeshPosition(mesh: MeshRef, x: Double, y: Double, z: Double): Unit
  def setMeshRotation(mesh: MeshRef, rx: Double, ry: Double, rz: Double): Unit
  def setMeshOpacity(mesh: MeshRef, opacity: Double): Unit
  def setMeshScale(mesh: MeshRef, sx: Double, sy: Double, sz: Double): Unit
  def getMeshPosition(mesh: MeshRef): (Double, Double, Double)
  def disposeMesh(mesh: MeshRef): Unit

  def createCamera(fov: Double, aspect: Double, near: Double, far: Double): CameraRef
  def setCameraPosition(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def setCameraUp(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def setCameraLookAt(cam: CameraRef, x: Double, y: Double, z: Double): Unit
  def setCameraFov(cam: CameraRef, fov: Double): Unit
  def updateCameraProjection(cam: CameraRef): Unit

  def setBackgroundColor(color: String): Unit
  def render(camera: CameraRef): Unit

  def viewportWidth: Int
  def viewportHeight: Int
  def centerX: Double
  def centerY: Double
}
```

**Test stub:** `StubSceneBackend` records all calls in a `List[SceneOp]` log for assertion.

**Impact:** Decouples `SpatialState`, `CameraAnimator`, `WebGLEffectRenderer`, and `SlideLayer` from Three.js. These 4 files contain the most complex logic and the most mutable state.

**Files affected:** `WebGLScene` (implements it), `SpatialState`, `CameraAnimator`, `WebGLEffectRenderer`, `SlideLayer`, `ThreeJsTerminal`.

#### 2. `CanvasFactory` trait — abstract over DOM canvas creation

**What it replaces:** Direct calls to `dom.document.createElement("canvas")`, `canvas.getContext("2d")`, `dom.window.devicePixelRatio` scattered across `SlideLayer`, `WebGLEffectRenderer`, `ThreeJsTerminal`.

```scala
private[nconsole] trait CanvasFactory {
  type CanvasRef
  type ContextRef

  def createCanvas(width: Int, height: Int): CanvasRef
  def getContext2D(canvas: CanvasRef): ContextRef
  def devicePixelRatio: Double
}
```

**Test stub:** `StubCanvasFactory` returns no-op references or records calls.

**Impact:** Medium. Removes the `document.createElement` / `devicePixelRatio` dependency from `SlideLayer`, `WebGLEffectRenderer`, and `ThreeJsTerminal`. However, `WebGLCanvasRenderer.renderLine` still needs a real or mocked `CanvasRenderingContext2D` — this trait alone doesn't make canvas _rendering_ testable, just canvas _creation_.

**Files affected:** `SlideLayer`, `WebGLEffectRenderer`, `ThreeJsTerminal`.

#### 3. `AnimationScheduler` trait — abstract over requestAnimationFrame

**What it replaces:** Direct `dom.window.requestAnimationFrame` and `dom.window.cancelAnimationFrame` in `CameraAnimator` and `RafTickerInterpreter`.

```scala
private[nconsole] trait AnimationScheduler {
  def requestFrame(callback: Double => Unit): Int
  def cancelFrame(id: Int): Unit
  def now(): Double
}
```

**Test stub:** `ManualScheduler` collects pending callbacks and lets tests advance time manually — similar to `TestTicker` but for the JS animation loop.

**Impact:** High for `CameraAnimator`. The entire `animateSpatial` method (the rAF loop) becomes testable: you can verify that camera position converges correctly over simulated frames, that easing is applied, and that the animation completes.

**Files affected:** `CameraAnimator`, `RafTickerInterpreter`.

#### 4. `KeyMapper` pure function extraction — already possible without new traits

**What it replaces:** The private `keyEventToChars(ev: KeyboardEvent)` method in `WebGLInputHandler`.

**Change:** Split into a pure function `keyNameToChars(key: String, ctrlKey: Boolean): Seq[Int]` (testable) and a thin wrapper that reads `ev.key` / `ev.ctrlKey` and calls `ev.preventDefault()`.

```scala
// Pure — testable
private[nconsole] def keyNameToChars(key: String, ctrlKey: Boolean): Seq[Int] = key match {
  case "ArrowUp"    => Seq(27, 91, 65)
  case "ArrowDown"  => Seq(27, 91, 66)
  // ... same logic, no ev.preventDefault()
}

// Side-effecting — thin wrapper
private def keyEventToChars(ev: KeyboardEvent): Seq[Int] = {
  val result = keyNameToChars(ev.key, ev.ctrlKey)
  if (result.nonEmpty) ev.preventDefault()
  result
}
```

**Impact:** Low effort, immediate testability for the key-mapping table (arrow keys, ctrl combos, special keys).

**Files affected:** `WebGLInputHandler` only.

#### 5. `SpatialMath` pure companion — extract from `SpatialState.init()`

**What it replaces:** The position resolution and deduplication logic embedded in `SpatialState.init()`.

```scala
private[nconsole] object SpatialMath {
  /** Fill-forward: slides without explicit positions inherit the previous slide's position. */
  def resolvePositions(positions: Vector[Option[SlidePosition]]): Vector[SlidePosition]

  /** Deduplicate: slides sharing a 6-DOF position share a single layer index. */
  def deduplicatePositions(resolved: Vector[SlidePosition]): (Vector[SlidePosition], Vector[Int])
}
```

**Impact:** Medium. These are non-trivial algorithms (especially deduplication with `LinkedHashMap`) that currently have zero test coverage.

**Files affected:** `SpatialState` only.

#### 6. `EffectMath` pure companion — extract from `WebGLEffectRenderer`

**What it replaces:** The per-frame particle animation math in `dissolve()` and `smoke()`, and the color parsing in `glow()`.

```scala
private[nconsole] object EffectMath {
  /** Dissolve: per-particle opacity based on global progress and particle-specific delay/speed. */
  def dissolveOpacity(progress: Double, fadeDelay: Double, fadeSpeed: Double): Double

  /** Smoke: per-particle position, opacity, scale, rotation based on progress and particle params. */
  case class SmokeFrameResult(x: Double, y: Double, rotZ: Double, opacity: Double, scale: Double)
  def smokeFrame(progress: Double, startX: Double, startY: Double,
                 driftX: Double, driftY: Double, rotSpeed: Double,
                 fadeDelay: Double, shrinkRate: Double): SmokeFrameResult

  /** Glow: parse CSS hex color and compute RGB intensity values. */
  def glowColor(hexColor: String, intensity: Double): (Double, Double, Double)

  /** Fade: clamp opacity to [0, 1]. */
  def clampOpacity(opacity: Double): Double
}
```

**Impact:** Medium. The dissolve/smoke math runs at 60fps during transitions — bugs here cause visible animation glitches. Testing the math in isolation catches regressions without needing a browser.

**Files affected:** `WebGLEffectRenderer` only.

### Effort vs. impact assessment

| Abstraction | Effort | Test coverage gained | Risk of regressions during refactor |
|---|---|---|---|
| **`SpatialMath` extraction** | Low | Position resolution + deduplication algorithms | Low — move pure code to companion |
| **`EffectMath` extraction** | Low | Dissolve/smoke/glow per-frame math | Low — move pure code to companion |
| **`KeyMapper` extraction** | Low | Full keyboard mapping table | Low — split one function |
| **`AnimationScheduler` trait** | Medium | Camera animation convergence, timing, easing | Medium — must thread trait through `CameraAnimator` constructor |
| **`SceneBackend` trait** | High | Scene graph operations in `SpatialState`, `CameraAnimator`, `WebGLEffectRenderer` | High — touches 6+ files, changes type signatures |
| **`CanvasFactory` trait** | Medium | Canvas creation in `SlideLayer`, `WebGLEffectRenderer` | Medium — touches 3 files |

### Recommendation

Start with the **low-effort pure extractions** (items 4, 5, 6) — they require no new traits, no constructor changes, and no risk of breaking existing code. Each one is a simple refactor: move a block of code into a companion object, add tests.

Then consider `AnimationScheduler` if camera animation bugs become a concern — it's the highest-value trait because it makes the entire rAF loop testable.

`SceneBackend` is the most comprehensive solution but also the most invasive. It should only be pursued if the project needs significant WebGL feature work and the team wants confidence in scene graph operations. For a presentation framework that's mostly stable, the ROI may not justify the refactoring cost.

## Top Recommendations (by impact)

1. ~~**`Layout.scala`**~~ ✅ Resolved
2. ~~**`Feature.scala`**~~ ✅ Resolved
3. ~~**`WebGLConfig.scala`**~~ ✅ Resolved
4. ~~**`SpatialMath` extraction**~~ ✅ Resolved — `SpatialMathSpec` (18 tests)
5. ~~**`EffectMath` extraction**~~ ✅ Resolved — `EffectMathSpec` (27 tests)
6. ~~**`KeyMapper` extraction**~~ ✅ Resolved — `KeyMapperSpec` (29 tests)
7. ~~**`AnimationScheduler` trait**~~ ✅ Resolved — `AnimationScheduler` trait + `DomAnimationScheduler` + `ManualScheduler` stub
8. ~~**`SceneBackend` trait**~~ ✅ Resolved — `SceneBackend` trait + `StubSceneBackend` stub
9. ~~**`CanvasFactory` trait**~~ ✅ Resolved — `CanvasFactory` trait + `DomCanvasFactory` + `StubCanvasFactory` stub
10. **`PlatformStrategy.scala`** — Platform-specific rendering decisions are untested.
11. **`JLineTerminal.scala`** — Only signal handling is tested; core terminal I/O is not.
