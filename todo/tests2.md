# Test Coverage Gap Analysis — Post-Facade Assessment

_Updated: July 23 2026, after wiring `SceneBackend` into `SlideLayer` and `SpatialState` (Priority 3 complete)._

## Current Test Inventory

| Suite | Location | Tests | Notes |
|---|---|---|---|
| `CameraMathSpec` | js/test | 40+ | Pure math: slide center, camera position, normals, up vectors, easing, lerp |
| `CameraAnimatorSpec` | js/test | 14 | Full animation loop: moveTo, jumpTo, convergence, zoom, cleanup via `StubSceneBackend` + `ManualScheduler` |
| `WebGLConfigSpec` | js/test | 16 | Validation rules and boundary conditions |
| `SlideLayerMathSpec` | js/test | 21 | Pure mesh positioning, rotation (deg→rad), cols/rows, canvas dimensions |
| `SlideLayerSpec` | js/test | 23 | ✅ **NEW** — Full construction lifecycle via `StubSceneBackend` + `StubCanvasFactory`: mesh creation, position, rotation, depthWrite, dispose |
| `SpatialStateSpec` | js/test | 17 | ✅ **NEW** — Full init pipeline: layer creation, deduplication, scene graph ops, activation, dispose |
| `SpatialMathSpec` | shared/test | 18 | Position resolution + deduplication algorithms |
| `EffectMathSpec` | shared/test | 27 | Dissolve opacity, smoke frame, glow color, clamp opacity |
| `KeyMapperSpec` | shared/test | 29 | Full keyboard mapping table (arrow keys, special keys, ctrl combos, edge cases) |
| _(shared specs)_ | shared/test | 500+ | Transitions, console internals, middleware, builders, etc. |
| **Total** | | **726** | All passing (JS: 726, JVM: 608) |

## What Was Done

### ✅ Priority 1: Wired `SceneBackend` into `CameraAnimator` (COMPLETE)

**Changes made:**
- Refactored `CameraAnimator` to accept `backend: SceneBackend` instead of `scene: WebGLScene`
- Replaced `ThreePerspectiveCamera` direct manipulation with `SceneBackend` camera operations (`setCameraPosition`, `setCameraUp`, `setCameraLookAt`, `setCameraFov`, `updateCameraProjection`)
- Added `getCameraPosition(cam)` and `getCameraUp(cam)` to `SceneBackend` trait (needed for reading camera state at animation start)
- Updated `StubSceneBackend` to track camera positions and up vectors
- Added `enableZoomListener: Boolean = true` param to skip DOM event wiring in tests
- Exposed `handleZoomKey(key: String)` as package-private for direct test invocation
- Created `WebGLSceneBackend` production impl wrapping `WebGLScene`
- Updated `ThreeJsTerminal` to pass `WebGLSceneBackend` to `CameraAnimator`
- Updated `SpatialState` to use `cameraRef` (was `perspCameraRef`)

**Tests written (`CameraAnimatorSpec` — 14 tests):**
- ✅ `moveTo` first call places camera immediately (no animation)
- ✅ `moveTo` to same position is a no-op
- ✅ `moveTo` triggers `animateSpatial` which schedules rAF callbacks
- ✅ Animation converges to target position over simulated frames
- ✅ `jumpTo` cancels running animation and snaps immediately
- ✅ `isAnimating` returns true during animation, false after completion
- ✅ Zoom in adjusts FOV correctly
- ✅ Zoom out adjusts FOV correctly
- ✅ Zoom reset restores base FOV
- ✅ Zoom in clamped at maxZoom
- ✅ `cleanup` cancels pending animation
- ✅ `moveTo` cancels previous animation before starting new one
- ✅ `initSpatialMode` creates camera with correct FOV and aspect
- ✅ Intermediate frames show partial progress

### ✅ Priority 2: Extracted `SlideLayerMath` (COMPLETE)

**Changes made:**
- Created `SlideLayerMath` object with pure computation methods: `computeCols`, `computeRows`, `meshPosition`, `meshRotation`, `canvasDimensions`
- Added `canvasFactory: CanvasFactory` parameter to `SlideLayer` (uses `devicePixelRatio` from it)
- Added computed val fields (`meshX`, `meshY`, `meshZ`, `meshRotX/Y/Z`) to `SlideLayer` for observability

**Tests written (`SlideLayerMathSpec` — 21 tests):**
- ✅ cols/rows computation (6 tests: standard, non-divisible, edge cases)
- ✅ mesh position computation (4 tests: origin, offset, negative, different viewports)
- ✅ mesh rotation conversion (6 tests: zero, 90°, 180°, 360°, negative, arbitrary)
- ✅ canvas dimensions with DPR (4 tests: 1x, 2x, 1.5x, 3x)
- ✅ transparent background documentation test (1 test)

### ✅ Priority 3: Wired `SceneBackend` into `SlideLayer` + `SpatialState` (COMPLETE)

**Changes made:**
- Added `backend: SceneBackend = null` parameter to `SlideLayer`; when non-null, all mesh ops go through the backend
- Replaced direct `dom.document.createElement("canvas")` with `canvasFactory.createCanvas(...)` in `SlideLayer`
- Replaced direct `canvas.getContext("2d")` with `canvasFactory.getContext2D(...)` in `SlideLayer`
- Replaced `new ThreeMesh(...)`, `new ThreePlaneGeometry(...)`, `new ThreeMeshBasicMaterial(...)` with `backend.createPlaneMesh(...)`
- Replaced `mesh.position.set(...)` with `backend.setMeshPosition(...)`
- Replaced `mesh.rotation.set(...)` with `backend.setMeshRotation(...)`
- Replaced `mat.depthWrite = false` with `backend.setMeshDepthWrite(mesh, false)`
- Replaced `texture.dispose(); mat.dispose(); geo.dispose()` with `backend.disposeMesh(meshRef)`
- Added `setMeshDepthWrite(mesh, depthWrite)` to `SceneBackend` trait
- Added `getMeshRotation(mesh)` to `SceneBackend` trait
- Updated `WebGLSceneBackend` with `setMeshDepthWrite` and `getMeshRotation` implementations
- Updated `StubSceneBackend` to track mesh rotations and log `SetMeshDepthWrite` operations
- Added `backend: SceneBackend` and `canvasFactory: CanvasFactory` parameters to `SpatialState`
- Replaced `glScene.scene.add(layer.mesh)` with `backend.addToScene(layer.meshRef)`
- Replaced `layer.mesh.position` access in `activateLayer` with `backend.getMeshPosition(layer.meshRef)`
- Replaced `glScene.viewportWidth/Height` usages with `backend.viewportWidth/Height` in `SpatialState`
- Made `Scene3DRef` creation conditional on `glScene != null` to support headless testing
- Updated `ThreeJsTerminal` to pass `sceneBackend` to `SpatialState`
- Kept backward-compatible `mesh` accessor on `SlideLayer` for production code that still needs `ThreeMesh`
- Added `texture` accessor that gets texture from mesh material (backward compat for `ThreeJsTerminal`)

**Tests written (`SlideLayerSpec` — 23 tests):**
- ✅ Construction creates plane mesh with correct dimensions
- ✅ Construction sets mesh position at world offset + half viewport
- ✅ Construction sets mesh rotation converting degrees to radians
- ✅ Construction with zero rotation sets zero radians
- ✅ Transparent background disables depth write on the mesh
- ✅ Non-transparent background does NOT disable depth write
- ✅ Construction creates offscreen canvas with correct physical dimensions (1x DPR)
- ✅ Construction creates offscreen canvas scaled by DPR (2x)
- ✅ Construction obtains 2D context from canvas
- ✅ Cols computed correctly from viewport and cell width
- ✅ Rows computed correctly from viewport and cell height
- ✅ meshX/meshY/meshZ reflect computed position
- ✅ meshRotX/Y/Z reflect computed rotation in radians
- ✅ Dispose calls backend.disposeMesh with the mesh reference
- ✅ Dispose targets the correct mesh ID
- ✅ Multiple layers create distinct mesh IDs
- ✅ Construction operations are in correct order: create → position → rotation
- ✅ Transparent bg: depthWrite is set after rotation
- ✅ previousFrame is initially empty
- ✅ canvasOffsetX/Y default to 0.0
- ✅ fixedRows is initially empty
- ✅ Index is preserved
- ✅ transparentBg flag is preserved

**Tests written (`SpatialStateSpec` — 17 tests):**
- ✅ init creates one SlideLayer per unique position
- ✅ init deduplicates layers sharing the same position
- ✅ init adds each layer mesh to the scene
- ✅ init creates meshes with correct viewport dimensions
- ✅ init positions mesh at world offset + half viewport
- ✅ init applies rotation from SlidePosition
- ✅ init with transparentBackground sets depthWrite false
- ✅ init creates offscreen canvases for each layer
- ✅ init canvas dimensions scaled by DPR
- ✅ init calls initSpatialMode on CameraAnimator
- ✅ activateLayer selects the correct layer
- ✅ activateLayer with deduplicated slides maps to same layer
- ✅ activateLayer with no glScene leaves sharedSceneRef as None
- ✅ activeLayer is None before init
- ✅ slideLayers is empty before init
- ✅ dispose calls disposeMesh on all layers
- ✅ init resolves None positions by inheriting from previous slide

## Test Stubs Available

| Stub | Location | Purpose |
|---|---|---|
| `ManualScheduler` | js/test | Deterministic `requestAnimationFrame` replacement: manual `advanceTime()`, `tick()`, `tickAt()` |
| `StubSceneBackend` | js/test | Records all scene graph operations as `SceneOp` ADT for assertion; tracks camera position/up/mesh position/rotation |
| `StubCanvasFactory` | js/test | Records canvas creation operations as `CanvasOp` ADT for assertion |

## What Changed — Facade Wiring Status

| Abstraction | Trait created | Production impl | Test stub | Wired into consumers |
|---|---|---|---|---|
| **`EffectMath`** | n/a (pure object) | ✅ shared | n/a | ✅ `WebGLEffectRenderer` delegates all math |
| **`KeyMapper`** | n/a (pure object) | ✅ shared | n/a | ✅ `WebGLInputHandler.keyEventToChars` delegates |
| **`AnimationScheduler`** | ✅ js | ✅ `DomAnimationScheduler` | ✅ `ManualScheduler` | ✅ `CameraAnimator` accepts via constructor param |
| **`SceneBackend`** | ✅ js | ✅ `WebGLSceneBackend` | ✅ `StubSceneBackend` | ✅ **`CameraAnimator` fully wired** / ✅ **`SlideLayer` fully wired** / ✅ **`SpatialState` fully wired** / ❌ `WebGLEffectRenderer` still uses `WebGLScene` directly |
| **`CanvasFactory`** | ✅ js | ✅ `DomCanvasFactory` | ✅ `StubCanvasFactory` | ✅ **`SlideLayer` fully wired** (canvas creation + DPR) / ✅ **`SpatialState` passes through** / ❌ `WebGLEffectRenderer` still uses `dom.document.createElement` |
| **`SlideLayerMath`** | n/a (pure object) | ✅ js | n/a | ✅ Pure extraction of positioning/rotation/dimension math |

## Updated Coupling Inventory

| File | Mutable vars | Three.js types (direct) | DOM/Browser APIs (direct) | Pure logic extracted? |
|---|---|---|---|---|
| **CameraMath** | 0 | 0 | 0 | n/a (was already pure) |
| **SlideLayerMath** | 0 | 0 | 0 | ✅ Pure companion |
| **EffectMath** | 0 | 0 | 0 | ✅ Pure companion |
| **KeyMapper** | 0 | 0 | 0 | ✅ Pure function |
| **SpatialMath** | 0 | 0 | 0 | ✅ Previously extracted |
| **WebGLConfig** | 0 | 0 | 0 | n/a (was already pure) |
| **CameraAnimator** | 9 | 0 (**decoupled!**) | `dom.document.addEventListener` (zoom — guarded by `enableZoomListener`) | ✅ All Three.js ops → `SceneBackend`; rAF → `AnimationScheduler`; math → `CameraMath` |
| **SlideLayer** | 4 | 0 (when backend provided) / legacy fallback retains Three.js types | 0 (when canvasFactory provided) | ✅ **All mesh ops → `SceneBackend`; canvas → `CanvasFactory`; math → `SlideLayerMath`** |
| **SpatialState** | 4 | 0 (via `SlideLayer` + `SceneBackend`) | 0 | ✅ **Mesh/scene ops → `SceneBackend`; position logic → `SpatialMath`; `glScene` only for `Scene3DRef` wiring** |
| **WebGLEffectRenderer** | 6 | `ThreeMesh`, `ThreeCanvasTexture`, `ThreeColor`, `ThreePlaneGeometry`, `ThreeMeshBasicMaterial` | `dom.window.devicePixelRatio`, `dom.document.createElement` | ✅ Dissolve/smoke/glow/fade math → `EffectMath` |
| **WebGLScene** | 3 | `ThreeScene`, `ThreeColor`, `ThreeWebGLRenderer` | `HTMLElement`, `dom.window.addEventListener`, `dom.window.devicePixelRatio` | ❌ Still fully coupled |
| **WebGLCanvasRenderer** | 0 | 0 | `CanvasRenderingContext2D` (received as param) | ❌ No pure logic to extract |
| **WebGLInputHandler** | 0 | 0 | `HTMLCanvasElement`, `KeyboardEvent`, `MouseEvent` | ✅ Key mapping → `KeyMapper` |
| **ThreeJsTerminal** | 1 | `ThreeCanvasTexture`, `ThreeMesh`, `ThreePlaneGeometry`, `ThreeMeshBasicMaterial` | `dom.HTMLCanvasElement`, `dom.CanvasRenderingContext2D`, `dom.window.devicePixelRatio` | ❌ Orchestrator; hard to unit test directly |
| **RafTickerInterpreter** | 0 (Ref) | 0 | `dom.window.requestAnimationFrame` | ❌ Not wired to `AnimationScheduler` |
| **Scene3DRef** | 3 | 0 (uses `js.Dynamic`) | 0 | n/a (thin wrapper) |

## What Is Testable Now

### Fully testable today (no additional wiring needed)

| Component | Test approach | Coverage |
|---|---|---|
| **`EffectMath`** | Pure MUnit tests in shared | ✅ 27 tests |
| **`KeyMapper`** | Pure MUnit tests in shared | ✅ 29 tests |
| **`CameraMath`** | Pure MUnit tests in JS | ✅ 40+ tests |
| **`SpatialMath`** | Pure MUnit tests in shared | ✅ 18 tests |
| **`WebGLConfig`** | Pure MUnit tests in JS | ✅ 16 tests |
| **`CameraAnimator`** | Full loop via `StubSceneBackend` + `ManualScheduler` | ✅ 14 tests |
| **`SlideLayerMath`** | Pure MUnit tests in JS | ✅ 21 tests |
| **`SlideLayer`** | Full construction lifecycle via `StubSceneBackend` + `StubCanvasFactory` | ✅ **23 tests** |
| **`SpatialState`** | Full init/activate/dispose pipeline via stubs | ✅ **17 tests** |

### Partially testable (some logic extracted, host class still coupled)

| Component | What's testable | What's NOT testable (still coupled) |
|---|---|---|
| **`WebGLEffectRenderer`** | Per-frame math via `EffectMath` | Particle spawning, scene graph manipulation, cleanup lifecycle |
| **`WebGLInputHandler`** | Key mapping via `KeyMapper` | DOM event listener wiring, paste handling, mouse events |

### Not testable yet (requires further wiring)

| Component | Blocking dependency | What wiring enables |
|---|---|---|
| **`WebGLEffectRenderer`** (particle lifecycle) | Spawns meshes via Three.js directly, reads `dom.window.devicePixelRatio` | Wire `SceneBackend` + `CanvasFactory` → test spawn/cleanup, effect dispatch, clearEffects |
| **`RafTickerInterpreter`** | Calls `dom.window.requestAnimationFrame` directly | Skip (tick logic tested via `Ticker` trait already) |
| **`ThreeJsTerminal`** (integration) | Orchestrates all components | Test indirectly via component-level testing |

## Summary — Coverage Heat Map

```
                        BEFORE              AFTER wiring (current)
Component               (tests2.md v2)      (July 23 2026 — Priority 3)
─────────────────────── ────────────────── ──────────────────
CameraMath              ████████████ 100%  ████████████ 100%
WebGLConfig             ████████████ 100%  ████████████ 100%
SpatialMath             ████████████ 100%  ████████████ 100%
EffectMath              ████████████ 100%  ████████████ 100%
KeyMapper               ████████████ 100%  ████████████ 100%
SlideLayerMath          ████████████ 100%  ████████████ 100%
CameraAnimator          ██████████░░  80%  ██████████░░  80%
SlideLayer              ██░░░░░░░░░░  15%  ██████████░░  85%  ← WIRED via SceneBackend
SpatialState            ███░░░░░░░░░  25%  ██████████░░  80%  ← WIRED via SceneBackend
WebGLEffectRenderer     ████░░░░░░░░  30%  ████░░░░░░░░  30%
WebGLInputHandler       ████░░░░░░░░  35%  ████░░░░░░░░  35%
WebGLCanvasRenderer     ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
WebGLScene              ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
ThreeJsTerminal         ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
RafTickerInterpreter    ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
Scene3DRef              ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
ThreeJsFacade           ░░░░░░░░░░░░   0%  ░░░░░░░░░░░░   0%
```

**Legend:**
- `████` = tested    `░░░░` = untested

## Permanently Untestable (without a browser)

These files are thin integration/wiring layers or directly manipulate browser/GPU APIs with no extractable logic:

| File | Why |
|---|---|
| **`WebGLScene`** | Constructor creates Three.js renderer + appends DOM element; pure wiring |
| **`WebGLSceneBackend`** | Thin delegation from `SceneBackend` trait to Three.js types; no logic |
| **`WebGLCanvasRenderer`** | Calls Canvas 2D drawing APIs (`fillRect`, `fillText`, `stroke`); logic is trivial (parse → draw) |
| **`ThreeJsTerminal`** | Orchestrator — tests should target individual components instead |
| **`ThreeJsFacade`** | Scala.js JS interop declarations; nothing to test |
| **`DomAnimationScheduler`** | 3-line delegation to `dom.window` |
| **`DomCanvasFactory`** | 3-line delegation to `dom.document` |
| **`RafTickerInterpreter`** | rAF usage is 1 line; tick loop logic is simple; `Ticker` trait is tested elsewhere |
| **`Scene3DRef`** | Thin `js.Dynamic` wrapper; 3 mutable vars for center tracking |
| **`LoteApp` (JS)** | App entry point |
| **`TerminalPlatform` (JS)** | Platform factory wiring |

## Remaining Recommendations

**Next priority: Wire `SceneBackend` + `CanvasFactory` into `WebGLEffectRenderer` for particle lifecycle testing.** This would replace direct `new ThreeMesh(...)`, `new ThreeCanvasTexture(...)`, `dom.document.createElement("canvas")` calls in `spawnSmokeParticles` / `spawnDissolveParticles` with backend + factory calls. Once done:
- Particle spawn/cleanup lifecycle can be tested via stubs
- Effect dispatch (`dissolve`, `smoke`, `glow`, `fade`, `clearEffects`) can be verified
- No DOM or Three.js needed for the full effect pipeline

**Skip:** `RafTickerInterpreter` (tick logic is tested via `Ticker`), `WebGLScene` (pure wiring), `ThreeJsTerminal` (test via components).
