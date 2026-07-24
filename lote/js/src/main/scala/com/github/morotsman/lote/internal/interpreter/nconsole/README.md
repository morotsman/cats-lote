# nconsole – WebGL Terminal Renderer

This package implements a **Three.js / WebGL-backed terminal** that renders
slides as positioned meshes in 3-D space (spatial mode) with a perspective
camera and animated fly-through transitions between slides.

---

## Package Structure

```
nconsole/
├── ThreeJsTerminal.scala              ← top-level aggregator (Terminal algebra impl)
│
├── facade/
│   └── ThreeJsFacade.scala            ← Three.js Scala.js type definitions
│
├── scene/
│   ├── SceneBackend.scala             ← abstract trait over the scene graph
│   ├── WebGLScene.scala               ← Three.js scene + renderer management
│   ├── WebGLSceneBackend.scala        ← production SceneBackend impl
│   ├── AnimationScheduler.scala       ← abstract animation frame scheduling
│   └── DomAnimationScheduler.scala    ← production impl (requestAnimationFrame)
│
├── camera/
│   ├── CameraAnimator.scala           ← animates camera between slide positions
│   └── CameraMath.scala               ← pure math for camera positioning
│
├── canvas/
│   ├── CanvasFactory.scala            ← abstract canvas creation trait
│   ├── DomCanvasFactory.scala         ← production impl (HTMLCanvasElement)
│   └── WebGLCanvasRenderer.scala      ← renders ANSI-styled text to 2D canvas
│
├── spatial/
│   ├── SlideLayer.scala               ← single slide's rendering surface
│   ├── SlideLayerMath.scala           ← pure math for slide grid layout
│   └── SpatialState.scala             ← mutable spatial-mode state management
│
├── effects/
│   └── WebGLEffectRenderer.scala      ← GPU visual effects (dissolve, smoke, glow, fade)
│
└── input/
    └── WebGLInputHandler.scala        ← bridges DOM events into effect queue
```

| Sub-package | Cohesion principle |
|---|---|
| **facade** | Isolates raw JS interop types — changes only when Three.js API changes |
| **scene** | Scene graph infrastructure + animation scheduling (render-loop plumbing) |
| **camera** | Self-contained camera math & animation — used by spatial + effects |
| **canvas** | 2D canvas abstraction & text rendering — orthogonal to 3D scene |
| **spatial** | Slide positioning/layout in 3D space — a distinct domain concept |
| **effects** | Visual transition effects |
| **input** | DOM event handling |

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      ThreeJsTerminal                            │
│         (main orchestrator / Terminal algebra impl)             │
└───────┬──────────┬──────────────┬───────────────┬──────────────┘
        │          │              │               │
        ▼          ▼              ▼               ▼
  scene/        camera/       effects/        input/
  WebGLScene    CameraAnimator  WebGLEffect   WebGLInput
  SceneBackend  CameraMath      Renderer      Handler
        │          │              │
        ▼          ▼              ▼
  ┌──────────────────────────────────────┐
  │       facade/ThreeJsFacade.scala     │
  │       (Three.js Scala.js bindings)  │
  └──────────────────────────────────────┘
        │
        ▼
  ┌──────────────────────────────────────┐
  │  spatial/SlideLayer                  │
  │  canvas/WebGLCanvasRenderer          │
  │  (per-slide canvas + ANSI text)     │
  └──────────────────────────────────────┘
```

---

## Components

### `ThreeJsTerminal.scala` – Main Orchestrator

The entry point that implements the `Terminal` algebra (from the public API).
It assembles all sub-package components. Responsibilities:

- Bootstraps the WebGL renderer and DOM canvas.
- Delegates frame rendering to `canvas/WebGLCanvasRenderer`.
- On `InitSpatialLayout`, creates positioned `SlideLayer` meshes and hands
  camera control to `camera/CameraAnimator`.
- Coordinates transitions and effects through `camera/CameraAnimator` and
  `effects/WebGLEffectRenderer`.

### `scene/` – Scene Graph Infrastructure

- **`SceneBackend`** — Abstract trait over the Three.js scene graph. Decouples
  rendering logic from the browser environment. In production, backed by
  `WebGLSceneBackend`; in tests, by `StubSceneBackend`.
- **`WebGLScene`** — Central state container owning the `THREE.Scene`,
  `THREE.WebGLRenderer`, viewport dimensions, and resize handling.
- **`WebGLSceneBackend`** — Production `SceneBackend` impl delegating to
  real Three.js objects.
- **`AnimationScheduler`** — Abstract trait over `requestAnimationFrame` /
  `cancelAnimationFrame` / `performance.now()` for testability.
- **`DomAnimationScheduler`** — Production impl delegating to real DOM APIs.

### `camera/` – 3-D Camera Control

- **`CameraAnimator`** — Manages the perspective camera: computes the camera
  distance that frames each slide, animates fly-throughs using ease-in-out
  interpolation, and handles interactive zoom (`+`/`-`/`0`) via FOV adjustment.
- **`CameraMath`** — Pure math utilities (rotation matrix, interpolation,
  easing, transition duration) extracted for unit testing without DOM/Three.js.

### `canvas/` – 2D Canvas Abstraction & Text Rendering

- **`CanvasFactory`** — Abstract trait over DOM canvas creation. Decouples
  `SlideLayer` from `document.createElement("canvas")`.
- **`DomCanvasFactory`** — Production impl creating real `<canvas>` elements.
- **`WebGLCanvasRenderer`** — Stateless Canvas2D renderer that converts
  ANSI-styled terminal content into styled text. Handles 256-color and
  true-color foreground/background, bold, italic, underline, and reset.

### `spatial/` – Slide Layout in 3D Space

- **`SlideLayer`** — Each slide's rendering surface: owns an offscreen canvas,
  a `THREE.Texture` backed by it, and a `THREE.Mesh` positioned/rotated in
  world space.
- **`SlideLayerMath`** — Pure math for grid dimensions and mesh positioning.
- **`SpatialState`** — Manages mutable spatial-mode state: slide layers,
  slide-to-layer mapping, active layer index, and the shared `Scene3DRef`.

### `effects/WebGLEffectRenderer.scala` – Visual Effects

Applies GPU-accelerated transition effects to the active slide layer:
dissolve, smoke particles, glow, and fade. Operates on the active layer's
mesh and material directly.

### `input/WebGLInputHandler.scala` – Input Bridge

Captures DOM events (keyboard, mouse, paste) from the renderer's `<canvas>`
element and translates them into JLine/xterm-compatible character sequences.

### `facade/ThreeJsFacade.scala` – Three.js Scala.js Bindings

Low-level `@js.native` facades exposing Three.js types to Scala.js:
`THREE.Scene`, `THREE.PerspectiveCamera`, `THREE.Mesh`, `THREE.PlaneGeometry`,
`THREE.MeshBasicMaterial`, `THREE.Texture`, `THREE.WebGLRenderer`,
`THREE.Vector3`, `THREE.Color`, and typed particle user data.

---

## Testability

The package uses trait-based abstractions to enable unit testing without
DOM or Three.js:

| Abstraction | Production | Test stub |
|---|---|---|
| `scene/SceneBackend` | `scene/WebGLSceneBackend` | `StubSceneBackend` |
| `canvas/CanvasFactory` | `canvas/DomCanvasFactory` | `StubCanvasFactory` |
| `scene/AnimationScheduler` | `scene/DomAnimationScheduler` | `ManualScheduler` |

Test stubs record operations in observable logs, allowing assertions on
the exact sequence of scene graph manipulations without GPU or browser.

---

## Three.js Concepts

This section explains the core Three.js terms used throughout the codebase.

### Scene

A `THREE.Scene` is the root container for all 3-D objects. Everything that
should be rendered (meshes, lights, particle systems) must be added to the
scene. Think of it as the "world" that the camera looks into.

### Camera

A camera defines the viewpoint from which the scene is rendered.

- **Perspective camera** — Mimics how a human eye sees: objects farther away
  appear smaller (foreshortening). Defined by a vertical field-of-view (FOV),
  aspect ratio, and near/far clipping planes. This is what we use.

### Renderer

`THREE.WebGLRenderer` takes a scene + camera and draws pixels onto an HTML
`<canvas>` element using the GPU via WebGL. Calling `renderer.render(scene, camera)`
produces one frame.

### Mesh

A `THREE.Mesh` is a visible object in the scene, composed of:

- **Geometry** — The shape (vertices and faces). We use `PlaneGeometry`
  (a flat rectangle) for each slide.
- **Material** — How the surface looks. We use `MeshBasicMaterial` with a
  texture map (no lighting needed for 2-D text content).

### Texture

A `THREE.Texture` wraps an image source (in our case, an offscreen
`<canvas>` element) and maps it onto a mesh's material. When the canvas
content changes, setting `texture.needsUpdate = true` tells the renderer to
re-upload the pixels to the GPU on the next frame.

### Plane Geometry

`THREE.PlaneGeometry(width, height)` creates a flat rectangular surface.
Each slide is a plane positioned and rotated in world space.

### Position, Rotation, Scale

Every object in Three.js has a `position` (x, y, z), `rotation` (Euler
angles), and `scale` (multiplier per axis). These determine where and how
the object appears in the scene.

### Field of View (FOV)

The vertical angle (in degrees) that the perspective camera can see. A
smaller FOV zooms in (telephoto effect); a larger FOV zooms out (wide-angle).
We use FOV adjustment for interactive zoom rather than moving the camera.

### lookAt

`camera.lookAt(x, y, z)` orients the camera to face a specific world-space
point. Combined with `camera.up` (which direction is "up"), this fully
defines the camera's orientation.

### Up Vector

The `camera.up` vector tells Three.js which direction is "up" for the camera.
Normally `(0, 1, 0)` (Y-up), but when slides are rotated the up vector must
be rotated to match, so the text appears right-side-up.

### requestAnimationFrame

A browser API that schedules a callback before the next screen repaint
(~60 fps). Used by `CameraAnimator` to drive smooth camera transitions
frame by frame.

---

## Data Flow

1. **Input** → `WebGLInputHandler` captures keystrokes and feeds them to the
   presentation state machine (outside this package).

2. **State update** → `ThreeJsTerminal` receives a new frame (ANSI string) or
   a navigation command.

3. **Rendering** → `WebGLCanvasRenderer` paints styled text onto the active
   `SlideLayer`'s offscreen canvas.

4. **Scene update** → The layer's texture is marked dirty; `WebGLScene.render()`
   asks Three.js to draw the scene.

5. **Camera animation** → On slide change, `CameraAnimator` kicks off a
   `requestAnimationFrame` loop that interpolates the camera pose and
   re-renders each frame until settled.

6. **Effects** → `WebGLEffectRenderer` may overlay particle systems or
   manipulate layer opacity/scale during transitions.

7. **Floating Characters** → `GlideLayer` (from the shared support library)
   dispatches `RenderEffect.RenderFloatingChars` to `NConsole.applyEffect`.
   These are rendered on a transparent layer above the slide grid at sub-pixel
   positions, enabling smooth motion for animated elements (bugs, snakes,
   transition edge chars) between discrete simulation steps.

---

## Platform Strategy – Abstracting Platform-Specific Rendering

The `PresentationExecutorInterpreter` delegates all platform-specific
rendering concerns to a **`PlatformStrategy[F[_]]`** trait (defined in
`internal.algebra`). This keeps the executor focused on navigation state,
user input dispatch, and fiber lifecycle, while each platform provides its
own implementation of spatial layout, layer management, and camera control.

### `PlatformStrategy[F[_]]` trait

```scala
trait PlatformStrategy[F[_]] {
  def setupPlatform(): F[Unit]         // one-time init at presentation start
  def activateSlide(index: Int): F[Unit]  // prepare rendering target for a slide
  def navigateToSlide(index: Int): F[Unit] // animated navigation to slide position
}
```

### Implementations

| Implementation | Platform | Behavior |
|----------------|----------|----------|
| `TerminalPlatformStrategy` | JVM (JLine) | All operations are **no-ops**. Terminal slides are rendered in-place without spatial concepts. |
| `SpatialPlatformStrategy` | JS (WebGL) | Manages 3-D spatial layout, layer activation via `RenderEffect.ActivateLayer`, and interruptible camera flights via `RenderEffect.MoveCameraTo` / `JumpCameraTo`. |

The strategy is **auto-selected** inside `PresentationExecutorInterpreter.make`
based on `NConsole.capabilities`: if `Transforms3D` is present, the spatial
strategy is used; otherwise the terminal strategy.

### Executor Startup Flow

The startup sequence in `PresentationExecutorInterpreter.start()` is now:

```
strategy.setupPlatform()  >>  executionLoop()
```

On each slide switch within the execution loop:

```
strategy.activateSlide(index)  >>  writeContent  >>  strategy.navigateToSlide(index)
```

---

## Startup Rendering – "Render All Slides at Startup" (WebGL)

When a presentation starts in WebGL mode, **every unique slide position gets
its first slide's content pre-rendered before the user sees anything**. This
ensures that all slide planes are visible with content as soon as the 3-D
scene appears, rather than showing blank rectangles that fill in lazily.

This logic lives inside `SpatialPlatformStrategy.setupPlatform()`, which
performs two phases:

### Phase 1 — Spatial Layout Initialization

Collects every slide's `Option[SlidePosition]` into a `Vector` and fires
`RenderEffect.InitSpatialLayout(positions)`, which is handled by
`ThreeJsTerminal.initSpatialMode()`. This method performs four steps:

| Step | Description |
|------|-------------|
| **1. Resolve positions** | Slides with `None` inherit the previous slide's position; the first slide defaults to `SlidePosition(0, 0, 0)`. |
| **2. Deduplicate by position** | Groups slides by their full 6-DOF key `(x, y, z, rotX, rotY, rotZ)`. Builds a `slideToLayerIndex` mapping so that multiple slides sharing the same position point to a **single** `SlideLayer` — no duplicate layers are created. For example, if slides 0, 3, and 5 all have position `(100, 0, 0, 0, 0, 0)`, they all map to the same layer index and share one offscreen canvas and one mesh. |
| **3. Create `SlideLayer` instances** | Creates exactly **one `SlideLayer` per unique position** (not one per slide). Each layer gets its own offscreen canvas, Three.js texture, and plane mesh, which is added to the scene via `glScene.scene.add(layer.mesh)`. |
| **4. Configure camera + `Scene3DRef`** | Sets up the perspective camera via `CameraAnimator.initSpatialMode()` and creates the shared `Scene3DRef` so scene-aware slides can inject custom 3-D objects. |

At this point all slide planes exist in the 3-D scene but their canvases are
blank (black or transparent).

### Phase 2 — Pre-Render First Slide Per Position

Iterates over **all** slide specs. For each slide that is the first at its
unique position:

1. **`ActivateLayer(idx)`** — sets that layer as the current write target.
2. **`slide.content`** — evaluates the slide's content to obtain an ANSI string.
3. **`write(content)`** — renders the ANSI text onto the layer's offscreen
   canvas via `WebGLCanvasRenderer`, marks the Three.js texture as dirty
   (`needsUpdate = true`), and triggers `glScene.render()`.

After this phase completes, every visible slide plane in the 3-D scene has
its text content rendered and uploaded to the GPU.

### Phase 3 — `executionLoop()`

Normal interactive navigation begins. On each slide switch, the executor
calls `strategy.activateSlide(index)` to select the WebGL layer, renders the
slide content, then calls `strategy.navigateToSlide(index)` which triggers an
interruptible camera flight to the slide's 3-D position. If the user presses
a key during the flight, the camera jumps instantly to the target.

### Visual Summary

```
AdvancedWebGLExample.run()
  └─► PresentationExecutor.start()
        │
        ├─► strategy.setupPlatform()   (SpatialPlatformStrategy)
        │     │
        │     ├─► InitSpatialLayout(positions)
        │     │     └─► ThreeJsTerminal.initSpatialMode()
        │     │           ├─ resolve positions (inherit from previous)
        │     │           ├─ deduplicate by 6-DOF key
        │     │           ├─ create SlideLayer per unique position
        │     │           │    └─ offscreen canvas + texture + mesh → scene.add(mesh)
        │     │           └─ configure camera + Scene3DRef
        │     │
        │     └─► pre-render first slide per position
        │           └─ for each first-at-position slide:
        │                ├─ ActivateLayer(idx)
        │                ├─ slide.content → ANSI string
        │                └─ write(content) → canvas render → texture upload → scene.render()
        │
        └─► executionLoop()
              └─ on each slide switch:
                   ├─ strategy.activateSlide(index) → ActivateLayer
                   ├─ writeContent → ANSI rendering
                   └─ strategy.navigateToSlide(index) → MoveCameraTo (interruptible)
```

### Key Files

| File | Role |
|------|------|
| `PlatformStrategy.scala` (shared, `internal.algebra`) | Trait defining `setupPlatform`, `activateSlide`, `navigateToSlide` |
| `SpatialPlatformStrategy.scala` (shared) | WebGL implementation: spatial layout init, layer activation, camera navigation |
| `TerminalPlatformStrategy.scala` (shared) | Terminal implementation: all no-ops |
| `PresentationExecutorInterpreter.scala` (shared) | Platform-agnostic executor that delegates to `PlatformStrategy` |
| `ThreeJsTerminal.scala` | Handles `InitSpatialLayout` / `ActivateLayer` / `MoveCameraTo` effects; contains `write()` |
| `spatial/SlideLayer.scala` | Per-position rendering surface: offscreen canvas, texture, and mesh |
| `spatial/SpatialState.scala` | Manages slide layers, deduplication, active layer, and Scene3DRef |
| `scene/WebGLScene.scala` | Owns the Three.js scene, renderer, and `render()` method |
| `canvas/WebGLCanvasRenderer.scala` | Paints ANSI-styled text onto a `SlideLayer`'s offscreen canvas |
| `camera/CameraAnimator.scala` | Perspective camera animation and zoom control |
| `effects/WebGLEffectRenderer.scala` | GPU-accelerated dissolve, smoke, glow, fade effects |
| `input/WebGLInputHandler.scala` | DOM event capture and JLine-compatible encoding |

---

## Spatial Mode

All presentations always run in spatial mode. Each slide gets a world-space
position via `SlidePosition(x, y, z, rotX, rotY, rotZ)`:

- Slides without an explicit position inherit from the previous slide.
- If **no** slide defines a position, they all default to `SlidePosition(0, 0, 0)`,
  meaning they stack at the origin and the camera stays put (behaving like a
  simple full-screen terminal).
- Multiple slides at the same position share a **single `SlideLayer`** (one
  offscreen canvas, one texture, one mesh). No extra layers are created for
  co-located slides. When the user navigates to a different slide at the same
  position, `strategy.activateSlide(index)` points to the shared layer and
  the new slide's content overwrites the previous content on that canvas —
  only one slide's text is visible on a given layer at a time.

This uniform approach eliminates the need for separate rendering modes or
code paths for "simple" vs "3-D" presentations.
