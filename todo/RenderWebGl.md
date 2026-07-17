# WebGL Slide Rendering — 3D Camera Navigation

## Overview

Slides are placed in a 3D world space. The presentation builder exposes operators for setting position (`x`, `y`, `z`) and rotation (`rotX`, `rotY`, `rotZ`) per slide. Each slide is projected onto a surface (initially a flat plane; concave or other shapes in the future). When navigating to a slide at a new coordinate, the camera moves smoothly to frame it — accelerating at first, then decelerating (ease-in-out). Slides without explicit coordinates stay at the same position and simply replace their content in-place (as today).

On the JVM/terminal backend these operators are no-ops — the presentation logic compiles and runs, but positioning is ignored.

---

## Priority 1 — 3D Slide Placement & Camera Navigation ✅ DONE

### What was implemented

- **`SlidePosition(x, y, z, rotX, rotY, rotZ)`** case class in shared API
- **`.at(x, y, z)`** and **`.rotatedBy(rx, ry, rz)`** on the slide builder DSL
- **`SlideLayer`** — one offscreen canvas + texture + mesh per unique position
- **`CameraAnimator`** — real orthographic camera movement with ease-in-out
- **`SpatialPresentationExecutorInterpreter`** — pre-renders all slides at startup, navigates by camera movement, swaps content in-place for slides sharing a position
- **`InitSpatialLayout` / `ActivateLayer` / `MoveCameraTo`** render effects for terminal ↔ executor communication
- **Automatic spatial mode detection** — `SessionBuilder` uses the spatial executor when any slide has `.at()`
- **Terminal backends** ignore position metadata (no-op)
- **Transitions** work in spatial mode (character-grid transitions: grab, morph, replace, falling characters)
- **Removed `FlipTransition` and `RotateTransition`** — these manipulated the single plane mesh which is incompatible with spatial mode. The camera navigation itself provides the visual movement between slides.

### Slide Positioning API

The `SessionBuilder` (or a new `SlideBuilder` layer) gains spatial operators:

```scala
SessionBuilder[IO]()
  .addSlide(
    SlideBuilder()
      .withContent(introSlide)
      .at(x = 0, y = 0, z = 0)              // world position
      .rotatedBy(rx = 0, ry = 0, rz = 0)    // rotation (degrees)
      .build
  )
  .addSlide(
    SlideBuilder()
      .withContent(chapter1Slide)
      .at(x = 2000, y = 0, z = -500)
      .rotatedBy(rx = 0, ry = 45, rz = 0)
      .build
  )
  .addSlide(
    SlideBuilder()
      .withContent(detailSlide)              // no .at() → stays at previous position
      .build
  )
  .build
```

**Semantics**:
- `.at(x, y, z)` — Sets the world-space position of the slide's surface.
- `.rotatedBy(rx, ry, rz)` — Sets Euler rotation (degrees) of the slide surface.
- If neither is specified, the slide inherits the position/rotation of the previous slide (i.e., in-place replacement, same behaviour as today).

**Terminal backend**: The position/rotation metadata is carried in the slide descriptor but `JLineTerminal` and `XtermTerminal` ignore it — no camera, no 3D, just content replacement.

### Slide Surface Projection

Each slide's content is rendered onto a surface mesh at the given world position and rotation:

```
SlideLayer (one per unique coordinate)
  ├── offscreenCanvas: HTMLCanvasElement      — text rendered here via Canvas2D
  ├── texture: CanvasTexture                  — GPU texture of the canvas
  ├── mesh: Mesh(PlaneGeometry, Material)     — positioned at (x, y, z) with rotation
  └── frame: Vector[String]                   — last rendered ANSI content
```

For the first implementation the surface is a flat `PlaneGeometry`. Later iterations could use:
- Concave (curved inward, like a cinema screen)
- Cylindrical (slides wrap around the viewer)
- Spherical (planetarium-style)
- Arbitrary meshes

### Camera Movement

When the session advances to a slide with a **new** coordinate:

1. **Compute target camera pose** — position the camera so the target slide's surface fills the viewport (matching orientation).
2. **Compute distance** — Euclidean distance between current camera position and target.
3. **Animate with ease-in-out** — The camera accelerates then decelerates:
   - Use a cubic or quintic ease function: `t³` ramp-up, `(1-t)³` ramp-down.
   - **Speed scales with distance**: Longer distances → higher peak speed, so travel time grows sub-linearly (e.g., `duration = baseMs + k * sqrt(distance)`). Short hops feel snappy; long jumps don't drag.
4. **During animation** — Both the departing slide's surface and the arriving slide's surface are visible in the scene (they're just meshes at different positions). The camera flying between them creates the visual transition.
5. **On arrival** — Camera settles exactly at the framing pose. Input is re-enabled.

When the session advances to a slide **without** a coordinate (inherits previous position):
- No camera movement.
- The content on the current surface is replaced in-place (existing row-diff rendering).

### Easing Formula (Sketch)

```scala
/** Attempt: smoothstep with distance-adaptive duration. */
def transitionDuration(distance: Double): FiniteDuration = {
  val baseMs  = 300
  val kFactor = 0.8
  (baseMs + kFactor * Math.sqrt(distance)).millis
}

def easeInOut(t: Double): Double = {
  // cubic Hermite (smoothstep)
  val clamped = t.max(0.0).min(1.0)
  clamped * clamped * (3 - 2 * clamped)
}
```

### Implementation Plan

| Step | Description | Files |
|------|-------------|-------|
| 1 | Add `SlidePosition(x, y, z, rx, ry, rz)` case class in shared | `lote/shared/…/api/SlidePosition.scala` (new) |
| 2 | Extend slide descriptor / builder to carry optional position | `SlideBuilder.scala`, `SessionBuilder.scala` |
| 3 | Terminal backends ignore position (no-op) | `JLineTerminal.scala`, `XtermTerminal.scala` — no changes needed |
| 4 | `WebGLScene` manages multiple positioned meshes | `WebGLScene.scala` |
| 5 | New `SlideLayer` class (canvas + texture + mesh) | `SlideLayer.scala` (new) |
| 6 | Camera animation system (ease-in-out, distance-adaptive) | `CameraAnimator.scala` (new) |
| 7 | `ThreeJsTerminal` wires position metadata into scene + animator | `ThreeJsTerminal.scala` |
| 8 | Content rendering targets the active `SlideLayer` | `WebGLCanvasRenderer.scala` |

### Landscape3DSlide Integration — DEFERRED (separate PR)

With slides living in a shared 3D world, the `Landscape3DSlide` no longer needs to create its own isolated Three.js scene, renderer, and camera. Instead it can be a first-class citizen of the presentation's 3D space:

- **Today**: `Landscape3DSlide` spins up an independent `WebGLRenderer`, appends its own `<canvas>` to the DOM, and manages its own camera. It's effectively a separate Three.js application overlaid on top of the terminal.
- **After this change**: The landscape geometry (terrain, castles, trees, figures, atmospheric effects) is added directly to the shared `WebGLScene`. The presentation camera navigates *into* the landscape the same way it navigates to any other slide — flying from the previous slide's position to the landscape's world coordinates.

**Benefits**:
- The transition into and out of the landscape is a smooth camera flight — no jarring overlay swap.
- The landscape can be visible in the distance from other slides (parallax / world continuity).
- Only one WebGL context is active (avoids multi-context overhead and z-fighting between canvases).
- Camera controls (A/D/W/S) during the landscape slide simply adjust the shared camera within bounds, rather than managing a separate camera instance.

**What changes**:
- `Landscape3DSlide` stops creating its own renderer/scene/camera. Instead it receives a reference to the shared scene and adds its meshes there.
- A new slide trait method (or capability) lets a slide declare "I manage 3D scene objects" so the framework knows to hand it the scene reference.
- On `stopShow`, the landscape removes its meshes from the shared scene (or they remain visible as the camera flies away — a design choice).

**Sketch**:
```scala
SessionBuilder[IO]()
  .addSlide(
    SlideBuilder()
      .withContent(introSlide)
      .at(x = 0, y = 0, z = 0)
      .build
  )
  .addSlide(
    SlideBuilder()
      .withContent(Landscape3DSlide.contextual())   // 3D geometry slide
      .at(x = 5000, y = 0, z = -2000)              // camera flies here
      .rotatedBy(rx = 0, ry = 30, rz = 0)
      .build
  )
  .addSlide(
    SlideBuilder()
      .withContent(conclusionSlide)
      .at(x = 0, y = 0, z = -4000)                 // camera flies onward
      .build
  )
  .build
```

The landscape becomes just another positioned slide — but instead of rendering text onto a flat surface, it populates the scene with 3D geometry at its assigned coordinates.

---

## Priority 2 — Dirty-Rect Texture Updates

**Problem**: When a single character changes (e.g., cursor blink), the entire row is re-rendered and the full texture is re-uploaded to the GPU.

**Proposal**: Use `WebGLRenderer.copyTextureToTexture()` to upload only the changed rectangular region:

```scala
// Instead of:
texture.needsUpdate = true   // re-uploads entire texture

// Do:
renderer.copyTextureToTexture(
  position = new Vector2(dirtyX, dirtyY),
  srcTexture = patchTexture,   // small canvas with just the changed region
  dstTexture = texture
)
```

**Files affected**:
- `WebGLCanvasRenderer.scala` — return dirty rect bounds from `renderLine()`
- `WebGLScene.scala` / `SlideLayer.scala` — expose partial texture update method
- `ThreeJsTerminal.scala` — use partial updates when only a small region changed

---

## Priority 3 — GPU Text Rendering with SDF Fonts

**Problem**: `ctx.fillText()` re-rasterizes every character on every update. At high DPI or large canvas sizes this becomes expensive.

**Proposal**: Use MSDF (Multi-channel Signed Distance Field) font atlases:

1. Pre-generate an MSDF atlas for the monospace font (e.g., using `msdf-atlas-gen`).
2. Create a custom `ShaderMaterial` with vertex/fragment shaders that sample the atlas.
3. Each character cell becomes a small quad with UV coordinates into the atlas.
4. Color, bold, italic, underline are passed as per-instance attributes.

**Benefits**:
- Resolution-independent — sharp at any zoom level, no re-rasterization.
- GPU-parallel — thousands of characters rendered in one draw call via instancing.
- Enables per-character animation (wave, typewriter, jitter) cheaply.

**Incremental path**: The `SlideLayer` abstraction hides whether rendering uses Canvas2D or SDF — swap the implementation without affecting `ThreeJsTerminal`.

---

## Open Questions

- **Surface shape API**: How should non-flat surfaces be specified? Per-slide, or a global "world shape" setting on the session?
- **Font atlas generation**: Pre-generated at build time, or generated at runtime on first load?
- **Layer lifecycle**: When the camera is far from a slide, should its texture be disposed to save GPU memory? (LOD / culling)
- **Rotation interpolation**: Use quaternion slerp to avoid gimbal lock when rotating between slides with very different orientations?
- **Overlays during camera motion**: Should persistent overlays (e.g., corner labels) stay screen-fixed or move with the camera?
- **Fallback for no WebGL**: If WebGL isn't available, should the Three.js terminal degrade to xterm.js automatically?
