# Alternative Platform Strategies

Currently we have two platform strategies:

- **Terminal** — renders slides sequentially in-place (JLine / xterm.js); ignores position metadata entirely.
- **Spatial (WebGL)** — places slides as surfaces in 3D space; a perspective camera flies between them.

Below are ideas for a third kind of strategy that is fundamentally different from both.

---

## 1. Scroll Strategy (Scrollytelling)

All slides rendered into a single continuous scrollable document. Content animates in/out based on scroll position rather than discrete key-press transitions.

- Slides become "sections" with trigger-based reveals (fade, pin-and-release, parallax).
- Transition progress is driven by scroll position, not time.
- Works naturally on mobile/touch.
- Implementation: a `ScrollPlatformStrategy` that renders all slides into a tall DOM container and uses `IntersectionObserver` or scroll-percentage to activate/deactivate sections.

---

## 2. Morph / Shared-Element Strategy

Instead of moving a camera or replacing content in-place, individual *elements* animate between their positions across slides (like Android shared-element transitions or Keynote's Magic Move).

- The system diffs consecutive slides, identifies matching elements (by key/id), and tweens position+style.
- Unmatched elements fade out/in.
- Purely 2D — no camera, no coordinate system — the "transition" is the content reshaping itself.
- Implementation: a `MorphPlatformStrategy` with element-level diffing and per-element CSS/canvas tweens.

---

## 3. Infinite Canvas + Semantic Zoom (2D Prezi-like)

A 2D infinite canvas where slides exist at different *zoom levels* rather than positions in 3D. Navigation is pan + scale (not a perspective camera).

- No perspective distortion — pure 2D affine transforms (translate + uniform scale).
- Slides can be nested: zoom into a bullet point and it expands into a full sub-slide.
- Much simpler math than 3D; works without WebGL (plain CSS transforms or Canvas2D).
- Implementation: a `ZoomCanvasPlatformStrategy` with viewport state = `{x, y, scale}`.

---

## 4. Notebook / Append-Only Strategy

Slides rendered as an append-only stream (like a Jupyter notebook or chat transcript). Previous slides remain visible above; new slides appear below.

- Never removes content — only appends.
- Typing/streaming animation for new content arrival.
- Natural for code-heavy or live-demo presentations.
- Could include interactive cells (run code, show output inline).
- Implementation: a `NotebookPlatformStrategy` that appends to a growing DOM/terminal buffer.

---

## Comparison

| Strategy | Transition metaphor | Requires WebGL | Previous slides visible | Best for |
|----------|-------------------|----------------|------------------------|----------|
| Terminal | Replace in-place | No | No | CLI demos, simplicity |
| Spatial | Camera flies through 3D | Yes | Yes (as surfaces) | Visual wow, spatial narrative |
| Scroll | Scroll reveals | No | Yes (above) | Storytelling, mobile |
| Morph | Elements reshape | No | No (they transform) | Design-focused, keynotes |
| Zoom Canvas | Pan + scale 2D | No | Yes (zoomed out) | Hierarchical content, maps |
| Notebook | Append below | No | Yes (above) | Code talks, workshops |


---

## Alternative Rendering Media

Currently the system supports two rendering backends (implementations of `Terminal[F]`):

- **ANSI terminal** (JLine on JVM, xterm.js on browser) — character-grid rendering with ANSI escape codes.
- **WebGL** (Three.js on browser) — GPU-accelerated rendering with sub-pixel positioning, particle effects, and 3D transforms.

These are the physical output targets. Below are other conceivable rendering media.

---

### 1. SVG Renderer

Render slides as SVG documents. Each character cell becomes a `<text>` element (or `<tspan>`); effects become SVG animations (`<animate>`, filters, transforms).

**Advantages:**
- Resolution-independent, perfect for export (PDF, print, embedding in docs).
- Accessibility: real text nodes, searchable, screen-reader friendly.
- No runtime dependency on WebGL or a terminal emulator.
- Can produce a static HTML file (one SVG per slide) for offline viewing.

**Use cases:** Export to PDF, embed presentations in documentation, accessibility-first rendering.

**Implementation surface:**
- New `SvgTerminal[F]` implementing `Terminal[F]`.
- `write()` appends `<text>` elements to an SVG DOM or string buffer.
- `size` returns a fixed viewport (configurable).
- Effects map to SVG filters (blur, opacity) or SMIL/CSS animations.
- Could run on both JVM (string-building for export) and JS (live DOM SVG).

---

### 2. Canvas 2D Renderer

Render to an HTML5 `<canvas>` element using the 2D context (`CanvasRenderingContext2D`). Text is drawn via `fillText`; effects use compositing operations and pixel manipulation.

**Advantages:**
- No WebGL required — much broader browser support (including older devices, Safari quirks).
- Sub-pixel text positioning and custom fonts without the complexity of Three.js.
- Lightweight: no scene graph, no shaders, no 3D math.
- Good enough for most visual effects (fade, dissolve, slide-in) via alpha compositing.

**Use cases:** Browser presentations on low-end devices, simpler deployment, reduced bundle size.

**Implementation surface:**
- New `Canvas2DTerminal[F]` (JS-only).
- `write()` calls `ctx.fillText()` at computed pixel positions.
- Effects use `globalAlpha`, `globalCompositeOperation`, or off-screen canvas blending.
- Could support the Zoom Canvas platform strategy natively (just transform the canvas context).

---

### 3. Image/GIF Renderer (Static Export)

Render each slide (or the full presentation as an animated sequence) to raster images — PNG per slide, or an animated GIF/APNG/WebP for transitions.

**Advantages:**
- Share presentations as images (social media, README badges, documentation).
- No runtime needed — the output is a file.
- Great for CI pipelines (generate presentation screenshots automatically).

**Use cases:** README previews, social sharing, visual regression testing, thumbnails.

**Implementation surface:**
- JVM-only `ImageTerminal[F]` using Java2D (`BufferedImage` + `Graphics2D`).
- `write()` renders text with `drawString()` using a monospace font.
- After each slide, export the `BufferedImage` as PNG.
- For animated GIF: render each frame during transitions and encode with a GIF library.
- Could also run headlessly in CI for snapshot testing (complementing the Playwright e2e tests).

---

### 4. PDF Renderer

Render slides directly to a PDF document — one page per slide, with proper text (not rasterized).

**Advantages:**
- Industry-standard format for sharing slide decks.
- Searchable text, hyperlinks, vector graphics.
- Print-friendly.

**Use cases:** Export for distribution, handouts, archival.

**Implementation surface:**
- JVM-only using a library like Apache PDFBox or iText.
- New `PdfTerminal[F]` that accumulates drawing commands per page.
- `write()` places text at computed positions using a monospace font.
- `clear()` finalizes the current page and starts a new one.
- Colors map to PDF color space; effects are mostly skipped (or rendered as static snapshots).

---

### 5. Audio / Speech Renderer

Generate audio narration from slide content using text-to-speech. Not a visual medium — it's a complementary output channel.

**Advantages:**
- Accessibility: vision-impaired users can "listen" to the presentation.
- Podcast-style export of presentation content.
- Could run alongside any visual renderer as a secondary output.

**Use cases:** Accessibility, auto-generated narration, podcast export.

**Implementation surface:**
- JVM: Java Speech API or an external TTS engine (e.g., `say` on macOS, `espeak` on Linux).
- JS: Web Speech API (`SpeechSynthesis`).
- Not a `Terminal[F]` replacement — more of a parallel output sink. Could be a decorator/listener that observes `writeString` calls and queues them for speech.

---

### 6. Remote / Network Renderer

Stream the presentation to a remote display — another browser tab, a connected device, or a projector with its own rendering engine.

**Advantages:**
- Presenter sees notes on their screen; audience sees slides on a second screen.
- Collaborative viewing: multiple audience members connect to a shared session.
- Could enable "live slides" where remote viewers see real-time updates.

**Use cases:** Speaker mode, multi-screen setups, collaborative presentations.

**Implementation surface:**
- A `RemoteTerminal[F]` that serializes write commands and sends them over WebSocket.
- Receiving end runs any other renderer (WebGL, Canvas2D, terminal).
- Need a small protocol: `{type: "write", text: "...", pos: [x,y]}`, `{type: "clear"}`, `{type: "effect", ...}`.
- The `PresentationExecutorInterpreter` doesn't need to change — it just writes to the terminal, which happens to be remote.

---

### 7. Braille / Tactile Renderer

Render slides to a Braille display or tactile graphics device.

**Advantages:**
- True accessibility for blind users (beyond screen-reader text).
- Maps naturally to the character-grid model (Braille cells are grid-based).

**Use cases:** Accessibility, assistive technology integration.

**Implementation surface:**
- Map each character cell to Braille Unicode characters (or drive a hardware Braille display via BRLAPI).
- Simplify colors to emphasis levels (bold = raised dots pattern).
- Likely a thin adapter on top of the existing ANSI terminal logic.

---

### Comparison of Rendering Media

| Medium | Platform | Interactive | Exportable | Requires GPU | Best for |
|--------|----------|-------------|------------|--------------|----------|
| ANSI Terminal | JVM + JS | Yes | No | No | CLI, SSH |
| WebGL | JS | Yes | No | Yes | Rich visuals, 3D |
| SVG | JVM + JS | Yes (in browser) | Yes | No | Export, accessibility |
| Canvas 2D | JS | Yes | No | No | Lightweight browser |
| Image/GIF | JVM | No | Yes | No | Sharing, CI |
| PDF | JVM | No | Yes | No | Distribution, print |
| Audio/TTS | JVM + JS | No | Yes | No | Accessibility |
| Remote/Network | JVM + JS | Yes (remote) | No | No | Multi-screen, collab |
| Braille | JVM | Yes | No | No | Assistive tech |

### Implementation priority suggestion

1. **Canvas 2D** — immediate value as a lighter alternative to WebGL in the browser; can support most effects without shaders.
2. **SVG** — enables export and accessibility with relatively little code; reuses character-grid mental model.
3. **Image/GIF** — useful for CI/testing and README previews; JVM-only, well-understood libraries.
4. **PDF** — clear user demand for "export my deck"; builds on the same positioning logic as Image.
5. **Remote** — interesting for speaker-mode UX but higher infrastructure complexity.
6. **Audio** — nice-to-have accessibility complement; can be added as a decorator without touching core rendering.

### What exists today

The `PlatformStrategy[F[_]]` trait (in `internal/algebra/`) defines three methods:

```scala
trait PlatformStrategy[F[_]] {
  def setupPlatform(): F[Unit]
  def activateSlide(index: Int): F[Unit]
  def navigateToSlide(index: Int): F[Unit]
}
```

The `PresentationExecutorInterpreter` auto-selects a strategy based on backend capabilities (`Transforms3D` → Spatial, otherwise Terminal). The strategy is called at three points in the execution loop:

1. **`setupPlatform()`** — once at presentation start.
2. **`activateSlide(index)`** — before rendering content to a slide (selects the render target).
3. **`navigateToSlide(index)`** — after content is ready, animates the viewport transition.

### What each new strategy would require

#### Scroll Strategy

| Layer | Changes |
|-------|---------|
| **API** | New `ScrollConfig` case class (section height, pin duration, parallax factor, reveal animation type). Optional — could reuse existing slide definitions as-is. |
| **PlatformStrategy impl** | `ScrollPlatformStrategy` (JS-only). `setupPlatform` renders ALL slides into a tall scrollable `<div>` container (one section per slide). `activateSlide` is a no-op (all slides are pre-rendered). `navigateToSlide` calls `element.scrollIntoView()` or sets `scrollTop`. |
| **New code** | ~1 new file: the strategy class. A scroll-position listener that fires `activateSlide` callbacks as sections enter/leave the viewport. |
| **Executor changes** | Minimal. Need to suppress keyboard-based navigation (or re-map to scroll-to-section). The executor currently calls `navigateToSlide` on arrow key — that still works, it just scrolls instead of flying. |
| **Capability detection** | New capability marker (e.g. `ScrollableContainer`) or explicit user opt-in via config. |
| **Effort estimate** | Small–medium. Most complexity is CSS/DOM layout, not algorithmic. |

#### Morph Strategy

| Layer | Changes |
|-------|---------|
| **API** | Elements need stable keys for cross-slide matching. Add an optional `key: Option[String]` to `ContentElement` (or equivalent). New `MorphConfig` (duration, easing, unmatched-element animation). |
| **PlatformStrategy impl** | `MorphPlatformStrategy` (JS-only). `setupPlatform` is a no-op. `activateSlide` renders the target slide off-screen to compute element positions. `navigateToSlide` diffs old vs new elements by key, generates per-element tweens. |
| **New code** | ~3 files: strategy class, element differ, tween animator. The differ compares bounding boxes and styles of keyed elements between two rendered slides. |
| **Executor changes** | Medium. The executor currently clears + re-renders on slide switch. Morph needs both old and new slide DOM simultaneously during the transition. May need a new `transitionBetween(fromIndex, toIndex)` method on the strategy, or the executor holds off clearing until the morph completes. |
| **Data model** | Needs element-level identity (keys). This is the biggest conceptual change — today slides are just strings/char grids. |
| **Effort estimate** | Medium–large. Element diffing and coordinated tweens are non-trivial. |

#### Zoom Canvas Strategy

| Layer | Changes |
|-------|---------|
| **API** | New `ZoomPosition` or reuse `SlidePosition` (only `x`, `y`, and a new `scale` field — ignore `z` and rotations). New `ZoomConfig` (min/max scale, pan speed, transition easing). |
| **PlatformStrategy impl** | `ZoomCanvasPlatformStrategy` (JS-only). `setupPlatform` creates a `<div>` with `transform-origin: 0 0` and renders all slides at their canvas positions (like spatial, but flat). `activateSlide` is a no-op. `navigateToSlide` animates `transform: translate(x, y) scale(s)` on the container. |
| **New code** | ~2 files: strategy class, viewport math (compute scale to frame a slide, interpolate between viewports). Much simpler than `CameraAnimator` — just 2D affine math. |
| **Executor changes** | Minimal. Same three-method contract works perfectly. |
| **Layout integration** | Existing `Layout` trait positions work (just ignore z/rotations). Could add a `Layout.nested()` that places child slides at smaller scales within a parent's bounds. |
| **Effort estimate** | Small–medium. Simplest of the four to implement — it's essentially "spatial mode without the 3D". |

#### Notebook Strategy

| Layer | Changes |
|-------|---------|
| **API** | New `NotebookConfig` (typing speed, append animation, separator style, max visible history). |
| **PlatformStrategy impl** | `NotebookPlatformStrategy` (works on both JS and JVM/terminal). `setupPlatform` initializes a scrollable buffer. `activateSlide` appends a new section/separator to the buffer. `navigateToSlide` scrolls to the bottom (or animates the new section appearing). |
| **New code** | ~1–2 files: strategy class, optional typing animator. |
| **Executor changes** | Small. The executor's `clear` before rendering a new slide must be suppressed (or the strategy intercepts it). Need a way to say "don't erase the previous slide." Possibly add a `clearSlide(index): F[Unit]` to the strategy, or have the executor check a `retainsPreviousSlides: Boolean` flag. |
| **Backward navigation** | Needs thought: going backward could either scroll up, or remove the last appended section. |
| **Effort estimate** | Small. Conceptually simple; the main challenge is integrating "no-clear" with the existing executor flow. |

### Suggested implementation order

1. **Zoom Canvas** — lowest effort, reuses existing layout infrastructure, provides immediate visual differentiation from terminal mode without requiring WebGL.
2. **Scroll** — natural fit for browser-based presentations, small code footprint.
3. **Notebook** — interesting for the terminal backend (finally a non-trivial terminal strategy).
4. **Morph** — most ambitious; requires element identity model that doesn't exist yet.

### Shared prerequisites

- The executor's strategy auto-selection logic needs to support more than two options. Currently it checks for `Transforms3D`; we'd need a user-facing config option (e.g. `PresentationMode.Scroll`) or additional capability markers.
- Consider making `PlatformStrategy` selection explicit in `SessionBuilder` rather than purely auto-detected, so users can choose their preferred mode.
