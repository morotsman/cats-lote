# Features Review 2 — Presentation & Content Authoring Tools

## 1. Existing Presentation Features

### 1.1 Text Content
| Feature | Status | Location |
|---------|--------|----------|
| Plain text slides | ✅ Present | `TextSlide`, `TextSlideBuilderDsl` |
| Step-by-step progressive reveal | ✅ Present | `.step()` / `.separator()` / `.hint()` in DSL |
| Horizontal alignment (Left, Center, Right) | ✅ Present | `Alignment.HorizontalAlignment` |
| Vertical alignment (Top, Center, Bottom) | ✅ Present | `Alignment.VerticalAlignment` |
| ANSI bold / bright / gray styling | ✅ Present | `Colors` utility |
| Unicode symbol library (box-drawing, arrows, braille, geometric, etc.) | ✅ Present | `Symbols` utility |

### 1.2 Navigation & Session
| Feature | Status | Location |
|---------|--------|----------|
| Slide titles & quick-jump navigation | ✅ Present | `.title()` in `SlideMetadataDsl`, `QuickNavigation` |
| Timer overlay | ✅ Present | `SessionBuilder.withTimer` |
| Progress bar with milestones | ✅ Present | `SessionBuilder.withProgressBar` |
| Keyboard & mouse input handling | ✅ Present | `UserInput` ADT, `NConsole` |
| Slide-change callback | ✅ Present | `SessionBuilder.onSlideChanged` |

### 1.3 Transitions & Overlays
| Feature | Status | Location |
|---------|--------|----------|
| Built-in transitions (replace, morph, falling, grab, smoke, dissolve) | ✅ Present | `SlideMetadataDsl`, `TransitionType` |
| Custom transition SPI | ✅ Present | `Transition` trait |
| `TickedTransition` builder with lifecycle management | ✅ Present | `api.support.TickedTransition` |
| Easing functions (linear, easeIn, easeOut, easeInOut, cubic, etc.) | ✅ Present | `api.support.Easing` |
| Custom overlay SPI (stateless & stateful) | ✅ Present | `Overlay` trait |
| Animation timing / frame-rate tuning (independent render vs. animation FPS) | ✅ Present | `AnimationSettings`, `FixedStep`, `withFrameRate`, `withAnimationFrameRate` |
| Platform-dependent transition fallback | ✅ Present | `.withFallback()` on `SlideMetadataDsl` |

### 1.4 3D / Spatial Features (WebGL)
| Feature | Status | Location |
|---------|--------|----------|
| 3D world-space positioning (x, y, z) | ✅ Present | `.at()` in `SlideMetadataDsl` |
| 3D Euler rotation (rx, ry, rz) | ✅ Present | `.rotatedBy()` in `SlideMetadataDsl` |
| Relative positioning (right, left, up, down, forward, back) | ✅ Present | `SlideMetadataDsl` |
| Relative rotation (rotateX, rotateY, rotateZ) | ✅ Present | `SlideMetadataDsl` |
| Full position in one call | ✅ Present | `.position(SlidePosition)` |
| Camera fly-through between slides | ✅ Present | WebGL backend |
| Transparent slide backgrounds | ✅ Present | `.transparentBackground()` |
| Spatial layout generators (grid, circle, spiral, line) | ✅ Present | `Layout` sealed trait + `Layout.grid/circle/spiral/line` |
| Layout sections for auto-positioned slides | ✅ Present | `SessionBuilder.addLayoutSection(Layout)` |
| Smoke transition (GPU particles) | ✅ Present | `.smokeTransition()` (WebGL only) |
| Dissolve transition (per-cell randomized fade) | ✅ Present | `.dissolveTransition()` (WebGL only) |
| Sub-pixel character rendering | ✅ Present | `GlideLayer` |
| Platform capability querying | ✅ Present | `SlideContext.capabilities`, `PlatformCapability` |
| Custom 3D scene geometry | ✅ Present | `SlideContext.scene3DRef` |
| WebGL configuration (FOV, clipping, camera, antialias, grid) | ✅ Present | `WebGLConfig` |

### 1.5 Extensibility & Testing
| Feature | Status | Location |
|---------|--------|----------|
| Custom interactive slides | ✅ Present | `SlideBuilderDsl`, `Slide` SPI |
| `TickedSlide` builder for animated custom slides | ✅ Present | `api.support.TickedSlide` |
| Full testkit (TestConsole, TestTicker, SimulatedClock) | ✅ Present | `testkit` package |
| `SlideTestHarness` for one-line test setup | ✅ Present | `testkit.SlideTestHarness` |
| `TickedTransition.forTest(harness)` for testing transitions | ✅ Present | `api.support.TickedTransition` |
| Contextual factory support | ✅ Present | `Contextual`, `ContextualF` wrappers |
| `SessionBuilder.runWith` for full-session integration tests | ✅ Present | `SessionBuilder.runWith(console, ticker)` |

### 1.6 Cross-Platform
| Feature | Status | Location |
|---------|--------|----------|
| JVM terminal backend (JLine) | ✅ Present | `lote/jvm/` |
| Browser/WebGL backend (Scala.js + Three.js) | ✅ Present | `lote/js/` |
| Shared presentation code across platforms | ✅ Present | `shared-examples/`, `SharedAdvancedPresentation` |
| Custom ticker factory (e.g., RAF-based) | ✅ Present | `SessionBuilder.withCustomTicker` |
| Graceful feature degradation (spatial no-ops on terminal) | ✅ Present | All spatial methods silently ignored on JVM |

---

## 2. Changes Since Previous Review

The following features were **added** since FeaturesReview.md:

| Feature | Impact | Notes |
|---------|--------|-------|
| **Spatial Layout generators** (`Layout.grid/circle/spiral/line`) | High | Automates 3D positioning for groups of slides |
| **`addLayoutSection`** on SessionBuilder | High | Declarative layout application to slide groups |
| **`TickedTransition` support framework** | High | Eliminates transition boilerplate (lifecycle, FixedStep, Deferred, final-frame) |
| **Easing functions** (`Easing` object) | Medium | Built-in easing curves for smooth animations |
| **`TickedSlide` support framework** | Medium | Same pattern as TickedTransition but for animated custom slides |
| **Relative positioning** (`.right()`, `.left()`, `.down()`, `.up()`, `.forward()`, `.back()`) | Medium | Relative slide placement without absolute coords |
| **Relative rotation** (`.rotateX()`, `.rotateY()`, `.rotateZ()`) | Medium | Cumulative rotation offsets |
| **`.position(SlidePosition)` shorthand** | Low | Set full position in one call |
| **`.withFallback()` on transitions** | Medium | Override terminal fallback for WebGL transitions |
| **`withCustomTicker` on SessionBuilder** | Medium | RAF-based ticker for WebGL |
| **Platform capability querying** | Medium | Runtime checks for `Effects`, `SubPixelRendering`, `Transforms3D` |
| **`GlideLayer` sub-pixel rendering** | Medium | Smooth character interpolation on WebGL |
| **`SlideTestHarness.runWithTicking`** | Medium | Test transitions without deadlock |
| **`SessionBuilder.runWith`** | Medium | Full-session integration testing |
| **Smoke & Dissolve transitions** | Medium | GPU-accelerated WebGL transitions with fallbacks |
| **WebGL configuration** (`WebGLConfig`) | Low | Tunable renderer parameters |
| **Scene3DRef** | Low | Custom 3D geometry in shared Three.js scene |

---

## 3. Missing Content Presentation Tools

The **content authoring layer** remains the primary gap. The library continues to offer a plain `.content(string)` method, leaving all formatting, structure, and annotation to the user.

### 3.1 Speaker Notes / Presenter Notes
- **What:** A private second-channel view (overlay or external file) showing talking points visible only to the presenter.
- **Why it matters:** Standard in PowerPoint, Keynote, reveal.js, presenterm. Presenters need cues and timing hints.
- **Suggestion:** `.speakerNotes(text)` on TextSlideBuilderDsl + toggleable overlay (practice mode) + dual-terminal file output (live mode). See Appendix A of FeaturesReview.md for full design.

### 3.2 Inline Code Blocks with Syntax Highlighting
- **What:** Fenced code regions with language-aware ANSI colorization (keywords, strings, comments in distinct colors).
- **Why it matters:** The #1 use case for a terminal presentation tool is showing code. Monochrome code walls are painful.
- **Suggestion:** `.codeBlock(language, source)` in the DSL. Integrate a lightweight ANSI tokenizer or shell out to `bat --color=always --style=plain`.

### 3.3 Inline Text Formatting (Rich Text Markup)
- **What:** Apply **bold**, *italic*, ~~strikethrough~~, underline, foreground/background color to arbitrary spans within content — not just the entire slide.
- **Why it matters:** `Colors` exposes raw ANSI constants but there's no user-facing markup language or string interpolator.
- **Suggestion:** A string interpolator `styled"Hello ${bold("world")}"` or mini-Markdown subset within `.content()`.

### 3.4 Bullet / Numbered Lists
- **What:** Structured list rendering with automatic indentation, bullet characters, and numbered sequences.
- **Why it matters:** Slides are overwhelmingly bullet-driven. Manual padding is tedious and error-prone.
- **Suggestion:** `.bulletList(items*)` and `.numberedList(items*)`, using `Symbols` for bullet characters.

### 3.5 Callout / Admonition Boxes
- **What:** Bordered boxes for tips, warnings, notes — using box-drawing characters from `Symbols`.
- **Why it matters:** Common in technical presentations to draw attention to caveats or best practices.
- **Suggestion:** `.callout(style, text)` with `Note`, `Warning`, `Tip` styles.

### 3.6 Section Headers / Heading Levels
- **What:** Distinct heading styles (H1, H2, H3) with different weights, separators, or banners.
- **Why it matters:** Visual hierarchy. Currently all text renders at uniform weight.
- **Suggestion:** `.heading(level, text)` with bold + underline for H1, bold for H2, italic for H3.

### 3.7 Tables
- **What:** Tabular data with auto-sized columns, header rows, and box-drawing borders.
- **Why it matters:** Data-heavy slides need structured grids. Manual formatting is fragile across terminal widths.
- **Suggestion:** `.table(headers, rows)` using `Symbols.boxThin` borders, with auto column-width calculation.

### 3.8 Image / ASCII-Art Embedding
- **What:** Load or embed pre-made ASCII art, FIGlet-style large text, or diagrams.
- **Why it matters:** Visual variety. `Symbols` provides primitives but no higher-level composition.
- **Suggestion:** `.asciiArt(text)` or `.figlet(text, font)` for banners, `.artFromFile(path)` for external art.

### 3.9 Annotations / Footnotes
- **What:** Numbered or symbolic footnotes at the bottom of a slide, linked from inline markers.
- **Why it matters:** Citations and clarifications without cluttering the main content.
- **Suggestion:** `.footnote(marker, text)` with auto-rendered footnote bar at slide bottom.

### 3.10 Tooltip / Hover Annotations (Mouse-Aware)
- **What:** Slide regions that reveal commentary on mouse hover, leveraging existing `MouseMove` input.
- **Why it matters:** The input system supports mouse events but no component uses hover for contextual help.
- **Suggestion:** `.tooltip(region, text)` on interactive slides.

---

## 4. Missing Structural & Visual Features

### 4.1 Content Slide Layouts (Title + Content, Two-Column, etc.)
- **What:** Pre-defined templates dividing the screen into named regions (title bar, body, sidebar, columns).
- **Why it matters:** Every mainstream tool has layouts. Without them, users manually concatenate title text, add blank lines, and pad columns by hand. This is the most labour-intensive gap.
- **Current state:** `Alignment` positions the *entire* content block. `Layout` (added since last review) handles *3D spatial positioning* of slides in WebGL — but there are still no *content region layouts* within a single slide.
- **Key distinction:** `Layout.grid/circle/spiral/line` ≠ slide content layouts. The former positions slides in 3D space; the latter divides a single slide's screen area into structured regions.
- **Suggestion:** `Layout.TitleAndContent`, `Layout.TwoColumn`, `Layout.TitleSlide`, etc. — pure functions `(Screen, LayoutContent) => ScreenAdjusted`. See FeaturesReview.md §2.11 for detailed DSL proposal.

### 4.2 Theming / Color Schemes
- **What:** A `Theme` object defining a coherent color palette applied consistently across all slides.
- **Why it matters:** Every other tool lets you set a color scheme once. reveal.js has ~20 themes. presenterm has catppuccin, dracula, gruvbox.
- **Suggestion:** `SessionBuilder[IO]().withTheme(Theme.Dracula)` — feeds into layouts, headings, code blocks, and progress bar colors.

### 4.3 Global Default Transition
- **What:** Set a transition once for the entire presentation instead of per-slide.
- **Why it matters:** Currently every slide must explicitly set its transition. PowerPoint has "Apply to All"; reveal.js has `transition: 'slide'` in config.
- **Suggestion:** `SessionBuilder[IO]().withDefaultTransition(_.morphTransition())` — applied to any slide that doesn't override.
- **Effort:** Low. Check `SlideSpecification.transition` for `None`, substitute default.

### 4.4 Slide Numbering
- **What:** A small "3/12" indicator in a corner or footer.
- **Why it matters:** Virtually every presentation tool has this. Distinct from the progress bar.
- **Suggestion:** `.withSlideNumbers()` on SessionBuilder — renders as a subtle overlay in bottom-right using `Colors.Gray`.
- **Effort:** Low. Built-in overlay, similar to Timer.

### 4.5 Header / Footer Chrome
- **What:** Persistent text regions at the top/bottom of every slide (company name, date, title).
- **Why it matters:** Professional appearance. Standard in PowerPoint masters, Beamer footline, presenterm.
- **Suggestion:** `.withHeader("Company Conf 2026")` and `.withFooter(ctx => s"${ctx.title} | ${ctx.slideNumber}/${ctx.total}")`.
- **Effort:** Low.

### 4.6 Fragment Styles (Fade, Highlight, Strike-Through)
- **What:** Previous `.step()` items dimmed/grayed, current highlighted bold/colored, or items struck through.
- **Why it matters:** reveal.js has `fade-in`, `fade-out`, `highlight-red`, `strike`. Beamer has `\alert`, `\temporal`.
- **Suggestion:** `.step("point", style = StepStyle.HighlightCurrent)` — dims previous steps.
- **Effort:** Medium. Requires changes to `TextSlide` rendering logic.

### 4.7 Blank / Black Screen
- **What:** Press `B` to black out the screen. Press again to restore.
- **Why it matters:** Used when the presenter wants full audience attention on spoken words. Standard in PowerPoint, Keynote, reveal.js, presenterm.
- **Suggestion:** Built-in middleware (like `Idle`) listening for `Character('b')` rendering blank `ScreenAdjusted`.
- **Effort:** Low. Trivial overlay.

### 4.8 Keyboard Shortcut Help Overlay
- **What:** Press `?` to show/hide a help panel listing available shortcuts.
- **Why it matters:** Discoverability. Standard in reveal.js, slides (Charm).
- **Suggestion:** Built-in `Overlay` (like QuickNavigation) toggled by `?`.
- **Effort:** Low.

---

## 5. Missing Authoring & Workflow Features

### 5.1 Markdown Source Format
- **What:** Define presentations in `.md` with `---` separators, parsed into slides at runtime.
- **Why it matters:** Every modern terminal tool (presenterm, slides, Marp, mdp, lookatme) uses markdown as primary format. Drastically lowers authoring barrier.
- **Suggestion:** `SessionBuilder.fromMarkdown(path)` loader. The Scala DSL remains for interactive/custom slides.
- **Effort:** High.

### 5.2 Incremental Code Highlighting
- **What:** Show a code block, then on successive steps highlight specific lines (dim the rest).
- **Why it matters:** Essential for "walk through the code" slides. Slidev, presenterm, Beamer all have it.
- **Suggestion:** `.codeBlock(lang, source).highlightLines(1 to 3).thenHighlight(5 to 8)` — integrates with `.step()`.
- **Effort:** Medium.

### 5.3 Hot Reload / Live Preview
- **What:** Presentation auto-refreshes on source file changes.
- **Why it matters:** Developer experience. presenterm has `--watch`, Slidev has HMR.
- **Suggestion:** For Markdown loader, use `fs2.io.file.Files.watch`. For Scala DSL, rely on `sbt ~run`.
- **Effort:** Medium.

### 5.4 Export to PDF
- **What:** Render all slides to static PDF for sharing/archival.
- **Why it matters:** Standard in all traditional tools, Marp, presenterm.
- **Suggestion:** `SessionBuilder[IO]().exportPdf(path)` iterating slides, rendering to fixed-width-font PDF via PDFBox.
- **Effort:** Medium.

### 5.5 Auto-Advance / Kiosk Mode
- **What:** Slides advance automatically after a configured duration.
- **Why it matters:** Lobby displays, trade shows, timed lightning talks.
- **Suggestion:** `.withAutoAdvance(every = 30.seconds)` or per-slide `.autoAdvance(15.seconds)`.
- **Effort:** Low.

### 5.6 Live Code Execution
- **What:** Run code snippets inline and display stdout below the code block.
- **Why it matters:** Impressive for tech talks. Slidev, presenterm have it.
- **Suggestion:** `.executableCode("scala", snippet)` using scala-cli or Ammonite.
- **Effort:** High.

### 5.7 Mermaid / ASCII Diagrams
- **What:** Render diagrams from text DSL (flowcharts, sequence diagrams).
- **Why it matters:** presenterm renders mermaid to ASCII via `mmdc`.
- **Suggestion:** `.mermaid(source)` shelling out to `mmdc` or `graph-easy`.
- **Effort:** High.

### 5.8 Pointer / Cursor Highlight
- **What:** A visible marker the presenter moves with arrow keys to draw attention.
- **Why it matters:** PowerPoint laser pointer, Keynote pointer.
- **Suggestion:** Toggleable mode (press `P`) showing a highlighted cursor, arrows move it instead of navigating.
- **Effort:** Medium.

---

## 6. Cross-Tool Feature Comparison (Updated)

### Reference tools surveyed:
- **Traditional:** PowerPoint, Keynote, Google Slides
- **Web-based:** reveal.js, Slidev, impress.js, Marp
- **Academic:** LaTeX Beamer
- **Terminal-native:** presenterm, slides (Charm), mdp, lookatme, sent

---

### 6.1 Content & Formatting

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Rich text (bold/italic/color spans) | ✅ | ✅ | ✅ | ✅ | ❌ | §3.3 |
| Bullet / numbered lists | ✅ | ✅ | ✅ | ✅ | ❌ | §3.4 |
| Tables | ✅ | ✅ | ✅ | ✅ | ❌ | §3.7 |
| Syntax-highlighted code | ✅ | ✅ | ✅ | ✅ | ❌ | §3.2 |
| Incremental code highlighting | ❌ | ✅ | ✅ | ✅ | ❌ | §5.2 |
| Mermaid / diagrams | ✅ | ✅ | ✅ | ✅ | ❌ | §5.7 |
| Emoji rendering | ✅ | ✅ | ❌ | ✅ | ❌ | trivial |
| Markdown source format | ❌ | ✅ | ❌ | ✅ | ❌ | §5.1 |
| Headings / hierarchy | ✅ | ✅ | ✅ | ✅ | ❌ | §3.6 |
| Callout / admonition boxes | ❌ | ✅ | ✅ | ✅ | ❌ | §3.5 |

### 6.2 Layout & Visual Structure

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Content layouts (Title+Content, Two-Column) | ✅ | ✅ | ✅ | ✅ | ❌ | §4.1 |
| **Spatial layouts (3D positioning)** | ❌ | ✅ (impress.js) | ❌ | ❌ | ✅ | ✓ NEW |
| **Layout generators (grid, circle, spiral)** | ❌ | ❌ | ❌ | ❌ | ✅ | ✓ NEW |
| Theming / color schemes | ✅ | ✅ | ✅ | ✅ | ❌ | §4.2 |
| Slide numbering | ✅ | ✅ | ✅ | ✅ | ❌ | §4.4 |
| Header / footer chrome | ✅ | ✅ | ✅ | ✅ | ❌ | §4.5 |
| Vertical / nested slides | ❌ | ✅ | ❌ | ❌ | ❌ | niche |

### 6.3 Navigation & Presenter UX

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Quick-jump to slide by title | ✅ | ✅ | ❌ | ✅ | ✅ | ✓ |
| Speaker notes | ✅ | ✅ | ✅ | ✅ | ❌ | §3.1 |
| Blank/black screen | ✅ | ✅ | N/A | ✅ | ❌ | §4.7 |
| Keyboard shortcut help ('?') | ✅ | ✅ | N/A | ✅ | ❌ | §4.8 |
| Pointer / cursor highlight | ✅ | ✅ | N/A | ❌ | ❌ | §5.8 |
| Per-slide timing / pacing | ✅ | ✅ | ❌ | ❌ | ❌ | niche |
| Progress bar | ✅ | ✅ | ✅ | ✅ | ✅ | ✓ |
| Timer | ✅ | ✅ | ❌ | ❌ | ✅ | ✓ |
| Slide-change callback | ✅ | ✅ | ❌ | ❌ | ✅ | ✓ |

### 6.4 Authoring & Workflow

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Hot reload / live preview | ✅ | ✅ | ❌ | ✅ | ❌ | §5.3 |
| Export to PDF | ✅ | ✅ | ✅ | ✅ | ❌ | §5.4 |
| Auto-advance / kiosk mode | ✅ | ✅ | ❌ | ❌ | ❌ | §5.5 |
| Live code execution | ❌ | ✅ | ❌ | ✅ | ❌ | §5.6 |
| **Full session integration testing** | ❌ | ❌ | ❌ | ❌ | ✅ | ✓ unique |
| **Deterministic animation testing** | ❌ | ❌ | ❌ | ❌ | ✅ | ✓ unique |

### 6.5 Animations & Transitions

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Slide transitions | ✅ | ✅ | ✅ | limited | ✅ | ✓ |
| Step-by-step reveals | ✅ | ✅ | ✅ | ✅ | ✅ | ✓ |
| Fragment styles (fade, highlight, strike) | ✅ | ✅ | ✅ | ❌ | ❌ | §4.6 |
| Global default transition | ✅ | ✅ | ✅ | ✅ | ❌ | §4.3 |
| **Easing functions** | ✅ | ✅ | ❌ | ❌ | ✅ | ✓ NEW |
| **Transition builder framework** | ❌ | ❌ | ❌ | ❌ | ✅ | ✓ unique |
| **Platform-aware transition fallbacks** | ❌ | N/A | N/A | N/A | ✅ | ✓ unique |
| Idle animation | screensaver | ❌ | ❌ | ❌ | ✅ | ✓ unique |
| **Sub-pixel character gliding (WebGL)** | N/A | N/A | N/A | ❌ | ✅ | ✓ unique |
| **GPU particle effects** | N/A | partial | N/A | ❌ | ✅ | ✓ unique |

### 6.6 Platform & Architecture

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Cross-platform (terminal + browser) | ❌ | browser only | PDF only | terminal only | ✅ | ✓ unique |
| **Same code on both platforms** | N/A | N/A | N/A | N/A | ✅ | ✓ unique |
| Graceful feature degradation | N/A | N/A | N/A | N/A | ✅ | ✓ |
| 3D camera navigation | ❌ | ✅ (impress.js) | ❌ | ❌ | ✅ | ✓ |
| Custom 3D scene geometry | ❌ | partial | ❌ | ❌ | ✅ | ✓ unique |

---

## 7. Revised Priority Ranking

| Priority | Feature | Impact | Effort | Category |
|----------|---------|--------|--------|----------|
| 🔴 High | **Content slide layouts** (Title+Content, Two-Column) | Foundational content gap | Medium | Structure |
| 🔴 High | **Theming / color schemes** | Visual coherence | Medium | Visual |
| 🔴 High | Inline text formatting / rich markup | Every slide benefits | Medium | Content |
| 🔴 High | Syntax-highlighted code blocks | #1 terminal presentation use case | High | Content |
| 🔴 High | Speaker notes (overlay + dual-terminal) | Core presenter need | Medium | Presenter UX |
| 🔴 High | **Global default transition** | Reduces boilerplate; trivial | Low | Transitions |
| 🟡 Medium | **Blank screen ('B' key)** | Quick win; common need | Low | Presenter UX |
| 🟡 Medium | **Keyboard shortcut help ('?')** | Discoverability | Low | Presenter UX |
| 🟡 Medium | **Slide numbering** | Expected everywhere | Low | Visual |
| 🟡 Medium | **Header / footer chrome** | Professional appearance | Low | Visual |
| 🟡 Medium | **Fragment styles (fade, highlight)** | Richer step-by-step | Medium | Animations |
| 🟡 Medium | **Incremental code highlighting** | Essential for code walkthroughs | Medium | Content |
| 🟡 Medium | Bullet / numbered lists | Very common pattern | Low | Content |
| 🟡 Medium | Section headers / heading levels | Visual hierarchy | Low | Content |
| 🟡 Medium | Tables | Data presentation | Medium | Content |
| 🟡 Medium | Callout / admonition boxes | Leverages existing `Symbols` | Low | Content |
| 🟡 Medium | **Markdown source format** | Lowers authoring barrier | High | Authoring |
| 🟢 Low | **Hot reload / live preview** | Dev experience | Medium | Authoring |
| 🟢 Low | **Export to PDF** | Sharing & archival | Medium | Workflow |
| 🟢 Low | **Auto-advance / kiosk mode** | Niche but useful | Low | Navigation |
| 🟢 Low | Footnotes / annotations | Niche | Low | Content |
| 🟢 Low | ASCII-art / FIGlet embedding | Polish | Medium | Content |
| 🟢 Low | **Mermaid / ASCII diagrams** | Complex | High | Content |
| 🟢 Low | **Live code execution** | Wow factor | High | Content |
| 🟢 Low | Tooltip / hover annotations | Advanced | High | Content |
| 🟢 Low | **Pointer / cursor highlight** | Niche | Medium | Presenter UX |
| 🟢 Low | **Vertical / nested slides** | Niche for terminal | High | Structure |
| 🟢 Low | **Emoji rendering** | Trivial if terminal supports | Low | Content |

---

## 8. Summary

### Strengths (Expanded Since Last Review)

The library has significantly strengthened its **structural mechanics and extensibility** since the previous review:

1. **Spatial layouts** (`Layout.grid/circle/spiral/line` + `addLayoutSection`) are a unique differentiator not found in any terminal-native tool, bringing impress.js-style 3D presentation to the terminal+WebGL ecosystem.
2. **`TickedTransition` framework** drastically lowers the barrier to writing custom transitions — from ~150 lines of lifecycle boilerplate to ~10 lines of rendering logic.
3. **Cross-platform architecture** (same presentation code on JVM terminal + browser WebGL) is genuinely unique in the presentation tool space.
4. **Testing infrastructure** (`SlideTestHarness`, `runWith`, `SimulatedClock`, `TickedTransition.forTest`) is unmatched — no other presentation tool provides deterministic, time-controllable test doubles.
5. **Easing functions** and **sub-pixel GlideLayer** bring animation quality closer to web-based tools.
6. **Platform capability querying** and **transition fallbacks** elegantly solve the cross-platform rendering problem.

### Persistent Gaps

The **content authoring layer remains thin**. Users still receive `.content(string)` and must manually handle all formatting, structure, and annotation. The core gaps from the previous review are unchanged:

1. **No theming** — every other tool lets you set a color scheme once
2. **No content layouts** — forces manual string formatting for every slide (note: spatial `Layout` solves 3D positioning, not this)
3. **No rich text DSL** — ANSI codes exist internally but aren't exposed ergonomically
4. **No code highlighting** — the #1 use case for a terminal presentation tool
5. **No markdown input** — every modern terminal tool uses markdown as primary format

### Quick Wins

These could each be shipped in under a day and would immediately improve polish:

| Feature | Effort | Lines of code (est.) |
|---------|--------|---------------------|
| Blank screen (`B` key) | ~1 hour | ~30 lines |
| Keyboard shortcut help (`?`) | ~2 hours | ~60 lines |
| Slide numbering overlay | ~2 hours | ~50 lines |
| Global default transition | ~1 hour | ~20 lines (check `None`, substitute default) |

### Strategic Positioning

cats-lote occupies a **unique niche**: it's the only presentation tool that is simultaneously:
- Functional-programming-native (Cats Effect, pure FP)
- Cross-platform (terminal + WebGL from same code)
- Programmable (Scala DSL, not just a file format)
- 3D-capable (spatial layouts, camera navigation)
- Fully testable (deterministic simulated time, test harness)

The competition from markdown-based tools (presenterm, slides) is primarily on **authoring convenience** and **content features** — exactly where cats-lote is weakest. The strategic question is whether to compete on ease-of-authoring (markdown, themes, code highlighting) or lean further into the programmable/interactive niche (custom slides, live code, 3D scenes).

**Recommendation:** Do both. The quick wins (theming, global transitions, slide numbers, blank screen, help overlay) and medium-effort content features (rich text, bullet lists, headings, code blocks) bring parity with terminal tools. The spatial layouts, testability, and cross-platform story remain the unique differentiators that no markdown tool can match.

---

## 9. Terminology Clarification: "Layout" Ambiguity

The term "Layout" now has two meanings in cats-lote, which may cause confusion:

| Concept | Current Implementation | What's Missing |
|---------|----------------------|----------------|
| **Spatial Layout** (3D positioning) | ✅ `Layout.grid/circle/spiral/line` + `addLayoutSection` | — |
| **Content Layout** (slide regions) | ❌ Not implemented | Title+Content, Two-Column, Section Header, etc. |

**Spatial Layouts** answer: "Where does this slide exist in 3D space?"
**Content Layouts** answer: "How is content arranged *within* a single slide?"

If content layouts are implemented, consider naming them distinctly (e.g., `SlideTemplate` or `ContentLayout`) to avoid ambiguity with the existing `Layout` sealed trait.

