# Features Review — Presentation & Comment Presentation Tools

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
| Slide titles & quick-jump navigation | ✅ Present | `.title()` in `SlideMetadataDsl` |
| Timer overlay | ✅ Present | `SessionBuilder.withTimer` |
| Progress bar with milestones | ✅ Present | `SessionBuilder.withProgressBar` |
| Keyboard & mouse input handling | ✅ Present | `UserInput` ADT, `NConsole` |

### 1.3 Transitions & Overlays
| Feature | Status | Location |
|---------|--------|----------|
| Built-in transitions (replace, morph, falling, grab) | ✅ Present | `SlideMetadataDsl` |
| Custom transition SPI | ✅ Present | `Transition` trait |
| Custom overlay SPI (stateless & stateful) | ✅ Present | `Overlay` trait |
| Animation timing / frame-rate tuning | ✅ Present | `AnimationSettings`, `FixedStep` |

### 1.4 Extensibility & Testing
| Feature | Status | Location |
|---------|--------|----------|
| Custom interactive slides | ✅ Present | `SlideBuilderDsl`, `Slide` SPI |
| Full testkit (TestConsole, TestTicker, SimulatedClock) | ✅ Present | `testkit` package |
| Contextual factory support | ✅ Present | `Contextual` wrappers |

---

## 2. Missing Comment Presentation Tools

"Comment presentation" encompasses any mechanism that lets a presenter annotate, enrich, or meta-communicate content to the audience beyond raw text. The following commonly expected tools are **absent** from the current API.

### 2.1 Speaker Notes / Presenter Notes
- **What:** A second-channel view (or togglable overlay) showing private notes visible only to the presenter, not rendered on the main slide.
- **Why it matters:** Standard in every mainstream presentation tool (PowerPoint, Keynote, reveal.js). Presenters need cues, talking points, and timing hints.
- **Suggestion:** Add a `.speakerNotes(text)` method to `TextSlideBuilderDsl` and a toggleable notes overlay triggered by a hotkey.
- **Practical design:** See [Appendix A](#appendix-a--speaker-notes-practical-design) below.

### 2.2 Inline Code Blocks with Syntax Highlighting
- **What:** Fenced or indented code regions rendered with language-aware ANSI colorization (keywords, strings, comments in different colors).
- **Why it matters:** A presentation library will frequently be used to show code. Without highlighting, code slides are a wall of monochrome text.
- **Suggestion:** Integrate a lightweight ANSI syntax-highlighting library (e.g., a Scala port or JVM wrapper around a Treesitter / Pygments-style tokenizer) and expose `.codeBlock(language, source)` in the DSL.

### 2.3 Inline Text Formatting (Rich Text Markup)
- **What:** Ability to apply **bold**, *italic*, ~~strikethrough~~, underline, foreground/background color to arbitrary spans within a single content string—not just the entire slide.
- **Why it matters:** The existing `Colors` utility only exposes a handful of raw ANSI constants; there is no user-facing DSL or markup language (e.g., a mini-Markdown or tagged-string interpolator) for inline styling.
- **Suggestion:** Provide a string interpolator (e.g., `styled"Hello ${bold("world")}"`) or support a subset of Markdown/ANSI markup inside `.content()`.

### 2.4 Bullet / Numbered Lists
- **What:** Structured list rendering with automatic indentation, bullet characters (from the existing `Symbols` library), and numbered sequences.
- **Why it matters:** Slides are overwhelmingly bullet-point driven. Currently users must manually pad and prefix every line.
- **Suggestion:** Add `.bulletList(items*)` and `.numberedList(items*)` helpers, optionally using symbols from the `Symbols` object.

### 2.5 Callout / Admonition Boxes
- **What:** Visually distinct bordered boxes for tips, warnings, notes, or important asides—using box-drawing characters already available in `Symbols`.
- **Why it matters:** Common in technical presentations to draw attention to caveats, best practices, or side-comments.
- **Suggestion:** Add `.callout(style, text)` with styles like `Note`, `Warning`, `Tip`, rendered with `Symbols.boxRounded` or `Symbols.boxDouble` borders.

### 2.6 Annotations / Footnotes
- **What:** Numbered or symbolic footnotes that appear at the bottom of a slide, linked from inline markers.
- **Why it matters:** Lets presenters add citations, clarifications, or tangential comments without cluttering the main content flow.
- **Suggestion:** Support `.footnote(marker, text)` that auto-renders a footnote bar at the slide bottom.

### 2.7 Section Headers / Heading Levels
- **What:** Distinct heading sizes or styles (H1, H2, H3) rendered with different weights, separators, or ASCII-art banners.
- **Why it matters:** Long or multi-topic slides benefit from visual hierarchy. Currently all text is rendered at a single uniform weight.
- **Suggestion:** Add `.heading(level, text)` that applies bold + underline for H1, bold for H2, italic for H3, etc., with optional separator lines.

### 2.8 Tables
- **What:** Tabular data rendering with auto-sized columns, header rows, and box-drawing borders.
- **Why it matters:** Data-heavy presentations need structured grids. Manual formatting is tedious and fragile across different terminal widths.
- **Suggestion:** Add `.table(headers, rows)` using `Symbols.boxThin` or similar for borders, with automatic column-width calculation respecting `Screen` dimensions.

### 2.9 Image / ASCII-Art Embedding
- **What:** Helper to load or embed pre-made ASCII art (e.g., from a file or FIGlet-style large text) and position it on a slide.
- **Why it matters:** Visual variety. The `Symbols` library provides primitives but there is no higher-level composition tool.
- **Suggestion:** Add `.asciiArt(text)` or `.figlet(text, font)` for large banner text, and `.artFromFile(path)` for external ASCII art.

### 2.10 Tooltip / Hover Annotations (Mouse-Aware)
- **What:** Regions of the slide that reveal additional commentary when the mouse hovers over them, leveraging the existing `MouseMove` input type.
- **Why it matters:** The input system already supports mouse events (`MouseClick`, `MouseMove`), but no built-in component uses hover to reveal contextual comments.
- **Suggestion:** Add a `.tooltip(region, text)` mechanism on interactive slides that renders a transient overlay on hover.

### 2.11 Slide Layouts (Title Slide, Title + Content, Two-Column, etc.)
- **What:** Pre-defined spatial templates that divide the screen into named regions (title bar, content body, footer, sidebar, columns) so users don't position everything manually.
- **Why it matters:** This is a **foundational feature** in every mainstream presentation tool:
  - PowerPoint/Keynote: "Title Slide", "Title and Content", "Two Content", "Section Header", "Blank"
  - reveal.js/Slidev: layout slots via HTML/Vue templates
  - LaTeX Beamer: `\frametitle{}`, headline/footline templates, columns environment
  
  Without layouts, every slide is a "blank canvas" where the user must manually concatenate title text, add blank lines for spacing, and pad columns by hand. This is the most labour-intensive gap in the current DSL.
- **Current state:** cats-lote only offers `Alignment` (vertical + horizontal positioning of the *entire* content block). There is no concept of distinct regions on a slide.
- **Common layouts needed:**

  | Layout | Regions | Use case |
  |--------|---------|----------|
  | **Title Slide** | Large centered title + subtitle line | Opening / section dividers |
  | **Title + Content** | Title bar (top, bold/underlined) + body below | Most common slide type |
  | **Two-Column** | Title bar + left column + right column | Comparisons, code vs. output |
  | **Title + Bullet List** | Title bar + auto-formatted list | Standard talking-point slides |
  | **Full-Screen** | No chrome, content fills screen | ASCII art, diagrams, demos |
  | **Section Header** | Centered large text + thin separator | Topic transitions |

- **Suggested DSL:**

  ```scala
  // Title slide
  .addTextSlide { _ =>
    _.layout(Layout.TitleSlide)
      .title("Cats Effect in Production")
      .subtitle("Lessons from 2 years at scale")
  }

  // Title + content (the "normal" slide)
  .addTextSlide { _ =>
    _.layout(Layout.TitleAndContent)
      .title("Why Cats Effect?")
      .content(
        """Composable concurrency primitives
          |Resource safety via bracket/Resource
          |Testable with IORuntime control""".stripMargin)
  }

  // Two-column layout
  .addTextSlide { _ =>
    _.layout(Layout.TwoColumn)
      .title("Before vs After")
      .leftColumn("Future {\n  // callback hell\n}")
      .rightColumn("IO {\n  // pure bliss\n}")
  }
  ```

- **Implementation approach:** A `Layout` is a pure function `(Screen, LayoutContent) => ScreenAdjusted` that:
  1. Receives the terminal dimensions and named content regions
  2. Computes the character grid with borders, padding, and alignment per region
  3. Returns the composed `ScreenAdjusted` string
  
  Layouts could be implemented as a sealed trait with built-in variants, plus a `Layout.custom(...)` escape hatch for user-defined templates.

---

## 3. Cross-Tool Feature Comparison

A systematic comparison against the features offered by major presentation tools, grouped by category. Features already identified in Section 2 are marked with a reference; **new gaps** are highlighted with ⚠️.

### Reference tools surveyed:
- **Traditional:** PowerPoint, Keynote, Google Slides
- **Web-based:** reveal.js, Slidev, impress.js, Marp
- **Academic:** LaTeX Beamer
- **Terminal-native:** presenterm, slides (Charm), mdp, lookatme, sent

---

### 3.1 Content & Formatting

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Rich text (bold/italic/color) | ✅ | ✅ | ✅ | ✅ (presenterm, slidev) | ❌ | §2.3 |
| Bullet / numbered lists | ✅ | ✅ | ✅ | ✅ (all markdown tools) | ❌ | §2.4 |
| Tables | ✅ | ✅ | ✅ | ✅ (presenterm, lookatme) | ❌ | §2.8 |
| Syntax-highlighted code | ✅ (via plugin) | ✅ | ✅ (minted/listings) | ✅ (presenterm, slidev) | ❌ | §2.2 |
| **⚠️ Incremental code highlighting** | ❌ | ✅ (Slidev) | ✅ (lstlisting) | ✅ (presenterm) | ❌ | **NEW** |
| **⚠️ Mermaid / diagrams** | ✅ (SmartArt) | ✅ (mermaid plugin) | ✅ (tikz) | ✅ (presenterm) | ❌ | **NEW** |
| **⚠️ Emoji rendering** | ✅ | ✅ | ❌ | ✅ (presenterm, slides) | ❌ | **NEW** |
| **⚠️ Markdown source format** | ❌ | ✅ | ❌ | ✅ (all modern) | ❌ | **NEW** |
| Headings / hierarchy | ✅ | ✅ | ✅ | ✅ | ❌ | §2.7 |
| Callout / admonition boxes | ❌ | ✅ (plugins) | ✅ (tcolorbox) | ✅ (presenterm) | ❌ | §2.5 |

### 3.2 Layout & Visual Structure

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Slide layouts / masters | ✅ | ✅ | ✅ | ✅ (presenterm) | ❌ | §2.11 |
| **⚠️ Theming / color schemes** | ✅ | ✅ (20+ themes) | ✅ (dozens) | ✅ (presenterm, slides) | ❌ | **NEW** |
| **⚠️ Slide numbering** | ✅ | ✅ | ✅ | ✅ (most) | ❌ | **NEW** |
| **⚠️ Header / footer chrome** | ✅ | ✅ | ✅ (headline/footline) | ✅ (presenterm) | ❌ | **NEW** |
| **⚠️ Vertical / nested slides** | ❌ | ✅ (reveal.js signature) | ❌ | ❌ | ❌ | **NEW** |

### 3.3 Navigation & Presenter UX

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Quick-jump to slide by title | ✅ | ✅ | ❌ | ✅ (slides) | ✅ | ✓ |
| Speaker notes | ✅ | ✅ | ✅ | ✅ (presenterm) | ❌ | §2.1 |
| **⚠️ Blank/black screen** | ✅ | ✅ ('B' key) | N/A | ✅ (slides, presenterm) | ❌ | **NEW** |
| **⚠️ Keyboard shortcut help ('?')** | ✅ | ✅ | N/A | ✅ (slides) | ❌ | **NEW** |
| **⚠️ Pointer / cursor highlight** | ✅ | ✅ (laser plugin) | N/A | ❌ | ❌ | **NEW** |
| **⚠️ Per-slide timing / pacing guide** | ✅ (rehearse) | ✅ (Slidev timer) | ❌ | ❌ | ❌ | **NEW** |
| Progress bar | ✅ | ✅ | ✅ (metropolis) | ✅ (presenterm) | ✅ | ✓ |
| Timer | ✅ | ✅ | ❌ | ❌ | ✅ | ✓ |

### 3.4 Authoring & Workflow

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| **⚠️ Hot reload / live preview** | ✅ | ✅ | ❌ (latexmk) | ✅ (presenterm --watch) | ❌ | **NEW** |
| **⚠️ Export to PDF** | ✅ | ✅ | ✅ (native) | ✅ (presenterm, Marp) | ❌ | **NEW** |
| **⚠️ Auto-advance / kiosk mode** | ✅ | ✅ (autoSlide) | ❌ | ❌ | ❌ | **NEW** |
| **⚠️ Live code execution** | ❌ | ✅ (Slidev) | ❌ | ✅ (presenterm) | ❌ | **NEW** |

### 3.5 Animations & Transitions

| Feature | PPT/Keynote | reveal.js/Slidev | Beamer | Terminal tools | cats-lote | Status |
|---------|:-----------:|:----------------:|:------:|:-------------:|:---------:|--------|
| Slide transitions | ✅ | ✅ | ✅ | limited | ✅ | ✓ |
| Step-by-step reveals | ✅ | ✅ (fragments) | ✅ (\\pause, \\onslide) | ✅ | ✅ | ✓ |
| **⚠️ Fragment styles (fade, highlight, strike-through)** | ✅ | ✅ | ✅ | ❌ | ❌ | **NEW** |
| **⚠️ Global default transition** | ✅ | ✅ | ✅ | ✅ (presenterm) | ❌ | **NEW** |
| Idle animation | screensaver | ❌ | ❌ | ❌ | ✅ | ✓ (unique!) |

---

## 4. Newly Identified Gaps (from cross-tool analysis)

### 4.1 Theming & Color Schemes
- **What:** A `Theme` object that defines a coherent color palette (background, foreground, accent, dimmed, heading color, border style) applied consistently across all slides.
- **Found in:** Every tool. reveal.js has ~20 built-in themes. Presenterm has `catppuccin`, `dracula`, `gruvbox`, etc.
- **Suggestion:** `SessionBuilder[IO]().withTheme(Theme.Dracula)` or `.withTheme(Theme.custom(bg = ..., fg = ..., accent = ...))`. The theme feeds into layouts, headings, code blocks, and progress bar colors.

### 4.2 Markdown Source Format
- **What:** Define presentations in a `.md` file (with `---` slide separators) rather than Scala code, and load them at runtime.
- **Found in:** Slidev, Marp, presenterm, mdp, lookatme, slides (Charm) — essentially all modern terminal tools.
- **Suggestion:** Provide a `SessionBuilder.fromMarkdown(path)` loader that parses headings, code fences, bullet lists, and `---` separators into slides. The Scala DSL remains available for interactive/custom slides.

### 4.3 Incremental Code Highlighting
- **What:** Show a code block, then on successive steps highlight specific lines (dim the rest, bold/color the focus lines). Essential for "walk through the code" slides.
- **Found in:** Slidev (`{1|3-4|6}` line markers), presenterm (`+highlight` blocks), Beamer (lstlisting `escapechar`).
- **Suggestion:** `.codeBlock(lang, source).highlightLines(1 to 3).thenHighlight(5 to 8)` — integrates with the existing `.step()` mechanism.

### 4.4 Blank / Black Screen
- **What:** Press `B` to instantly black out the screen (audience sees nothing). Press again to restore. Used when the presenter wants full audience attention on their words, not the screen.
- **Found in:** PowerPoint, Keynote, reveal.js, presenterm, slides.
- **Suggestion:** Built-in middleware (like `Idle`) that listens for `Character('b')` and renders a fully blank `ScreenAdjusted`. Trivial to implement.

### 4.5 Keyboard Shortcut Help Overlay
- **What:** Press `?` to show/hide a help panel listing all available keyboard shortcuts.
- **Found in:** reveal.js, slides (Charm), most web tools.
- **Suggestion:** A built-in `Overlay` (like QuickNavigation) toggled by `?`. Shows: arrows = navigate, Esc = quit, N = notes (if enabled), B = blank, ? = this help.

### 4.6 Slide Numbering
- **What:** A small "3/12" indicator, typically in a corner or footer.
- **Found in:** Virtually every tool. Distinct from the progress bar (which is visual-only).
- **Suggestion:** `SessionBuilder[IO]().withSlideNumbers()` — renders as a subtle overlay in the bottom-right, using `Colors.Gray`.

### 4.7 Header / Footer Chrome
- **What:** Persistent text regions at the top/bottom of every slide (company name, date, conference name, slide title).
- **Found in:** PowerPoint masters, Beamer headline/footline, presenterm.
- **Suggestion:** `.withHeader("Company Conf 2026")` and `.withFooter(ctx => s"${ctx.title} | ${ctx.slideNumber}/${ctx.totalSlides}")` on `SessionBuilder`.

### 4.8 Fragment Styles (Fade, Highlight, Strike-Through)
- **What:** When doing step-by-step reveals, previous items can be dimmed (gray), current item highlighted (bold/colored), or items can be struck through. Beyond simple "append next line."
- **Found in:** reveal.js (fragment classes: `fade-in`, `fade-out`, `highlight-red`, `strike`), Beamer (`\alert`, `\only`, `\temporal`).
- **Suggestion:** Extend `.step()` to accept a `StepStyle`: `.step("next point", style = StepStyle.HighlightCurrent)` which dims previous steps when the new one appears.

### 4.9 Global Default Transition
- **What:** Set a transition once for the entire presentation instead of per-slide.
- **Found in:** All tools. PowerPoint: "Apply to All", reveal.js: `transition: 'slide'` in config.
- **Suggestion:** `SessionBuilder[IO]().withDefaultTransition(_.morphTransition())` — applied to any slide that doesn't override with its own.

### 4.10 Hot Reload / Live Preview
- **What:** When the source file changes, the presentation auto-refreshes without restarting.
- **Found in:** Slidev, presenterm (`--watch`), reveal.js (via webpack-dev-server).
- **Suggestion:** For the Markdown loader (`fromMarkdown`), watch the file with `fs2.io.file.Files.watch` and rebuild the presentation. For the Scala DSL, rely on sbt `~run` (already works for recompilation).

### 4.11 Export to PDF
- **What:** Render all slides to a static PDF for sharing, handouts, or archival.
- **Found in:** All traditional tools, Marp, presenterm, Slidev.
- **Suggestion:** `SessionBuilder[IO]().exportPdf(path)` that iterates slides, renders each to its `ScreenAdjusted` string, and writes a fixed-width-font PDF using a library like Apache PDFBox. Each slide becomes one page.

### 4.12 Auto-Advance / Kiosk Mode
- **What:** Slides advance automatically after a configured duration. Used for lobby displays, trade show booths, or timed lightning talks.
- **Found in:** PowerPoint (rehearse timings), reveal.js (`autoSlide: 5000`), Google Slides (publish mode).
- **Suggestion:** `.withAutoAdvance(every = 30.seconds)` or per-slide `.autoAdvance(15.seconds)`. Resets on manual input.

### 4.13 Live Code Execution
- **What:** Run a code snippet on the slide and display its output inline, during the presentation.
- **Found in:** Slidev (Monaco editor + run), presenterm (`+exec` blocks), Jupyter notebooks.
- **Suggestion:** For a Scala tool, integrate with Ammonite or scala-cli: `.executableCode("scala", snippet)` that runs the snippet and appends stdout below the code block. High effort but very impressive for tech talks.

### 4.14 Mermaid / ASCII Diagrams
- **What:** Render diagrams from a text DSL (flowcharts, sequence diagrams, class diagrams).
- **Found in:** Slidev/Marp (mermaid.js), presenterm (renders mermaid to ASCII via external tool).
- **Suggestion:** `.mermaid(source)` that shells out to `mmdc` (mermaid CLI) or a JVM diagramming library, renders to ASCII art, and embeds the result. Alternatively, integrate with `graph-easy` for simpler box-and-arrow diagrams.

### 4.15 Pointer / Cursor Highlight
- **What:** A visible marker (e.g., `>>>` or a colored block) that the presenter can move around the slide with arrow keys to draw attention to specific content.
- **Found in:** PowerPoint (laser pointer), Keynote (pointer), reveal.js (laser plugin).
- **Suggestion:** A toggleable mode (press `P`) that shows a highlighted cursor position on screen, movable with arrow keys. When active, arrows move the pointer instead of navigating slides.

---

## 5. Revised Priority Ranking

| Priority | Feature | Impact | Effort | Category |
|----------|---------|--------|--------|----------|
| 🔴 High | **Slide layouts** | Foundational | Medium | Structure |
| 🔴 High | **Theming / color schemes** | Visual coherence across all slides | Medium | Visual |
| 🔴 High | Inline text formatting / rich markup | Every slide benefits | Medium | Content |
| 🔴 High | Syntax-highlighted code blocks | Primary terminal-presentation use case | High | Content |
| 🔴 High | Speaker notes (dual-terminal) | Core presenter need | Medium | Presenter UX |
| 🔴 High | **Global default transition** | Reduces boilerplate; trivial to implement | Low | Transitions |
| 🟡 Medium | **Blank screen ('B' key)** | Quick win; common presenter need | Low | Presenter UX |
| 🟡 Medium | **Keyboard shortcut help ('?')** | Discoverability | Low | Presenter UX |
| 🟡 Medium | **Slide numbering** | Expected everywhere | Low | Visual |
| 🟡 Medium | **Header / footer chrome** | Professional appearance | Low | Visual |
| 🟡 Medium | **Fragment styles (fade, highlight)** | Richer step-by-step reveals | Medium | Animations |
| 🟡 Medium | **Incremental code highlighting** | Essential for code walkthroughs | Medium | Content |
| 🟡 Medium | Bullet / numbered lists | Very common slide pattern | Low | Content |
| 🟡 Medium | Section headers / heading levels | Visual hierarchy | Low | Content |
| 🟡 Medium | Tables | Data presentation | Medium | Content |
| 🟡 Medium | Callout / admonition boxes | Leverages existing `Symbols` | Low | Content |
| 🟡 Medium | **Markdown source format** | Drastically lowers authoring barrier | High | Authoring |
| 🟢 Low | **Hot reload / live preview** | Developer experience during authoring | Medium | Authoring |
| 🟢 Low | **Export to PDF** | Sharing & archival | Medium | Workflow |
| 🟢 Low | **Auto-advance / kiosk mode** | Niche but useful | Low | Navigation |
| 🟢 Low | Footnotes / annotations | Niche but valuable | Low | Content |
| 🟢 Low | ASCII-art / FIGlet embedding | Polish | Medium | Content |
| 🟢 Low | **Mermaid / ASCII diagrams** | Impressive but complex | High | Content |
| 🟢 Low | **Live code execution** | Wow factor for tech talks | High | Content |
| 🟢 Low | Tooltip / hover annotations | Advanced | High | Content |
| 🟢 Low | **Pointer / cursor highlight** | Niche presenter tool | Medium | Presenter UX |
| 🟢 Low | **Vertical / nested slides** | reveal.js-style; niche for terminal | High | Structure |
| 🟢 Low | **Emoji rendering** | Trivial if terminal supports it | Low | Content |

---

## 6. Summary

The library excels at **structural presentation mechanics**: slide sequencing, transitions, overlays, animation timing, and extensibility via SPIs. Its internal `Symbols` and `Colors` utilities provide the raw building blocks for rich terminal output. The idle animation is a unique differentiator not found in other tools.

However, the **content authoring layer is thin**. Users receive a plain `.content(string)` method and must manually handle all formatting, structure, and annotation.

**Biggest gaps compared to the ecosystem:**
1. **No theming** — every other tool lets you set a color scheme once and have it apply everywhere
2. **No layouts** — forces manual string formatting for every slide
3. **No rich text DSL** — ANSI codes exist internally but aren't exposed ergonomically
4. **No code highlighting** — the #1 use case for a terminal presentation tool
5. **No markdown input** — every modern terminal tool (presenterm, slides, lookatme) uses markdown as its primary format

The quick wins (blank screen, shortcut help, slide numbers, global default transition) could be shipped in a day and would immediately make the tool feel more polished and complete.

---

## Appendix A — Speaker Notes Practical Design (referenced from §2.1)

### How It Would Work

Since cats-lote is a **single-terminal** presentation tool, speaker notes need a design that fits within that constraint. There are two viable approaches:

---

### Option 1: Toggleable Notes Overlay (Recommended)

**User experience:**
- The presenter presses a hotkey (e.g., `N`) to toggle notes visibility.
- When visible, notes appear in a bordered region at the bottom of the screen (3–5 lines), dimmed with gray ANSI styling to visually separate them from slide content.
- The audience-facing mode (notes hidden) is the default.

**DSL usage:**

```scala
SessionBuilder[IO]()
  .withSpeakerNotes()          // enables the notes overlay, toggle with 'N'
  .addTextSlide { _ =>
    _.content("Welcome to the talk!")
      .speakerNotes("Greet the audience. Mention the agenda.")
      .title("Intro")
  }
  .addTextSlide { _ =>
    _.content("Key Architecture Decisions")
      .speakerNotes(
        """- Mention the CAP theorem trade-off
          |- Explain why we chose eventual consistency
          |- 2 min max on this slide""".stripMargin)
      .title("Architecture")
  }
  .run()
```

**Implementation sketch:**

```scala
// 1. Extend TextSlideBuilderDsl to accept notes
trait TextSlideBuilderReady[F[_]] extends SlideMetadataDsl[F, TextSlideBuilderReady[F]] {
  // ...existing methods...
  def speakerNotes(notes: String): TextSlideBuilderReady[F]
}

// 2. Store notes per slide in SlideSpecification
case class SlideSpecification[F[_]](
  slide: Slide[F],
  transition: Option[Transition[F]],
  title: Option[String],
  speakerNotes: Option[String] = None   // <-- new field
)

// 3. Implement as a stateful Overlay (similar to InputStatusOverlay)
object SpeakerNotesOverlay {

  def make[F[_]: Monad: Ref.Make](
      notesBySlideIndex: Map[Int, String]
  ): F[Overlay[F]] =
    for {
      visibleRef    <- Ref[F].of(false)
      slideIndexRef <- Ref[F].of(0)
    } yield new Overlay[F] {

      override def applyOverlay(
          context: Screen,
          screenAdjusted: ScreenAdjusted,
          originalContent: ScreenAdjusted
      ): F[ScreenAdjusted] =
        for {
          visible    <- visibleRef.get
          slideIndex <- slideIndexRef.get
        } yield {
          if (!visible) screenAdjusted
          else {
            val notes = notesBySlideIndex.getOrElse(slideIndex, "(no notes)")
            val width = context.screenWidth
            val border = Symbols.boxThin("horizontal") * (width - 2)
            val notesLines = notes.linesIterator.toVector.take(4).map { line =>
              s"${Colors.Gray} ${line.take(width - 4).padTo(width - 4, ' ')} ${Colors.Reset}"
            }
            val noteBlock = (s"┌$border┐" +: notesLines :+ s"└$border┘").mkString("\n")

            // Replace the bottom N lines of the screen with the notes block
            val lines = screenAdjusted.content.split("\n", -1).toVector
            val replacement = lines.dropRight(notesLines.size + 2) ++ noteBlock.split("\n")
            screenAdjusted.copy(content = replacement.mkString("\n"))
          }
        }

      override def onUserInput(userInput: UserInput)(implicit F: Applicative[F]): F[Unit] =
        userInput match {
          case Character('n') | Character('N') =>
            visibleRef.update(!_)
          case _ => F.unit
        }
    }
}
```

**How the session wires it together:**

The `SessionBuilder` collects `speakerNotes` strings from each slide spec, builds a `Map[Int, String]` of slide-index → notes, passes it to `SpeakerNotesOverlay.make`, and registers it as a middleware overlay. The `onSlideChange` callback (already supported) updates the overlay's current slide index.

---

### Option 2: Dual-Terminal Mode (Advanced) — Full Design

**User experience:**
- The presenter starts the session with `.withPresenterNotes(path)`.
- The library writes structured notes output to a **named pipe or file** that a second terminal `tail -f`s.
- The main terminal shows clean slides (audience-facing); the second terminal shows the current slide's notes, title, progress info, and optionally a preview of the next slide.

**DSL usage:**

```scala
SessionBuilder[IO]()
  .withPresenterNotes("/tmp/lote-notes")  // enables dual-terminal mode
  .addTextSlide { _ =>
    _.content("Welcome!")
      .speakerNotes("Greet audience. 30 seconds max.")
      .title("Intro")
  }
  .addTextSlide { _ =>
    _.content("Architecture Overview")
      .speakerNotes(
        """- CAP theorem trade-off
          |- Why eventual consistency
          |- Show diagram on next slide""".stripMargin)
      .title("Architecture")
  }
  .run()
```

Then in a second terminal (on the presenter's laptop, not projected):
```bash
tail -f /tmp/lote-notes
```

---

#### Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    SessionBuilder                         │
│                                                          │
│  ┌──────────────┐    ┌──────────────────────────────┐   │
│  │ SlideSpecs   │───▶│  PresenterNotesEmitter[F]    │   │
│  │ (with notes) │    │                              │   │
│  └──────────────┘    │  - notesByIndex: Map[Int,Str]│   │
│                      │  - sink: NotesSink[F]        │   │
│  ┌──────────────┐    │  - onSlideChange(idx): F[Unit│   │
│  │ onSlideChange│───▶│                              │   │
│  │ callback     │    └──────────────┬───────────────┘   │
│  └──────────────┘                   │                    │
│                                     ▼                    │
│                        ┌────────────────────────┐        │
│                        │   NotesSink[F]         │        │
│                        │   (algebra / trait)     │        │
│                        └────────────┬───────────┘        │
│                                     │                    │
└─────────────────────────────────────┼────────────────────┘
                                      │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                  ▼
          ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
          │ FileSink     │  │ PipeSink     │  │ TestSink     │
          │ (append file)│  │ (named pipe) │  │ (Ref-based)  │
          └──────────────┘  └──────────────┘  └──────────────┘
```

---

#### Component 1: `NotesSink[F]` — The Output Algebra

A simple trait abstracting where notes are written. This keeps the implementation testable and allows different backends.

```scala
package com.github.morotsman.lote.api.spi

import cats.effect.Resource

/**
 * Abstraction for writing presenter notes to an external channel.
 * Implementations may write to a file, named pipe, socket, or test buffer.
 */
trait NotesSink[F[_]] {
  /** Write a complete notes frame (replaces previous content conceptually). */
  def write(frame: String): F[Unit]

  /** Signal that the presentation has ended. */
  def close: F[Unit]
}

object NotesSink {
  /** Resource that creates a file-based sink, cleaning up on release. */
  def file[F[_]: Async](path: java.nio.file.Path): Resource[F, NotesSink[F]] =
    Resource.make(
      Async[F].delay {
        // Truncate or create the file
        java.nio.file.Files.write(path, Array.emptyByteArray)
        path
      }
    )(_ => Async[F].unit) // file persists after session; presenter can review
      .map { p =>
        new NotesSink[F] {
          override def write(frame: String): F[Unit] = Async[F].delay {
            // Overwrite file content so `tail -f` or `watch cat` shows latest
            java.nio.file.Files.writeString(p, frame)
          }
          override def close: F[Unit] = Async[F].delay {
            java.nio.file.Files.writeString(p, "\n[Presentation ended]\n")
          }
        }
      }
}
```

---

#### Component 2: `PresenterNotesEmitter[F]` — The Coordinator

This component holds the notes data, listens to slide-change events, and formats + writes the notes frame.

```scala
package com.github.morotsman.lote.internal.interpreter.middleware

import cats.Monad
import cats.implicits._
import com.github.morotsman.lote.api.spi.NotesSink

/**
 * Emits formatted presenter notes to an external sink on each slide change.
 */
private[lote] object PresenterNotesEmitter {

  final case class SlideNotes(
      index: Int,
      title: Option[String],
      notes: Option[String]
  )

  def make[F[_]: Monad](
      slides: Vector[SlideNotes],
      sink: NotesSink[F]
  ): PresenterNotesEmitter[F] =
    new PresenterNotesEmitter[F](slides, sink)
}

private[lote] class PresenterNotesEmitter[F[_]: Monad](
    slides: Vector[PresenterNotesEmitter.SlideNotes],
    sink: NotesSink[F]
) {
  import PresenterNotesEmitter._

  /** Called by the session's onSlideChange callback. */
  def onSlideChange(index: Int): F[Unit] = {
    val frame = renderFrame(index)
    sink.write(frame)
  }

  private def renderFrame(index: Int): String = {
    val current = slides.lift(index)
    val next = slides.lift(index + 1)
    val total = slides.length

    val sb = new StringBuilder
    sb.append(separator)
    sb.append(s"\n  SLIDE ${index + 1} / $total")
    current.flatMap(_.title).foreach(t => sb.append(s" — $t"))
    sb.append("\n")
    sb.append(separator)
    sb.append("\n\n")

    // Current slide notes
    current.flatMap(_.notes) match {
      case Some(notes) =>
        sb.append("  NOTES:\n")
        notes.linesIterator.foreach { line =>
          sb.append(s"    $line\n")
        }
      case None =>
        sb.append("  (no notes for this slide)\n")
    }

    sb.append("\n")

    // Next slide preview
    next match {
      case Some(n) =>
        sb.append(s"  NEXT: ${n.title.getOrElse(s"Slide ${index + 2}")}\n")
        n.notes.foreach { nn =>
          sb.append(s"  NEXT NOTES (preview): ${nn.linesIterator.take(2).mkString(" / ")}...\n")
        }
      case None =>
        sb.append("  (last slide)\n")
    }

    sb.append("\n")
    sb.append(separator)
    sb.toString()
  }

  private val separator = "─" * 60
}
```

---

#### Component 3: DSL Extension — `.speakerNotes()` on slides

```scala
// In TextSlideBuilderDsl:
trait TextSlideBuilderReady[F[_]] extends SlideMetadataDsl[F, TextSlideBuilderReady[F]] {
  // ...existing methods...
  def speakerNotes(notes: String): TextSlideBuilderReady[F]
}

// In SlideMetadataDsl (so custom slides can have notes too):
trait SlideMetadataDsl[F[_], Self] {
  // ...existing methods...
  def speakerNotes(notes: String): Self
}
```

---

#### Component 4: Model Extension — `SlideSpecification`

```scala
private[lote] final case class SlideSpecification[F[_]](
    slide: Slide[F],
    out: Option[Transition[F]],
    title: Option[String] = None,
    speakerNotes: Option[String] = None   // <-- new field
)
```

---

#### Component 5: Wiring in `SessionBuilder`

```scala
case class SessionBuilder[F[_]: Async: Ref.Make] private (
    // ...existing fields...
    private val presenterNotesPath: Option[String] = None
) {

  /** Enables presenter notes output to the given file path.
    * Open a second terminal with `tail -f <path>` to see live notes.
    */
  def withPresenterNotes(path: String): SessionBuilder[F] =
    this.copy(presenterNotesPath = Some(path))

  // In run():
  def run(): F[Unit] = {
    require(slideSteps.nonEmpty, "...")

    val sinkResource: Resource[F, Option[NotesSink[F]]] =
      presenterNotesPath match {
        case Some(path) =>
          NotesSink.file[F](java.nio.file.Path.of(path)).map(Some(_))
        case None =>
          Resource.pure(None)
      }

    (NConsoleInterpreter.resource[F](), sinkResource).tupled.use { case (rawConsole, maybeSink) =>
      for {
        ticker <- TickerInterpreter.make[F](tickerInterval)
        // ... existing setup ...
        presentation <- buildPresentation(console, ticker)

        // Build the emitter if notes are enabled
        emitter <- maybeSink.traverse { sink =>
          val slideNotes = presentation.slideSpecifications.zipWithIndex.map {
            case (spec, idx) =>
              PresenterNotesEmitter.SlideNotes(idx, spec.title, spec.speakerNotes)
          }.toVector
          Monad[F].pure(PresenterNotesEmitter.make[F](slideNotes, sink))
        }

        // Compose emitter into the onSlideChange callback chain
        executor <- {
          PresentationExecutorInterpreter.make[F](presentation, { index =>
            // ...existing callbacks (progress, idle, quickNav, user)...
            val notesUpdate = emitter.fold(Monad[F].unit)(_.onSlideChange(index))
            existingCallbacks *> notesUpdate
          })
        }

        // Emit initial frame for slide 0
        _ <- emitter.fold(Monad[F].unit)(_.onSlideChange(0))
        _ <- executor.start()
      } yield ()
    }
  }
}
```

---

#### What the Presenter Sees (second terminal)

```
────────────────────────────────────────────────────────────
  SLIDE 2 / 12 — Architecture
────────────────────────────────────────────────────────────

  NOTES:
    - CAP theorem trade-off
    - Why eventual consistency
    - Show diagram on next slide

  NEXT: Deep Dive
  NEXT NOTES (preview): - Explain the event sourcing pattern / - Demo the replay...

────────────────────────────────────────────────────────────
```

The file is overwritten (not appended) on each slide change, so `tail -f` shows a clean current view. Alternatively, ANSI clear codes could be prepended for terminals that support them.

---

#### Testing

```scala
// TestNotesSink accumulates frames in a Ref for assertions
object TestNotesSink {
  def make[F[_]: Ref.Make: Monad](): F[(NotesSink[F], Ref[F, Vector[String]])] =
    Ref[F].of(Vector.empty[String]).map { ref =>
      val sink = new NotesSink[F] {
        override def write(frame: String): F[Unit] = ref.update(_ :+ frame)
        override def close: F[Unit] = Monad[F].unit
      }
      (sink, ref)
    }
}

// In tests:
for {
  (sink, framesRef) <- TestNotesSink.make[IO]()
  emitter = PresenterNotesEmitter.make[IO](slides, sink)
  _ <- emitter.onSlideChange(0)
  _ <- emitter.onSlideChange(1)
  frames <- framesRef.get
} yield {
  assert(frames(0).contains("SLIDE 1 / 3"))
  assert(frames(1).contains("Architecture"))
}
```

---

#### Trade-offs Revisited

| Aspect | Option 1 (Overlay) | Option 2 (Dual-Terminal) |
|--------|--------------------|-----------------------|
| Setup complexity | None | One extra `tail -f` command |
| Audience safety | Notes visible if toggle pressed | Fully isolated |
| Implementation effort | Low (reuses Overlay SPI) | Medium (new `NotesSink` algebra + wiring) |
| Screen real-estate | Steals bottom lines | Zero impact on main display |
| Testability | Test via `TestConsole` output | Test via `TestNotesSink` Ref |
| Rich notes formatting | Constrained by slide width | Unlimited (own terminal width) |
| Next-slide preview | Hard (needs look-ahead render) | Easy (just text from spec) |

---

### The Audience Visibility Problem

With a single-terminal tool, **any overlay is visible to the audience** if the terminal is projected/shared. This is an inherent constraint — you cannot hide content on a screen that is being mirrored.

However, this is the same limitation faced by most terminal/CLI presentation tools:

| Tool | Same problem? | How they handle it |
|------|--------------|-------------------|
| **reveal.js** | ✅ Solved | Opens a separate browser window for the speaker view (different URL) |
| **Slidev** | ✅ Solved | Same as reveal.js — presenter mode is a separate page |
| **sent** (suckless) | ❌ No notes at all | Simply doesn't support speaker notes |
| **mdp** (terminal) | ❌ No notes at all | No speaker notes feature |
| **lookatme** (terminal) | ❌ No notes at all | No speaker notes feature |
| **PowerPoint/Keynote** | ✅ Solved | macOS/Windows extend display — presenter view on laptop, slides on projector |

The key insight: **web-based tools solve this by opening two windows on two displays**. Terminal tools generally just skip the feature entirely.

For cats-lote, the practical options are:

1. **Option 2 (dual-terminal) is the only truly hidden approach** — the presenter has a second terminal on their laptop screen while projecting the main terminal. This mirrors how PowerPoint/Keynote work with extended displays.

2. **Option 1 (overlay) is useful for rehearsal and solo practice** — you can study your notes while preparing, then present without toggling. It's still valuable, just not for live "hidden notes."

3. **A hybrid approach** works well: support both. Use the overlay during practice; use the file-based output during live talks.

### Recommendation

**Implement both options**, but for different use cases:

- **Option 1 (overlay):** Ship first. Low effort, useful for rehearsal, and demonstrates the concept. Label it as "practice mode" in docs.
- **Option 2 (dual-terminal):** The real "presenter mode." Required for actually hiding notes from an audience. The design above introduces only 3 new components (`NotesSink` trait, `PresenterNotesEmitter`, and the `SlideSpecification` field), wires into the existing `onSlideChange` callback, and is fully testable via `TestNotesSink`.

The honest reality is that **no single-screen terminal tool can hide content from the audience**. Any tool that claims to have speaker notes in a single projected terminal has the same problem. The dual-terminal/dual-display approach is the only genuine solution, and it's exactly what the established tools do (just with browser windows or OS-level display routing instead of terminal panes).
