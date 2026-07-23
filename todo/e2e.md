# E2E Testing Investigation

## Current State

The project already has a solid **unit/integration testing** foundation:
- **MUnit + Cats Effect** as the test framework
- **SlideTestHarness** with `TestConsole`, `TestTicker`, `SimulatedClock` test doubles
- **SessionBuilder.runWith()** pattern for full presentation runs against test doubles
- Pre-loaded inputs, deterministic timing, frame recording/assertion

These tests verify logic and rendered frames **in-memory** but do not exercise the actual terminal rendering (JLine) or the browser/WebGL rendering (Three.js).

---

## What Would True E2E Tests Add?

| Layer | Currently Tested | E2E Would Cover |
|-------|-----------------|-----------------|
| Slide logic | ✅ | — |
| Transitions/animations | ✅ (frames) | Actual timing/smoothness |
| Terminal rendering (JLine) | ❌ | ANSI escape codes, cursor, resize |
| Browser rendering (Three.js) | ❌ | WebGL canvas output, camera, DOM |
| User input (real keyboard) | ❌ | Key events end-to-end |
| Cross-platform parity | ❌ | Same presentation looks correct on both |

---

## Option A: Terminal E2E Tests (JVM)

### Approach 1: Pseudo-terminal (PTY) Harness

Use a PTY to spawn the JVM presentation process and interact with it programmatically.

**Tools/Libraries:**
- [pty4j](https://github.com/JetBrains/pty4j) — JetBrains PTY library for JVM
- [expect4j](https://github.com/cverges/expect4j) — Expect-style automation
- Raw `ProcessBuilder` + piped stdin/stdout with ANSI parsing

**How it works:**
1. Launch the presentation as a subprocess (`sbt "examples/run"` or a compiled jar)
2. Send keystrokes via PTY stdin
3. Read rendered frames from PTY stdout (raw ANSI escape sequences)
4. Assert on expected content appearing in the terminal buffer

**Pros:**
- Tests the full stack including JLine rendering
- Runs in CI without a display server
- Can validate ANSI output correctness

**Cons:**
- Parsing raw ANSI output is complex
- Timing-sensitive (need retries/polling)
- Platform-specific PTY behavior (macOS vs Linux)

### Approach 2: Virtual Terminal Emulator in Tests

Use a terminal emulator library to interpret ANSI output into a screen buffer.

**Tools/Libraries:**
- [lanterna](https://github.com/mabe02/lanterna) — has a virtual terminal
- Custom ANSI parser + grid buffer (lightweight)
- [vt100-emulator](https://github.com/pexpect/pexpect) (Python, could use via subprocess)

**How it works:**
1. Pipe presentation output through a virtual terminal emulator
2. Query the emulator's screen buffer for content at specific positions
3. Assert on cell content, colors, cursor position

**Estimated effort:** Medium (2–4 days for PTY harness + ANSI parser + first tests)

---

## Option B: Browser E2E Tests (Scala.js / WebGL)

### Approach 1: Playwright (Recommended)

**Why Playwright:**
- First-class support for headless Chrome/Firefox/WebKit
- Can run in CI (GitHub Actions has built-in support)
- Rich API: screenshots, DOM queries, keyboard/mouse simulation
- WebGL canvas screenshot comparison for visual regression

**Setup:**
```
npm init -y
npm install --save-dev @playwright/test
npx playwright install
```

**Test flow:**
1. Build: `sbt browserExamples/fastLinkJS`
2. Serve: Start a local HTTP server (e.g., `http-server` or express)
3. Test: Playwright opens the page, waits for canvas/WebGL to render
4. Assert:
   - DOM element `#terminal` exists
   - Canvas has non-zero dimensions
   - Screenshot comparison (visual regression)
   - Keyboard input triggers slide transitions
   - Navigation through all slides completes without errors

**Example test structure:**
```typescript
// e2e/browser.spec.ts
import { test, expect } from '@playwright/test';

test('presentation loads and renders', async ({ page }) => {
  await page.goto('http://localhost:8080/simple.html');
  await page.waitForSelector('canvas');
  const canvas = page.locator('canvas');
  await expect(canvas).toBeVisible();
  // Screenshot baseline comparison
  await expect(page).toHaveScreenshot('initial-slide.png', { maxDiffPixels: 100 });
});

test('keyboard navigation works', async ({ page }) => {
  await page.goto('http://localhost:8080/simple.html');
  await page.waitForSelector('canvas');
  await page.keyboard.press('ArrowRight');
  await page.waitForTimeout(2000); // transition time
  await expect(page).toHaveScreenshot('second-slide.png', { maxDiffPixels: 100 });
});

test('presentation completes without console errors', async ({ page }) => {
  const errors: string[] = [];
  page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
  await page.goto('http://localhost:8080/simple.html');
  await page.waitForTimeout(5000);
  expect(errors).toHaveLength(0);
});
```

**Estimated effort:** Low–Medium (1–2 days for setup + first tests)

### Approach 2: Cypress

Similar to Playwright but heavier. Less ideal for WebGL (limited canvas support). Not recommended over Playwright for this project.

### Approach 3: Selenium WebDriver via Scala

Could write browser tests in Scala using selenium-java, keeping everything in the sbt build.

**Pros:** Same language, same build tool
**Cons:** More verbose API, slower, less WebGL support than Playwright

---

## Option C: Combined Terminal + Browser E2E (Cross-Platform Parity)

### Visual Regression Comparison

1. Run the **same shared presentation** on both platforms
2. Capture terminal output (as rendered text grid) and browser output (as screenshot)
3. Compare key slides for content parity (not pixel-perfect, but content-equivalent)

This validates that `SharedAdvancedPresentation` renders correctly on both backends.

---

## Recommended Plan

### Phase 1: Browser E2E with Playwright (Quickest Win) ✅ DONE
- [x] Add `e2e/` directory with `package.json` + Playwright config
- [x] Write test that builds Scala.js, serves HTML, validates rendering
- [x] Add screenshot baselines for visual regression
- [x] Add keyboard navigation tests
- [x] CI integration (GitHub Actions)

**Note:** Corporate Artifactory doesn't proxy `@playwright` packages — added `e2e/.npmrc` pointing to public registry.

### Phase 2: Terminal E2E with PTY
- [ ] Add PTY4J dependency (test scope only)
- [ ] Create `TerminalE2EHarness` that spawns presentation in PTY
- [ ] Implement basic ANSI screen buffer parser
- [ ] Write tests for: startup, navigation, exit
- [ ] CI integration

### Phase 3: Cross-Platform Parity
- [ ] Shared test scenarios (same input sequence)
- [ ] Content comparison between terminal buffer and browser DOM/canvas
- [ ] Automated regression detection

---

## Open Questions

1. **CI environment** — Do we have GitHub Actions or another CI? (affects browser install strategy)
2. **Visual regression tolerance** — How much pixel drift is acceptable for WebGL screenshots across runs?
3. **Test scope** — Should e2e tests cover all example presentations or just a smoke test on `simple.html`?
4. **Performance budget** — Should e2e tests assert on frame timing/FPS?
5. **Terminal size** — Should PTY tests use a fixed terminal size (e.g., 80×24) for deterministic output?

---

## Effort Summary

| Phase | Effort | Value |
|-------|--------|-------|
| Browser E2E (Playwright) | 1–2 days | High — catches rendering/JS errors |
| Terminal E2E (PTY) | 2–4 days | Medium — catches JLine/ANSI issues |
| Cross-platform parity | 1–2 days | Low–Medium — catches divergence |

**Total estimated effort: ~5–8 days for full coverage.**

