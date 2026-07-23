import { test, expect, Page } from '@playwright/test';

/**
 * E2E happy-flow test for the Simple Animations example.
 *
 * Strategy for animated vs static content:
 *   - Static text slides: screenshot comparison (deterministic)
 *   - Animated slides: verify canvas pixels CHANGE over time (proves animation runs)
 *   - Transitions: verify no errors and canvas remains active
 *   - All slides: no console errors, no crashes
 *
 * The ExampleLauncher routes `#simple` to SharedSimpleExamplesPresentation
 * which has 14 slides (indices 0–13):
 *   0: Intro (static text)
 *   1: SimpleCounterSlide (animated counter)
 *   2: Counter recap (static text)
 *   3: SimpleGlideSlide (animated ball)
 *   4: Glide recap (static text)
 *   5: SimpleStaticMarkerScrollSlide (animated scroll)
 *   6: SimpleScrollSlide (animated scroll + orbit)
 *   7: Scroll recap (static text) → wipe transition
 *   8: Wipe recap (static text) → sweep transition
 *   9: Sweep recap (static text) → eased wipe transition
 *  10: Eased wipe recap (static text) → typewriter transition
 *  11: Typewriter recap (static text) → glide sweep transition
 *  12: Sweep glide recap (static text)
 *  13: Done (static text)
 */

const SIMPLE_URL = '/simple.html#simple';
const CANVAS_INIT_TIMEOUT = 15_000;
const TRANSITION_WAIT = 3_000;
const ANIMATION_SAMPLE_INTERVAL = 500; // ms between pixel samples

// ─── Helpers ────────────────────────────────────────────────────────────────

/** Take a screenshot of the canvas and return its pixel data as a Buffer. */
async function captureCanvasPixels(page: Page): Promise<Buffer> {
  const canvas = page.locator('#terminal canvas');
  return await canvas.screenshot();
}

/** Verify that the canvas is actively animating by comparing two frames.
 *  Takes multiple samples with increasing intervals to handle headless WebGL timing. */
async function assertCanvasIsAnimating(page: Page) {
  const frame1 = await captureCanvasPixels(page);
  // Try a few samples — headless WebGL may need more time between frames
  for (const wait of [500, 1000, 1500]) {
    await page.waitForTimeout(wait);
    const frame2 = await captureCanvasPixels(page);
    if (!frame1.equals(frame2)) return; // success — animation detected
  }
  // If we get here, none of the samples differed
  expect(false).toBe(true); // fail with a clear message
}

/** Verify that pressing a key changes the canvas output. */
async function assertKeyChangesCanvas(page: Page, key: string) {
  const before = await captureCanvasPixels(page);
  await page.keyboard.press(key);
  await page.waitForTimeout(ANIMATION_SAMPLE_INTERVAL);
  const after = await captureCanvasPixels(page);
  expect(before.equals(after)).toBe(false);
}

/** Verify that the canvas has rendered something (not all black/empty). */
async function assertCanvasHasContent(page: Page) {
  const pixels = await captureCanvasPixels(page);
  // A non-trivial PNG will be larger than a blank canvas
  // (a fully black 1280x720 PNG compresses to ~2-3KB, rendered text is much larger)
  expect(pixels.length).toBeGreaterThan(5000);
}

/** Navigate forward one slide and wait for transition to complete. */
async function navigateForward(page: Page) {
  await page.keyboard.press('ArrowRight');
  await page.waitForTimeout(TRANSITION_WAIT);
}

// ─── Tests ──────────────────────────────────────────────────────────────────

test.describe('Simple Animations — Startup', () => {

  test('loads and renders a WebGL canvas', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });

    const box = await canvas.boundingBox();
    expect(box).not.toBeNull();
    expect(box!.width).toBeGreaterThan(0);
    expect(box!.height).toBeGreaterThan(0);
  });

  test('no console errors on startup', async ({ page }) => {
    const errors: string[] = [];
    page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    expect(errors).toHaveLength(0);
  });
});

test.describe('Simple Animations — Static Slides (screenshot)', () => {

  test('slide 0: intro text', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(3000);

    await expect(page).toHaveScreenshot('slide-00-intro.png', {
      maxDiffPixelRatio: 0.05,
    });
  });

  test('slide 2: counter recap text', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate: intro → counter → counter recap
    await navigateForward(page);
    await navigateForward(page);

    await expect(page).toHaveScreenshot('slide-02-counter-recap.png', {
      maxDiffPixelRatio: 0.05,
    });
  });

  test('slide 13: done text', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate to the last slide
    for (let i = 0; i < 13; i++) {
      await navigateForward(page);
    }

    await expect(page).toHaveScreenshot('slide-13-done.png', {
      maxDiffPixelRatio: 0.05,
    });
  });
});

test.describe('Simple Animations — Animated Slides (pixel change)', () => {

  test('slide 1: counter responds to W/S key presses', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    await navigateForward(page); // → counter slide

    // Counter only changes when the user presses W (increment) or S (decrement)
    await assertKeyChangesCanvas(page, 'w');
  });

  test('slide 3: glide slide is animating', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // intro → counter → recap → glide
    for (let i = 0; i < 3; i++) await navigateForward(page);

    await assertCanvasIsAnimating(page);
  });

  test('slide 5: static marker scroll is animating', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    for (let i = 0; i < 5; i++) await navigateForward(page);

    await assertCanvasIsAnimating(page);
  });

  test('slide 6: orbiting scroll is animating', async ({ page }) => {
    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    for (let i = 0; i < 6; i++) await navigateForward(page);

    await assertCanvasIsAnimating(page);
  });
});

test.describe('Simple Animations — Transitions (no crash)', () => {

  test('wipe transition plays without error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate to slide 7 (scroll recap) which has wipe transition on exit
    for (let i = 0; i < 7; i++) await navigateForward(page);

    // Trigger the wipe transition → next slide
    await navigateForward(page);

    expect(errors).toHaveLength(0);
    await assertCanvasHasContent(page);
  });

  test('sweep transition plays without error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate to slide 8 (wipe recap with sweep transition)
    for (let i = 0; i < 8; i++) await navigateForward(page);

    // Trigger sweep transition
    await navigateForward(page);

    expect(errors).toHaveLength(0);
    await assertCanvasHasContent(page);
  });

  test('typewriter transition plays without error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate to slide 10 (eased wipe recap with typewriter transition)
    for (let i = 0; i < 10; i++) await navigateForward(page);

    // Trigger typewriter transition
    await navigateForward(page);

    expect(errors).toHaveLength(0);
    await assertCanvasHasContent(page);
  });

  test('glide sweep transition plays without error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Navigate to slide 11 (typewriter recap with glide sweep transition)
    for (let i = 0; i < 11; i++) await navigateForward(page);

    // Trigger glide sweep transition
    await navigateForward(page);

    expect(errors).toHaveLength(0);
    await assertCanvasHasContent(page);
  });
});

test.describe('Simple Animations — Full Navigation', () => {

  test('can navigate through all 14 slides without crashes', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    for (let i = 0; i < 13; i++) {
      await navigateForward(page);
    }

    expect(errors).toHaveLength(0);
    await expect(canvas).toBeVisible();
  });

  test('can navigate backwards without crashes', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.goto(SIMPLE_URL);
    const canvas = page.locator('#terminal canvas');
    await expect(canvas).toBeVisible({ timeout: CANVAS_INIT_TIMEOUT });
    await page.waitForTimeout(2000);

    // Forward 3 slides
    for (let i = 0; i < 3; i++) await navigateForward(page);

    // Backward 2 slides
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(TRANSITION_WAIT);
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(TRANSITION_WAIT);

    expect(errors).toHaveLength(0);
    await expect(canvas).toBeVisible();
  });
});
