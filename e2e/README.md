# E2E Browser Tests

End-to-end tests for cats-lote browser examples using [Playwright](https://playwright.dev/).

## Prerequisites

- Node.js 20+
- sbt (for building Scala.js)
- Playwright browsers installed

## Setup

```bash
cd e2e
npm install
npx playwright install chromium
```

## Running Tests

First, build the Scala.js output:

```bash
sbt browserExamples/fastLinkJS
```

Then run the tests (the HTTP server starts automatically via Playwright's `webServer` config):

```bash
cd e2e
npx playwright test
```

### Useful commands

```bash
# Run with browser visible
npx playwright test --headed

# Run Playwright UI mode (interactive)
npx playwright test --ui

# Update screenshot baselines
npx playwright test --update-snapshots

# Run a specific test file
npx playwright test tests/simple-animations.spec.ts
```

## Test Structure

- `tests/simple-animations.spec.ts` — Happy-flow test for the Simple Animations example
  - Validates WebGL canvas renders
  - Checks no console errors occur
  - Navigates through all slides via keyboard
  - Visual regression screenshots

## Visual Regression

Screenshot baselines are stored in `tests/simple-animations.spec.ts-snapshots/`.
These are platform-specific (Linux in CI, macOS locally).

To update baselines after intentional visual changes:

```bash
npx playwright test --update-snapshots
```

## CI

Tests run automatically on push/PR via GitHub Actions (`.github/workflows/e2e.yml`).

