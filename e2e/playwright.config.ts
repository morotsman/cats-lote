import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 4 : undefined, // undefined = use half of CPU cores
  reporter: process.env.CI ? 'github' : 'html',
  timeout: 60_000,
  snapshotPathTemplate: '{testDir}/{testFileDir}/{testFileName}-snapshots/{arg}{ext}',

  use: {
    baseURL: 'http://localhost:8080',
    trace: 'on-first-retry',
    video: 'on-first-retry',
  },

  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],

  /* Start the HTTP server before tests run */
  webServer: {
    command: 'npx http-server ../browser-examples -p 8080 --silent',
    port: 8080,
    reuseExistingServer: !process.env.CI,
    timeout: 10_000,
  },
});

