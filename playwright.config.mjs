import { defineConfig, devices } from '@playwright/test';

const artifactRoot = process.env.PLAYWRIGHT_ARTIFACT_DIR || 'artifacts/persona-playwright';

export default defineConfig({
  testDir: './e2e/web',
  // These suites require their own synthetic-media or API/S3 lifecycle and
  // run from playwright.music-native/-integration.config.mjs instead.
  testIgnore: [/music-player-native\.spec\.mjs$/, /music-player-integration\.spec\.mjs$/],
  // Playwright's git-info plugin buffers the whole `git diff <prBase> HEAD`
  // into one string before truncating it. This repository's merge commits span
  // tens of thousands of files, which exceeds V8's maximum string length and
  // crashes the run. Commit metadata is still captured.
  captureGitInfo: { diff: false },
  fullyParallel: false,
  forbidOnly: Boolean(process.env.CI),
  retries: 0,
  workers: process.env.CI ? 2 : 1,
  timeout: 30_000,
  expect: { timeout: 8_000 },
  outputDir: `${artifactRoot}/test-results`,
  reporter: [
    ['line'],
    ['json', { outputFile: `${artifactRoot}/results.json` }],
    ['html', { outputFolder: `${artifactRoot}/html`, open: 'never' }],
  ],
  use: {
    baseURL: 'http://127.0.0.1:4173',
    locale: 'es-EC',
    timezoneId: 'America/Guayaquil',
    colorScheme: 'dark',
    reducedMotion: 'reduce',
    screenshot: 'only-on-failure',
    trace: 'retain-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    // Synthetic browser fixtures exercise the enabled workflow; production defaults off.
    env: {
      VITE_ACCOUNT_DELETION_FORM_ENABLED: process.env.PLAYWRIGHT_ACCOUNT_DELETION_FORM_ENABLED === 'false' ? 'false' : 'true',
      VITE_ACCOUNT_DELETION_QUEUE_ENABLED: 'true',
    },
    command: 'npm run build:e2e --workspace=tdf-hq-ui && npm run preview:e2e --workspace=tdf-hq-ui',
    url: 'http://127.0.0.1:4173/inicio',
    reuseExistingServer: !process.env.CI,
    timeout: 300_000,
  },
  projects: [
    { name: 'chromium-desktop', use: { ...devices['Desktop Chrome'] } },
    { name: 'chromium-phone', use: { ...devices['Pixel 7'] } },
    {
      name: 'chromium-tablet',
      use: {
        browserName: 'chromium',
        viewport: { width: 834, height: 1194 },
        deviceScaleFactor: 2,
        hasTouch: true,
        isMobile: true,
      },
    },
    { name: 'firefox-critical', grep: /@critical/, use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit-critical', grep: /@critical/, use: { ...devices['Desktop Safari'] } },
  ],
});
