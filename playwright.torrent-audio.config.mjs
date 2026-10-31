import { defineConfig, devices } from '@playwright/test';
const artifacts = process.env.PLAYWRIGHT_ARTIFACT_DIR || 'artifacts/torrent-audio-browser';
export default defineConfig({
  testDir: './e2e/torrent-audio', testMatch: '*.spec.mjs',
  captureGitInfo: { diff: false }, fullyParallel: false, workers: 1, retries: 0,
  forbidOnly: Boolean(process.env.CI), timeout: 45_000,
  globalSetup: './e2e/torrent-audio/setup.mjs',
  outputDir: `${artifacts}/test-results`,
  reporter: [['line'], ['json', { outputFile: `${artifacts}/results.json` }]],
  use: { trace: 'retain-on-failure', screenshot: 'only-on-failure' },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
    { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit', use: { ...devices['Desktop Safari'] } },
  ],
});
