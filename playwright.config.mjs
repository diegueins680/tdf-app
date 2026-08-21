import { defineConfig, devices } from '@playwright/test';

const artifactRoot = process.env.PLAYWRIGHT_ARTIFACT_DIR || 'artifacts/persona-playwright';

export default defineConfig({
  testDir: './e2e/web',
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
    command: 'npm run dev --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4173',
    url: 'http://127.0.0.1:4173/inicio',
    reuseExistingServer: !process.env.CI,
    timeout: 120_000,
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
