import { defineConfig, devices } from '@playwright/test';

const artifactRoot = process.env.PLAYWRIGHT_ARTIFACT_DIR || 'artifacts/persona-playwright';
const requestedPort = Number.parseInt(process.env.PLAYWRIGHT_PORT ?? '4173', 10);
const serverPort = Number.isSafeInteger(requestedPort) && requestedPort >= 1024 && requestedPort <= 65_535
  ? requestedPort
  : 4173;
const serverUrl = `http://127.0.0.1:${serverPort}`;

export default defineConfig({
  testDir: './e2e/web',
  // These suites require their own synthetic-media or API/S3 lifecycle.
  testIgnore: [/music-player-native\.spec\.mjs$/, /music-player-integration\.spec\.mjs$/],
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
    baseURL: serverUrl,
    locale: 'es-EC',
    timezoneId: 'America/Guayaquil',
    colorScheme: 'dark',
    reducedMotion: 'reduce',
    screenshot: 'only-on-failure',
    trace: 'retain-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    command: `npm run dev --workspace=tdf-hq-ui -- --host 127.0.0.1 --port ${serverPort}`,
    url: `${serverUrl}/inicio`,
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
