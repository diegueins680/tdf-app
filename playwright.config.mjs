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
    {
      // Redmi-class Android Chrome (360 CSS px wide), where the white-screen,
      // squeezed-column and hidden-cart reports were observed (2026-10-07).
      name: 'android-chrome-small',
      grep: /@mobile-flow/,
      use: {
        browserName: 'chromium',
        viewport: { width: 360, height: 740 },
        screen: { width: 360, height: 800 },
        deviceScaleFactor: 2.75,
        isMobile: true,
        hasTouch: true,
        userAgent: 'Mozilla/5.0 (Linux; Android 13; 22111317G) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Mobile Safari/537.36',
      },
    },
    { name: 'webkit-iphone', grep: /@mobile-flow/, use: { ...devices['iPhone 13'] } },
    { name: 'firefox-critical', grep: /@critical/, use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit-critical', grep: /@critical/, use: { ...devices['Desktop Safari'] } },
  ],
});
