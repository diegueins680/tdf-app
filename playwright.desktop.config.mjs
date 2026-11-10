import { defineConfig } from '@playwright/test';

// Electron shell journeys. The hosted web application is the production build
// of tdf-hq-ui served from a loopback origin; the TDF API is synthetic and
// intercepted inside the shell's browser context. No request reaches a live
// service. Run through `xvfb-run -a` on headless Linux.
const artifactRoot = process.env.PLAYWRIGHT_ARTIFACT_DIR || 'artifacts/desktop-playwright';
const port = Number(process.env.PLAYWRIGHT_PORT ?? 4173);
if (!Number.isSafeInteger(port) || port < 1024 || port > 65535) throw new Error('Invalid PLAYWRIGHT_PORT');

export default defineConfig({
  testDir: './e2e/desktop',
  captureGitInfo: { diff: false },
  fullyParallel: false,
  forbidOnly: Boolean(process.env.CI),
  retries: 0,
  workers: 1,
  timeout: 90_000,
  expect: { timeout: 10_000 },
  outputDir: `${artifactRoot}/test-results`,
  reporter: [
    ['line'],
    ['json', { outputFile: `${artifactRoot}/results.json` }],
    ['html', { outputFolder: `${artifactRoot}/html`, open: 'never' }],
  ],
  use: {
    baseURL: `http://127.0.0.1:${port}`,
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
  },
  webServer: {
    command: `npm run build:e2e --workspace=tdf-hq-ui && npm run preview:e2e --workspace=tdf-hq-ui -- --port ${port}`,
    url: `http://127.0.0.1:${port}/inicio`,
    reuseExistingServer: !process.env.CI,
    timeout: 300_000,
    env: {
      VITE_API_BASE: 'http://127.0.0.1:19099',
      VITE_TZ: 'America/Guayaquil',
      // The shell hides Google sign-in; the build must have it configured so the test is meaningful.
      VITE_GOOGLE_CLIENT_ID: 'synthetic-google-client',
    },
  },
});
