import { defineConfig, devices } from '@playwright/test';
import base from './playwright.artist-follow.config.mjs';
export default defineConfig({
  ...base,
  testIgnore: [],
  testMatch: 'community-discovery.spec.mjs',
  outputDir: 'artifacts/discovery-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/discovery-playwright/results.json' }]],
  use: { ...base.use, baseURL: 'http://127.0.0.1:4194' },
  projects: [...base.projects, { name: 'chromium-tablet', use: { browserName: 'chromium', viewport: { width: 834, height: 1194 }, hasTouch: true, isMobile: true, deviceScaleFactor: 2 } }, { name: 'webkit-iphone', use: { ...devices['iPhone 13'] } }, { name: 'android-small', use: { ...devices['Pixel 5'], viewport: { width: 360, height: 740 } } }],
  webServer: { ...base.webServer, command: `${process.env.TDF_DISCOVERY_SKIP_BUILD === '1' ? '' : 'npm run build:e2e --workspace=tdf-hq-ui && '}npm run preview --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4194 --strictPort`, url: 'http://127.0.0.1:4194', env: { VITE_API_BASE: 'http://127.0.0.1:4194', VITE_POSTHOG_KEY: '', VITE_GOOGLE_CLIENT_ID: '' } },
});
