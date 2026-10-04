import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';

// Disposable local browser-fixture run: never reuse another checkout's dev server.
export default defineConfig({
  ...base,
  testMatch: 'artist-follow.spec.mjs',
  workers: 1,
  timeout: 60_000,
  outputDir: 'artifacts/artist-follow-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/artist-follow-playwright/results.json' }]],
  use: { ...base.use, baseURL: 'http://127.0.0.1:4191' },
  projects: base.projects.filter((project) => ['chromium-desktop', 'chromium-phone'].includes(project.name)),
  webServer: {
    command: 'npm run dev --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4191 --strictPort',
    url: 'http://127.0.0.1:4191',
    env: { VITE_API_BASE: 'http://127.0.0.1:4191', VITE_POSTHOG_KEY: '', VITE_GOOGLE_CLIENT_ID: '' },
    reuseExistingServer: false,
    timeout: 180_000,
  },
});
