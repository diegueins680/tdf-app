import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';

const origin = process.env.EVENT_RACI_UI_ORIGIN ?? '';
if (process.env.EVENT_RACI_DISPOSABLE_BROWSER_TEST !== '1'
  || !/^http:\/\/127\.0\.0\.1:\d+$/.test(origin)) {
  throw new Error('Run sh scripts/test-event-raci-browser.sh; do not reuse an existing server.');
}
export default defineConfig({
  ...base, testDir: './e2e/integration', testMatch: 'event-raci-browser.spec.mjs',
  workers: 1, retries: 0, timeout: 60_000, webServer: undefined,
  projects: base.projects.filter(project => ['chromium-desktop', 'chromium-phone'].includes(project.name)),
  use: { ...base.use, baseURL: origin, serviceWorkers: 'block' },
  outputDir: 'artifacts/event-raci-browser/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/event-raci-browser/results.json' }]],
});
