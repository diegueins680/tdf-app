import { defineConfig } from '@playwright/test';

const origin = process.env.TDF_TICKET_JOURNEY_UI_ORIGIN ?? '';
if (!/^http:\/\/127\.0\.0\.1:\d+$/.test(origin)) throw new Error('Use scripts/run-ticket-retrieval-browser.mjs');
export default defineConfig({
  testDir: './e2e/integration', testMatch: 'ticket-retrieval.spec.mjs',
  workers: 1, retries: 0, timeout: 120_000,
  expect: { timeout: 20_000 },
  projects: [{ name: 'chromium-phone', use: { browserName: 'chromium', viewport: { width: 360, height: 800 } } }],
  use: { baseURL: origin, serviceWorkers: 'block', trace: 'retain-on-failure', screenshot: 'only-on-failure' },
  outputDir: 'artifacts/ticket-retrieval/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/ticket-retrieval/results.json' }]],
});
