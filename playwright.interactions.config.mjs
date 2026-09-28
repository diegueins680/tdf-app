import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';
export default defineConfig({ ...base, testDir: './e2e/interactions', testMatch: '**/interactions.spec.mjs', workers: 1, timeout: 60000,
  outputDir: '/private/tmp/tdf-interaction-browser-results',
  reporter: [['line'], ['json', { outputFile: '/private/tmp/tdf-interaction-browser-results.json' }]],
  use: { ...base.use, baseURL: 'http://127.0.0.1:4188' },
  webServer: { command: 'npm run build:e2e --workspace=tdf-hq-ui && npm exec --workspace=tdf-hq-ui -- vite preview --host 127.0.0.1 --port 4188 --strictPort',
    url: 'http://127.0.0.1:4188/inicio', reuseExistingServer: false, timeout: 300000 },
});
