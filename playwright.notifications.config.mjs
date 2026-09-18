import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';
export default defineConfig({ ...base, testMatch: '**/notification-navigation.spec.mjs',
  outputDir: '/tmp/tdf-notification-browser-results',
  reporter: [['line'], ['json', { outputFile: '/tmp/tdf-notification-browser-results.json' }]],
  use: { ...base.use, baseURL: 'http://127.0.0.1:4187' },
  webServer: { command: 'npm run build:e2e --workspace=tdf-hq-ui && npm exec --workspace=tdf-hq-ui -- vite preview --host 127.0.0.1 --port 4187 --strictPort',
    url: 'http://127.0.0.1:4187/inicio', reuseExistingServer: false, timeout: 300000 },
});
