import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';

const port = Number(process.env.PLAYWRIGHT_PORT ?? 4173);
if (!Number.isSafeInteger(port) || port < 1024 || port > 65535) throw new Error('Invalid PLAYWRIGHT_PORT');

export default defineConfig({
  ...base,
  testIgnore: [],
  testMatch: /music-player(?:-native)?\.spec\.mjs/,
  globalSetup: './e2e/web/fixtures/native-music-setup.mjs',
  workers: 1,
  retries: 0,
  timeout: 90_000,
  use: { ...base.use, baseURL: `http://127.0.0.1:${port}`, serviceWorkers: 'block' },
  webServer: {
    ...base.webServer,
    command: `npm run build:e2e --workspace=tdf-hq-ui && npm run preview:e2e --workspace=tdf-hq-ui -- --port ${port}`,
    url: `http://127.0.0.1:${port}/inicio`,
    reuseExistingServer: false,
    env: { VITE_API_BASE: 'http://127.0.0.1:19099', VITE_TZ: 'America/Guayaquil' },
  },
});
