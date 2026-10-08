import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';

export default defineConfig({
  ...base,
  testIgnore: [],
  testMatch: /music-player(?:-native)?\.spec\.mjs/,
  globalSetup: './e2e/web/fixtures/native-music-setup.mjs',
  workers: 1,
  retries: 0,
  timeout: 90_000,
  use: { ...base.use, serviceWorkers: 'block' },
  webServer: {
    ...base.webServer,
    command: `${base.webServer.command} --strictPort`,
    reuseExistingServer: false,
    env: { VITE_API_BASE: 'http://127.0.0.1:19099', VITE_TZ: 'America/Guayaquil' },
  },
});
