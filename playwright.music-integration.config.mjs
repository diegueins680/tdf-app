import assert from 'node:assert/strict';
import { defineConfig } from '@playwright/test';
import base from './playwright.config.mjs';

assert.match(process.env.TDF_MUSIC_BROWSER_API ?? '', /^http:\/\/127\.0\.0\.1:\d+$/);
assert.match(process.env.TDF_MUSIC_BROWSER_STORAGE ?? '', /^https:\/\/127\.0\.0\.1:\d+$/);
assert.equal(process.env.PLAYWRIGHT_PORT, '4187');

export default defineConfig({
  ...base,
  testIgnore: [],
  testMatch: /music-player-integration\.spec\.mjs/,
  workers: 1, retries: 0, timeout: 90_000,
  use: { ...base.use, serviceWorkers: 'block',
    // Ephemeral self-signed MinIO certificate, loopback destinations only.
    // This is NOT browser trust-chain/CDN verification; Node/curl verify the CA.
    ignoreHTTPSErrors: true, trace: 'off', video: 'off' },
  webServer: { ...base.webServer, command: `${base.webServer.command} --strictPort`,
    reuseExistingServer: false,
    env: { VITE_API_BASE: process.env.TDF_MUSIC_BROWSER_API, VITE_TZ: 'America/Guayaquil',
      VITE_PAYPAL_CLIENT_ID: '', VITE_GOOGLE_CLIENT_ID: '' } },
});
