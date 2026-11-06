import { defineConfig, devices } from '@playwright/test';
export default defineConfig({
  testDir: './e2e/web', testMatch: 'storage-upload-recovery.spec.mjs', workers: 1,
  timeout: 45_000, retries: 0, captureGitInfo: { diff: false },
  outputDir: 'artifacts/storage-upload/test-results', reporter: [['line']],
  use: { launchOptions: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE ? { executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE } : {}, baseURL: 'http://127.0.0.1:4177', locale: 'es-EC', screenshot: 'only-on-failure', trace: 'retain-on-failure' },
  webServer: {
    command: 'npm run dev --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4177 --strictPort',
    url: 'http://127.0.0.1:4177/e2e/storage-upload.html', reuseExistingServer: false, timeout: 90_000,
    env: { VITE_API_BASE: 'http://127.0.0.1:4177', VITE_GOOGLE_DRIVE_CLIENT_ID: 'synthetic.apps.googleusercontent.com', VITE_GOOGLE_DRIVE_REDIRECT_URI: 'http://127.0.0.1:4177/e2e/storage-upload.html' },
  },
  projects: [{ name: 'desktop', use: { ...devices['Desktop Chrome'] } }, { name: 'phone', use: { ...devices['Pixel 7'] } }],
});
