import { defineConfig } from '@playwright/test';
import isolatedBrowserConfig from './playwright.artist-follow.config.mjs';

export default defineConfig({
  ...isolatedBrowserConfig,
  testMatch: 'event-task-view.spec.mjs',
  outputDir: 'artifacts/event-task-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/event-task-playwright/results.json' }]],
});
