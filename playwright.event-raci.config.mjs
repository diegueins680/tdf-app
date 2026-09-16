import { defineConfig } from '@playwright/test';
import isolatedBrowserConfig from './playwright.artist-follow.config.mjs';

export default defineConfig({
  ...isolatedBrowserConfig,
  testMatch: ['event-raci-editor.spec.mjs', 'event-task-view.spec.mjs'],
  outputDir: 'artifacts/event-raci-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/event-raci-playwright/results.json' }]],
});
