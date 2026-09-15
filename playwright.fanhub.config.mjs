import { defineConfig } from '@playwright/test';
import isolatedBrowserConfig from './playwright.artist-follow.config.mjs';

// Reuse the existing disposable, local-only Vite runner; never attach to another worktree.
export default defineConfig({
  ...isolatedBrowserConfig,
  testMatch: 'fanhub-onboarding.spec.mjs',
  outputDir: 'artifacts/fanhub-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/fanhub-playwright/results.json' }]],
});
