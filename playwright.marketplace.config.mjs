import { defineConfig } from '@playwright/test';
import isolatedBrowserConfig from './playwright.artist-follow.config.mjs';

// ADR 0134: demand/offer marketplace negotiation journey. Reuses the
// disposable local Vite runner; every API call is a synthetic fixture.
export default defineConfig({
  ...isolatedBrowserConfig,
  testMatch: 'marketplace-negotiation.spec.mjs',
  outputDir: 'artifacts/marketplace-playwright/test-results',
  reporter: [['line'], ['json', { outputFile: 'artifacts/marketplace-playwright/results.json' }]],
});
