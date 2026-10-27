import {defineConfig,devices} from '@playwright/test';
import {fileURLToPath} from 'node:url';
import {resolve} from 'node:path';
const root=process.env.TDF_QUITO_SOURCE_ROOT || fileURLToPath(new URL('../../',import.meta.url));
const out=process.env.TDF_QUITO_OUTPUT || resolve(root,'artifacts/quito-preview');
export default defineConfig({testDir:resolve(root,'e2e/web'),testMatch:'booking-timezone.spec.mjs',fullyParallel:false,retries:0,workers:1,timeout:30000,expect:{timeout:8000},outputDir:resolve(out,'test-results'),reporter:[['line'],['json',{outputFile:resolve(out,'results.json')}]],use:{baseURL:'https://7946ab20.tdf-app.pages.dev',locale:'es-EC',colorScheme:'dark',reducedMotion:'reduce',trace:'retain-on-failure',screenshot:'only-on-failure',serviceWorkers:'block'},projects:[{name:'chromium-desktop',use:{...devices['Desktop Chrome']}},{name:'chromium-phone',use:{...devices['Pixel 7']}}]});
