import { defineConfig, devices } from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
export default defineConfig({ testDir: '.', timeout: 30000, expect: { timeout: 8000 }, retries: 0, workers: 1,
 reporter: [['line'],['json',{outputFile:'/private/tmp/tdf-booking-resize-phase-final-matrix-20261029/results.json'}]],
 outputDir:'/private/tmp/tdf-booking-resize-phase-final-matrix-20261029/output',
 use: {baseURL:'http://127.0.0.1:41039',locale:'es-EC',timezoneId:'America/Guayaquil',colorScheme:'dark',reducedMotion:'reduce',trace:'on',screenshot:'only-on-failure'},
 projects: [
  {name:'chromium-desktop',use:{...devices['Desktop Chrome'],browserName:'chromium',defaultBrowserType:'chromium',launchOptions:{executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'}}},
  {name:'chromium-phone',use:{...devices['iPhone 13'],browserName:'chromium',defaultBrowserType:'chromium',launchOptions:{executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'}}},
  {name:'firefox',use:{...devices['Desktop Firefox'],browserName:'firefox',defaultBrowserType:'firefox'}},
 ] });
