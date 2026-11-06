import base from '/private/tmp/tdf-studio-gallery-safari-20261026/playwright.config.mjs';
export default {
  ...base,
  testDir: '/private/tmp/tdf-studio-gallery-safari-20261026/e2e/web',
  projects: base.projects.filter(project => ['chromium-desktop', 'chromium-phone', 'android-chrome-small', 'firefox-critical'].includes(project.name)),
  outputDir: '/private/tmp/tdf-studio-gallery-booking-joint-20261026/browser/test-results',
  reporter: [['line'], ['json', {outputFile: '/private/tmp/tdf-studio-gallery-booking-joint-20261026/browser/results.json'}]],
  use: {...base.use, baseURL: 'http://127.0.0.1:4294', serviceWorkers: 'block'},
  webServer: {
    ...base.webServer,
    command: 'npm run preview --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4294 --strictPort',
    cwd: '/private/tmp/tdf-studio-gallery-safari-20261026',
    url: 'http://127.0.0.1:4294/tdf',
    reuseExistingServer: false,
  },
};
