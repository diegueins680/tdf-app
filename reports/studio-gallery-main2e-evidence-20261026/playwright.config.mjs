import base from '/private/tmp/tdf-studio-gallery-safari-20261026/playwright.config.mjs';
export default {
  ...base,
  testDir: '/private/tmp/tdf-studio-gallery-safari-20261026/e2e/web',
  projects: base.projects.filter(project => ['chromium-desktop', 'chromium-phone', 'android-chrome-small', 'firefox-critical'].includes(project.name)),
  outputDir: '/private/tmp/tdf-gallery-main2e-browser-20261026/test-results',
  reporter: [['line'], ['json', {outputFile: '/private/tmp/tdf-gallery-main2e-browser-20261026/results.json'}]],
  use: {...base.use, baseURL: 'http://127.0.0.1:4288', serviceWorkers: 'block'},
  webServer: {
    ...base.webServer,
    command: 'npm run preview --workspace=tdf-hq-ui -- --host 127.0.0.1 --port 4288 --strictPort',
    cwd: '/private/tmp/tdf-studio-gallery-safari-20261026',
    url: 'http://127.0.0.1:4288/tdf',
    reuseExistingServer: false,
  },
};
