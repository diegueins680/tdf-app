import { chromium } from '@playwright/test';
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
const require = createRequire(import.meta.url);
const output = process.env.TDF_SOCIAL_BROWSER_RESULTS ?? 'docs/social/evidence/browser';
await mkdir(output, { recursive: true });
const browser = await chromium.launch({ headless: true });
try {
  const context = await browser.newContext({ viewport: { width: 1360, height: 1000 } });
  await context.route('**/*', route => {
    const host = new URL(route.request().url()).hostname;
    return host === '127.0.0.1' ? route.continue() : route.abort();
  });
  const page = await context.newPage();
  page.setDefaultTimeout(120000);
  const errors = [];
  page.on('pageerror', error => { errors.push(String(error)); console.error('Browser:', String(error)); });
  await page.goto('http://127.0.0.1:5199/social-preview');
  await page.getByRole('heading', { name: 'Buscamos una voz para una colaboración' }).waitFor();
  if (await page.getByRole('tab', { name: 'Siguiendo' }).getAttribute('aria-selected') !== 'true') {
    throw new Error('Following must be default');
  }
  await page.screenshot({ path: `${output}/following-desktop.png`, fullPage: true });
  await page.getByRole('tab', { name: 'Siguiendo' }).focus();
  await page.keyboard.press('ArrowRight');
  await page.keyboard.press('Enter');
  await page.getByText('Intereses musicales compartidos').waitFor();
  await page.getByRole('button', { name: 'Aceptar conexión: Lucía — guitarra y producción' }).click();
  await page.getByRole('button', { name: 'Desconectar: Lucía — guitarra y producción' }).waitFor();
  await page.setViewportSize({ width: 390, height: 844 });
  if (await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1)) {
    throw new Error('Horizontal overflow at 390px');
  }
  await page.screenshot({ path: `${output}/discover-mobile.png`, fullPage: true });
  await page.addScriptTag({ content: await readFile(require.resolve('axe-core/axe.min.js'), 'utf8') });
  const accessibility = await page.evaluate(async () => window.axe.run(document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa'] },
  }));
  const report = { scope: 'Local component with synthetic session/API fixtures; not authenticated E2E',
    checks: ['default Following', 'keyboard tabs', 'explicit acceptance', '390px overflow'],
    errors, accessibilityViolations: accessibility.violations };
  await writeFile(`${output}/results.json`, `${JSON.stringify(report, null, 2)}\n`);
  if (errors.length || accessibility.violations.length) throw new Error(JSON.stringify(report));
  console.log('PASS: local component journey, keyboard tabs, mobile overflow, WCAG axe checks');
} finally { await browser.close(); }
