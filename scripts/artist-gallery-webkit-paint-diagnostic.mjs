// Private diagnostic branch only. This is not a release gate or a production operation.
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { spawnSync } from 'node:child_process';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const out = resolve(root, 'artifacts/artist-gallery-webkit-paint');
mkdirSync(out, { recursive: true });
const appSource = 'eecb54709b877ee13d6875557dea304d89f7b796';
const origin = 'https://0aa86ab2.tdf-app.pages.dev';
const digest = data => createHash('sha256').update(data).digest('hex');
async function identity() {
  const r = await fetch(origin + '/tdf'); assert.equal(r.status, 200);
  const entry = (await r.text()).match(/src="(\/assets\/index-[^"]+\.js)"/)?.[1];
  assert(entry); const asset = await fetch(origin + entry); assert.equal(asset.status, 200);
  const code = await asset.text(); assert(code.includes(appSource));
  return { appSource, origin, entry, sha256: digest(code) };
}
const prepareOnly = process.argv.includes('--prepare-only');
const before = prepareOnly ? null : await identity();
writeFileSync(resolve(out, 'identity-before.json'), JSON.stringify(before, null, 2) + '\n');
const original = readFileSync(resolve(root, 'e2e/web/public-welcome.spec.mjs'), 'utf8');
assert.equal(digest(original), '5102a2614b4f1ba3f9f94a8f65666e1610cfdb7172513ee8edf938028142c494');
const begin = original.indexOf("test('studio photos open with keyboard");
const end = original.indexOf("test('failed studio images retain", begin);
assert(begin >= 0 && end > begin);
const originalCase = original.slice(begin, end);
const visible = '  await expect(dialog).toBeVisible();';
assert.equal(originalCase.split(visible).length - 1, 2);
const readyCase = originalCase.replaceAll(visible,
  visible + "\n  await expect(dialog.locator('..')).toHaveCSS('opacity', '1');");
const withCase = body => original.slice(0, begin) + body + original.slice(end);
const forceSlowPaint = body => body.replace('  await trigger.focus();',
  "  await page.addStyleTag({ content: '.MuiDialog-container { transition-duration: 6s !important; }' });\n  await trigger.focus();");
const badContrast = readyCase.replace("  await expectAccessible(page, testInfo, 'studio-photo-dialog');",
  "  await page.addStyleTag({ content: '#studio-photo-title { color: #111b24 !important; }' });\n  await expectAccessible(page, testInfo, 'studio-photo-dialog');");
const phases = [
  { name: 'original', text: original, repeats: 3, goal: 'observe' },
  { name: 'paint-ready', text: withCase(readyCase), repeats: 3, goal: 'pass' },
  { name: 'slow-original', text: withCase(forceSlowPaint(originalCase)), repeats: 1, goal: 'contrast-failure' },
  { name: 'slow-paint-ready', text: withCase(forceSlowPaint(readyCase)), repeats: 1, goal: 'pass' },
  { name: 'bad-contrast-paint-ready', text: withCase(badContrast), repeats: 1, goal: 'contrast-failure' },
];
if (prepareOnly) {
  for (const phase of phases) {
    const file = resolve(out, 'prepare-' + phase.name + '.mjs');
    writeFileSync(file, phase.text);
    const syntax = spawnSync(process.execPath, ['--check', file], { stdio: 'inherit' });
    assert.equal(syntax.status, 0, phase.name);
  }
  JSON.parse(readFileSync(resolve(out, 'identity-before.json')));
  console.log(JSON.stringify({ preparedPhases: phases.length, fixtureSyntaxValid: true, noNetworkOrBrowserStarted: true }));
  process.exit(0);
}
function errors(node, list = []) {
  for (const s of node.suites || []) errors(s, list);
  for (const s of node.specs || []) for (const t of s.tests || [])
    for (const r of t.results || []) if (r.status !== 'passed') list.push(...(r.errors || []).map(e => e.message || ''));
  return list;
}
const results = [];
for (const phase of phases) {
  const dir = resolve(out, phase.name); mkdirSync(dir);
  writeFileSync(resolve(dir, 'public-welcome.spec.mjs'), phase.text);
  const config = `import base from ${JSON.stringify(pathToFileURL(resolve(root, 'playwright.config.mjs')).href)};
export default {...base,captureGitInfo:{commit:false,diff:false},testDir:${JSON.stringify(dir)},testMatch:'public-welcome.spec.mjs',grep:/studio photos open with keyboard/,repeatEach:${phase.repeats},projects:base.projects.filter(p=>['webkit-critical','webkit-iphone'].includes(p.name)).map(p=>({...p,grep:/studio photos open with keyboard/})),outputDir:${JSON.stringify(resolve(dir, 'test-results'))},reporter:[['line'],['json',{outputFile:${JSON.stringify(resolve(dir, 'results.json'))}}]],use:{...base.use,baseURL:${JSON.stringify(origin)},serviceWorkers:'block'},webServer:undefined};`;
  const configPath = resolve(dir, 'playwright.config.mjs'); writeFileSync(configPath, config);
  const run = spawnSync(process.execPath, [resolve(root, 'node_modules/@playwright/test/cli.js'), 'test', '--config=' + configPath],
    { cwd: root, stdio: 'inherit', env: process.env });
  const report = JSON.parse(readFileSync(resolve(dir, 'results.json')));
  const failures = errors(report);
  const expectedCases = phase.repeats * 2;
  assert.equal(report.stats.skipped, 0);
  let accepted;
  if (phase.goal === 'pass') accepted = run.status === 0 && report.stats.expected === expectedCases && report.stats.unexpected === 0;
  else if (phase.goal === 'contrast-failure') accepted = run.status === 1 && report.stats.unexpected === expectedCases && failures.length > 0 && failures.every(s => s.includes('color-contrast'));
  else accepted = (run.status === 0 || run.status === 1) && (run.status === 0 || failures.every(s => s.includes('color-contrast')));
  results.push({ phase: phase.name, fixtureSha256: digest(phase.text), exit: run.status,
    goal: phase.goal, stats: report.stats, errors: failures, accepted });
  writeFileSync(resolve(out, 'results-summary.json'), JSON.stringify({ appSource, before, results,
    scope: 'Diagnostic clones only. Original case is byte-exact EECB; proposed readiness adds two CSS-opacity assertions. Slow fade and bad text color are temporary controls, never shipped. All original accessibility/behavior assertions and test budgets remain.' }, null, 2) + '\n');
}
const after = await identity(); assert.deepEqual(after, before);
writeFileSync(resolve(out, 'identity-after.json'), JSON.stringify(after, null, 2) + '\n');
assert(results.every(r => r.accepted), 'At least one diagnostic phase did not meet its declared outcome; inspect retained results');
console.log(JSON.stringify({ appSource, acceptedPhases: results.length, originalsUnchanged: true,
  scope: 'Diagnostic completion only; does not mark any failed release check successful or prove root cause if original phase does not reproduce.' }));
