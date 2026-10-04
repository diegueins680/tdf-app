import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { copyFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { test } from 'node:test';
import { loadConfigFromFile } from 'vite';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const checker = join(root, 'tdf-hq-ui/scripts/check-initial-bundle.mjs');

function checkFixture({ preloads = [], entry = '', extra = {} } = {}) {
  const fixture = mkdtempSync(join(tmpdir(), 'tdf-ui-bundle-guard-'));
  try {
    mkdirSync(join(fixture, 'scripts'));
    mkdirSync(join(fixture, 'dist/assets'), { recursive: true });
    copyFileSync(checker, join(fixture, 'scripts/check-initial-bundle.mjs'));
    writeFileSync(join(fixture, 'dist/index.html'),
      '<script type="module" src="/assets/index.js"></script>'
      + preloads.map(name => `<link rel="modulepreload" href="/assets/${name}.js">`).join(''));
    writeFileSync(join(fixture, 'dist/assets/index.js'), entry);
    for (const name of preloads) writeFileSync(join(fixture, `dist/assets/${name}.js`), 'export const value = 1;');
    for (const [name, source] of Object.entries(extra)) writeFileSync(join(fixture, `dist/assets/${name}.js`), source);
    return spawnSync(process.execPath, [join(fixture, 'scripts/check-initial-bundle.mjs')], { encoding: 'utf8' });
  } finally {
    rmSync(fixture, { recursive: true, force: true });
  }
}

test('bundler isolates exact Zod package modules, without reclassifying unrelated code', async () => {
  const loaded = await loadConfigFromFile({ command: 'build', mode: 'test' }, join(root, 'tdf-hq-ui/vite.config.ts'));
  const classify = loaded.config.build.rollupOptions.output.manualChunks;
  assert.equal(classify('/repo/node_modules/zod/v3/types.js'), 'zod');
  assert.equal(classify('/repo/tdf-hq-ui/node_modules/zod/index.js'), 'zod');
  assert.notEqual(classify('/repo/node_modules/zod-helper/index.js'), 'zod');
  assert.equal(classify('/repo/src/zod/schema.ts'), undefined);
});

test('small initial artifact passes while Zod may exist as an unloaded route dependency', () => {
  const result = checkFixture({ extra: { 'zod-test': 'export const value = 1;' } });
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /bundle-budget/);
});

test('initial Zod preload fails even when its bytes are below budget', () => {
  const result = checkFixture({ preloads: ['zod-test'] });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /route-only chunks were preloaded: \/assets\/zod-test.js/);
});

test('existing route-only preload guard still fails', () => {
  const result = checkFixture({ preloads: ['EventTaskPage-test'] });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /route-only chunks were preloaded/);
});

test('ninth module preload still fails', () => {
  const result = checkFixture({ preloads: Array.from({ length: 9 }, (_, i) => `shared-${i}`) });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /module preloads 9 exceed 8/);
});

test('410 KiB compressed budget is unchanged and enforced', () => {
  // Deterministic high-entropy fixture, never an application payload or secret.
  const payload = Array.from({ length: 20_000 }, (_, i) => createHash('sha256').update(String(i)).digest('hex')).join('');
  const result = checkFixture({ entry: `export const fixture = "${payload}";` });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /initial JS \d+ bytes gzip exceeds 419840/);
});

test('secret-shaped strings in non-initial chunks still fail', () => {
  const result = checkFixture({ extra: { 'lazy-test': `export const fixture = "${'sk-' + 'x'.repeat(25)}";` } });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /potential OpenAI-style API key embedded/);
});
