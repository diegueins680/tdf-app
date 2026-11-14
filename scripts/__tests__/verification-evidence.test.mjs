import assert from 'node:assert/strict';
import test from 'node:test';
import { readFileSync, mkdtempSync, mkdirSync, writeFileSync, rmSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { classifyExecution, sourceManifest } from '../lib/verification-evidence.mjs';

test('operational configuration changes invalidate source evidence even before staging', t => {
  const root = mkdtempSync(path.join(tmpdir(), 'tdf-ops-evidence-'));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  execFileSync('git', ['init', '-q'], { cwd: root });
  mkdirSync(path.join(root, 'ops/hetzner'), { recursive: true });
  const config = path.join(root, 'ops/hetzner/compose.production.yaml');
  writeFileSync(config, 'services: {}\n');
  const before = sourceManifest(root);
  assert.ok(before.files['ops/hetzner/compose.production.yaml']);
  writeFileSync(config, 'services: { changed: {} }\n');
  const after = sourceManifest(root);
  assert.notEqual(before.digest, after.digest);
  mkdirSync(path.join(root, 'formal/system/evidence'), { recursive: true });
  writeFileSync(path.join(root, 'formal/system/evidence/receipt.json'), '{}\n');
  assert.equal(sourceManifest(root).digest, after.digest);
});

test('success requires completed execution, exact source stability and completion evidence', () => {
  const log = 'Event operations formal verification passed within the documented finite bounds.';
  let accepted = 0;
  // Complete finite product of these admission predicates, including contradictory process outcomes.
  for (const status of [0, 1, null]) for (const signal of [null, 'SIGTERM'])
    for (const error of [undefined, 'spawn failed']) for (const complete of [true, false])
      for (const stable of [true, false]) {
        const result = classifyExecution({ status, signal, error, log: complete ? log : '', before: 'a', after: stable ? 'a' : 'b' });
        const passes = status === 0 && !signal && !error && complete && stable;
        assert.equal(result === 'bounded-model-checks-passed', passes);
        if (passes) accepted++;
      }
  assert.equal(accepted, 1);
});

test('both formal workflows revalidate on PRs, main integration and schedule without path omissions', () => {
  for (const name of ['event-operations-formal.yml', 'social-verification.yml']) {
    const source = readFileSync(new URL(`../../.github/workflows/${name}`, import.meta.url), 'utf8');
    assert.match(source, /^  pull_request:/m);
    assert.match(source, /^  push:\n    branches: \[main\]/m);
    assert.match(source, /^  schedule:/m);
    assert.doesNotMatch(source, /^\s+paths(?:-ignore)?:/m);
    assert.doesNotMatch(source, /continue-on-error/);
  }
});

// Every shipped execution surface must invalidate retained model evidence.
test('desktop, edge, campaign and media source edits invalidate evidence', t => {
  const root = mkdtempSync(path.join(tmpdir(), 'tdf-surfaces-evidence-'));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  execFileSync('git', ['init', '-q'], { cwd: root });
  for (const name of ['tdf-desktop/src/main/main.cjs', 'services/campaign-platform/worker.py',
    'functions/api.ts', 'streaming/worker.js', 'tidal-agent/worker.py', 'e2e/journey.mjs', 'test/capacity.json']) {
    const full = path.join(root, name);
    mkdirSync(path.dirname(full), { recursive: true });
    writeFileSync(full, 'original');
    const before = sourceManifest(root);
    assert.ok(before.files[name], `${name} must be fingerprinted`);
    writeFileSync(full, 'changed');
    assert.notEqual(sourceManifest(root).digest, before.digest, name);
  }
});
