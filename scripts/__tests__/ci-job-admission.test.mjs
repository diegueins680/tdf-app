import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, mkdirSync, copyFileSync, writeFileSync, symlinkSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { parse as parseYaml } from 'yaml';
import { admitSelectedJobs } from '../ci-job-admission.mjs';

const root = fileURLToPath(new URL('../../', import.meta.url));
const scopeNames = ['repo', 'ui', 'mobile', 'backend', 'contracts', 'migrations'];
const qualityJobs = ['repo-quality', 'ui-quality', 'persona-web-e2e', 'native-music-e2e',
  'mobile-quality', 'backend-quality', 'native-android-e2e'];

function fixture(mask = 63, gate = 'quality') {
  const outputs = Object.fromEntries(scopeNames.map((name, index) =>
    [name, String(Boolean(mask & (1 << index)))]));
  const jobs = gate === 'quality' ? qualityJobs :
    gate === 'api-contracts' ? ['api-contract-tests'] : ['migration-tests'];
  return Object.fromEntries([
    ['changes', { result: 'success', outputs }],
    ...jobs.map(name => [name, { result: 'success' }]),
  ]);
}

function accepts(...args) {
  try { admitSelectedJobs(...args); return true; } catch { return false; }
}

test('exhaustive quality selection and success/skip vectors obey the admission contract', () => {
  let cases = 0;
  for (let scopes = 0; scopes < 64; scopes++) {
    for (const native of [false, true]) {
      // Independent normative oracle: the UI and repository journeys use OR;
      // native Android is selected by its separate workflow-dispatch condition.
      const selected = [!!(scopes & 1), !!(scopes & 2), !!(scopes & 3), !!(scopes & 3),
        !!(scopes & 4), !!(scopes & 8), native];
      for (let results = 0; results < 128; results++) {
        const needs = fixture(scopes);
        qualityJobs.forEach((name, index) => {
          needs[name].result = results & (1 << index) ? 'success' : 'skipped';
        });
        const expected = selected.every((required, index) => !required || !!(results & (1 << index)));
        assert.equal(accepts('quality', needs, String(native)), expected,
          `scopes=${scopes}, native=${native}, results=${results}`);
        cases++;
      }
    }
  }
  assert.equal(cases, 16384);
});

test('failed, cancelled, missing and unknown dependencies fail even when unselected', () => {
  for (const mask of [0, 63]) for (const name of qualityJobs) {
    for (const result of ['failure', 'cancelled', 'timed_out', 'neutral', '', undefined]) {
      const needs = fixture(mask);
      needs[name].result = result;
      assert.equal(accepts('quality', needs, 'false'), false, `${mask}/${name}/${result}`);
    }
    const needs = fixture(mask);
    delete needs[name];
    assert.equal(accepts('quality', needs, 'false'), false);
  }
});

test('classification and every selection flag must be complete and unambiguous', () => {
  for (const result of ['skipped', 'failure', 'cancelled', undefined]) {
    const needs = fixture(); needs.changes.result = result;
    assert.equal(accepts('quality', needs, 'false'), false);
  }
  for (const name of scopeNames) for (const value of [true, false, '', null, undefined, 'TRUE']) {
    const needs = fixture(); needs.changes.outputs[name] = value;
    assert.equal(accepts('quality', needs, 'false'), false);
  }
  for (const native of [undefined, '', true, 'TRUE']) {
    assert.equal(accepts('quality', fixture(), native), false);
  }
  const extraScope = fixture(); extraScope.changes.outputs.unmapped = 'true';
  assert.equal(accepts('quality', extraScope, 'false'), false);
  const extraJob = fixture(); extraJob.unmapped = { result: 'success' };
  assert.equal(accepts('quality', extraJob, 'false'), false);
  assert.equal(accepts('quality', null, 'false'), false);
  assert.equal(accepts('unknown', fixture(), 'false'), false);
});

test('contract and migration gates admit skips only for explicitly unselected work', () => {
  for (let scopes = 0; scopes < 64; scopes++) {
    for (const [gate, name, bit] of [['api-contracts', 'api-contract-tests', 16],
      ['production-migrations', 'migration-tests', 32]]) {
      for (const result of ['success', 'skipped', 'failure', 'cancelled', 'neutral', undefined]) {
        const needs = fixture(scopes, gate); needs[name].result = result;
        const expected = result === 'success' || (result === 'skipped' && !(scopes & bit));
        assert.equal(accepts(gate, needs), expected, `${scopes}/${gate}/${result}`);
      }
    }
  }
});

test('actual workflow gates reject three historical selected-skip counterexamples', () => {
  const workflow = parseYaml(readFileSync(new URL('../../.github/workflows/ci.yml', import.meta.url), 'utf8'));
  const old = JSON.parse(readFileSync(new URL('./fixtures/ci-selected-job-admission-before.json', import.meta.url)));
  const nativeSelection = workflow.jobs.quality.steps.find(row => row.env?.CI_ADMISSION_NATIVE_ANDROID);
  assert.equal(nativeSelection.env.CI_ADMISSION_NATIVE_ANDROID,
    '${{ ' + workflow.jobs['native-android-e2e'].if + " && 'true' || 'false' }}");
  for (const [gate, dependency, legacyVariable] of [
    ['quality', 'backend-quality', 'BACKEND_RESULT'],
    ['api-contracts', 'api-contract-tests', 'CONTRACT_RESULT'],
    ['production-migrations', 'migration-tests', 'MIGRATION_RESULT'],
  ]) {
    const needs = fixture(63, gate); needs[dependency].result = 'skipped';
    const env = { PATH: process.env.PATH, CI_ADMISSION_NEEDS: JSON.stringify(needs),
      CI_ADMISSION_NATIVE_ANDROID: 'false',
      ...Object.fromEntries(old.gates[gate].environmentNames.map(name => [name, 'success'])),
      [legacyVariable]: 'skipped' };
    const run = script => spawnSync('bash', ['-euo', 'pipefail', '-c', script],
      { cwd: root, encoding: 'utf8', env });
    const baseline = run(old.gates[gate].run);
    assert.equal(baseline.status, 0, baseline.stderr);
    const step = workflow.jobs[gate].steps.find(row => row.env?.CI_ADMISSION_NEEDS);
    assert.equal(step.env.CI_ADMISSION_NEEDS, '${{ toJSON(needs) }}');
    const corrected = run(step.run);
    assert.equal(corrected.status, 1, corrected.stderr);
    needs[dependency].result = 'success'; env.CI_ADMISSION_NEEDS = JSON.stringify(needs);
    const positive = run(step.run);
    assert.equal(positive.status, 0, positive.stderr);
    const admitted = JSON.parse(positive.stdout);
    assert.ok(admitted.results.some(row => row.name === dependency && row.selected));
  }
});

test('CLI rejection never prints unrelated dependency outputs or malformed JSON', () => {
  const secret = 'synthetic-private-output-never-log';
  const needs = fixture(); needs.changes.outputs.backend = secret;
  for (const payload of [secret, JSON.stringify(needs), JSON.stringify({ [secret]: {} })]) {
    const result = spawnSync('node', ['scripts/ci-job-admission.mjs', 'quality'], {
      cwd: root, encoding: 'utf8', env: { PATH: process.env.PATH,
        CI_ADMISSION_NEEDS: payload, CI_ADMISSION_NATIVE_ANDROID: 'false' },
    });
    assert.equal(result.status, 1);
    assert.ok(!(result.stdout + result.stderr).includes(secret));
  }
});

test('independent guard-removal control must fail all three admission properties', () => {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-ci-admission-mutation-'));
  try {
    for (const name of ['scripts/ci-job-admission.mjs', 'scripts/__tests__/ci-job-admission.test.mjs',
      'scripts/__tests__/fixtures/ci-selected-job-admission-before.json', '.github/workflows/ci.yml']) {
      const target = path.join(directory, name);
      mkdirSync(path.dirname(target), { recursive: true });
      copyFileSync(path.join(root, name), target);
    }
    symlinkSync(path.join(root, 'node_modules'), path.join(directory, 'node_modules'), 'dir');
    const target = path.join(directory, 'scripts/ci-job-admission.mjs');
    const original = readFileSync(target, 'utf8');
    const guard = "result === 'skipped' && !required";
    assert.equal(original.split(guard).length, 2, 'Review mutation placement after guard changes');
    writeFileSync(target, original.replace(guard, "result === 'skipped'"));
    // The name filter excludes this mutation test, so the child cannot recurse.
    const result = spawnSync(process.execPath, ['--test', '--test-reporter=tap',
      '--test-name-pattern=exhaustive quality|contract and migration gates|actual workflow gates',
      path.join(directory, 'scripts/__tests__/ci-job-admission.test.mjs')],
    { encoding: 'utf8', timeout: 30000, env: { PATH: process.env.PATH } });
    assert.equal(result.status, 1, result.stderr);
    assert.match(result.stdout, /^# fail 3$/m);
    for (const title of ['exhaustive quality', 'contract and migration gates', 'actual workflow gates']) {
      assert.match(result.stdout, new RegExp(`^not ok \\d+ - ${title}`, 'm'));
    }
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
