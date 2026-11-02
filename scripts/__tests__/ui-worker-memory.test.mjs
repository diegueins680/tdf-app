import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, realpathSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { test } from 'node:test';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const CONTROLLED_WORKER_LIMIT_BYTES = 1024;
const DEFAULT_WORKER_LIMIT_BYTES = 1024 ** 3;

/**
 * @precondition the actual UI workspace test command and installed Jest are available.
 * @invariant only disposable local CommonJS fixtures execute; no application or provider is called.
 * @postcondition the receipt includes every fixture result and the PID that actually executed it.
 */
function runFixture({ forcedInBand = false, failedSuite = false, fatalSuite = false } = {}) {
  const fixture = realpathSync(mkdtempSync(join(tmpdir(), 'tdf-ui-worker-memory-')));
  const events = join(fixture, 'events.jsonl');
  const result = join(fixture, 'result.json');
  try {
    const config = join(fixture, 'jest.config.cjs');
    writeFileSync(config, `
      const original = require(${JSON.stringify(join(root, 'tdf-hq-ui/jest.config.cjs'))});
      module.exports = {
        ...original,
        // These dependency-free CommonJS fixtures do not use the UI transformer.
        preset: undefined,
        rootDir: ${JSON.stringify(fixture)},
        testEnvironment: 'node',
        testMatch: ['**/*.test.cjs'],
        transform: {},
        extensionsToTreatAsEsm: [],
        setupFiles: [],
        setupFilesAfterEnv: [],
        reporters: ['default'],
        // An unambiguous small byte limit forces recycling without large allocations.
        workerIdleMemoryLimit: ${CONTROLLED_WORKER_LIMIT_BYTES},
      };
    `);
    const suites = ['first', 'middle', 'last'];
    const paths = suites.map(name => {
      const path = join(fixture, `${name}.test.cjs`);
      writeFileSync(path, `
        test(${JSON.stringify(name)}, () => {
          require('node:fs').appendFileSync(${JSON.stringify(events)},
            JSON.stringify({ suite: ${JSON.stringify(name)}, pid: process.pid, worker: process.env.JEST_WORKER_ID }) + '\\n');
          ${fatalSuite && name === 'middle' ? 'process.exit(1);' : ''}
          expect(${JSON.stringify(failedSuite && name === 'middle' ? 'failure' : 'pass')}).toBe('pass');
        });
      `);
      return path;
    });
    const args = ['run', 'test', '--workspace=tdf-hq-ui', '--', '--config', config,
      '--runTestsByPath', ...paths, '--json', '--outputFile', result];
    if (forcedInBand) args.push('--runInBand');
    const child = spawnSync('npm', args, { cwd: root, encoding: 'utf8', timeout: 90_000 });
    assert.equal(child.signal, null, child.stderr);
    assert.equal(child.error, undefined, child.error?.message);
    assert.ok(existsSync(result), child.stdout + child.stderr);
    const report = JSON.parse(readFileSync(result, 'utf8'));
    const executions = readFileSync(events, 'utf8').trim().split('\n').map(line => JSON.parse(line));
    assert.equal(report.numTotalTestSuites, suites.length, child.stderr);
    assert.equal(report.numPendingTests, 0);
    if (fatalSuite) {
      assert.equal(report.numRuntimeErrorTestSuites, 1);
      assert.deepEqual([...new Set(executions.map(x => x.suite))].sort(), suites.toSorted());
    } else {
      assert.equal(report.numTotalTests, suites.length, child.stderr);
      assert.equal(report.numRuntimeErrorTestSuites, 0);
      assert.deepEqual(executions.map(x => x.suite).sort(), suites.toSorted());
    }
    return { status: child.status, report, executions };
  } finally {
    rmSync(fixture, { recursive: true, force: true });
  }
}

test('the default UI command enables serial worker recycling with the configured idle budget', () => {
  const child = spawnSync('npm', ['run', 'test', '--workspace=tdf-hq-ui', '--', '--showConfig'],
    { cwd: root, encoding: 'utf8', timeout: 90_000 });
  assert.equal(child.status, 0, child.stdout + child.stderr);
  const start = child.stdout.indexOf('\n{');
  assert.notEqual(start, -1, child.stdout);
  const { globalConfig } = JSON.parse(child.stdout.slice(start + 1));
  assert.equal(globalConfig.runInBand, false, 'the default command must permit a recyclable child');
  assert.equal(globalConfig.maxWorkers, 1, 'the default command must preserve serial scheduling');
  assert.equal(globalConfig.workerIdleMemoryLimit, DEFAULT_WORKER_LIMIT_BYTES,
    'the real production configuration must supply the memory budget that fixtures scale down');
});

test('the real UI command recycles its worker after the controlled per-file memory threshold', () => {
  const result = runFixture();
  assert.equal(result.status, 0);
  assert.equal(result.report.numPassedTests, 3);
  assert.deepEqual([...new Set(result.executions.map(x => x.worker))], ['1'],
    'files must use a single serial worker slot');
  assert.equal(new Set(result.executions.map(x => x.pid)).size, 3,
    'all three files must execute in distinct recycled worker processes');
});

test('forcing the old in-band command defeats the recycling probe', () => {
  const result = runFixture({ forcedInBand: true });
  assert.equal(result.status, 0);
  assert.equal(result.report.numPassedTests, 3);
  assert.equal(new Set(result.executions.map(x => x.pid)).size, 1,
    'the negative control must demonstrate one unrecycled process');
});

test('worker recycling preserves a failing assertion and still executes every file', () => {
  const result = runFixture({ failedSuite: true });
  assert.equal(result.status, 1);
  assert.equal(result.report.success, false);
  assert.equal(result.report.numFailedTests, 1);
  assert.equal(result.report.numPassedTests, 2);
  assert.equal(new Set(result.executions.map(x => x.pid)).size, 3);
});

test('a persistently crashing file fails after worker retries and other files still complete', () => {
  const result = runFixture({ fatalSuite: true });
  assert.equal(result.status, 1);
  assert.equal(result.report.success, false);
  assert.equal(result.report.numFailedTestSuites, 1);
  assert.equal(result.report.numPassedTestSuites, 2);
  assert.equal(result.report.numPassedTests, 2);
});
