import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
function rejected(script, overrides, message) {
  const env = { ...process.env, GITHUB_ACTIONS: '', EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST: '',
    EVENT_OPERATIONS_TEST_DSN: '', ...overrides };
  const result = spawnSync('sh', [path.join(root, 'scripts', script)], {
    cwd: root, env, encoding: 'utf8', timeout: 5000,
  });
  assert.ifError(result.error);
  assert.equal(result.status, 1);
  assert.match(result.stderr, message);
  assert.equal(result.stdout, '');
}

test('HTTP harness refuses absent or invalid disposable-test guard before compilation or SQL', () => {
  for (const guard of ['', '0', 'true']) {
    rejected('run-event-operations-http-harness.sh', {
      EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST: guard,
      EVENT_OPERATIONS_TEST_DSN: 'unused-never-connected',
    }, /Disposable HTTP test guard required/);
  }
});

test('HTTP harness refuses a missing DSN before compilation or SQL', () => {
  rejected('run-event-operations-http-harness.sh', {
    EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST: '1',
  }, /Test DSN required/);
});

test('CI fixture runner refuses execution outside the declared service environment', () => {
  rejected('test-event-operations-http-ci.sh', {}, /requires the CI service container/);
});
