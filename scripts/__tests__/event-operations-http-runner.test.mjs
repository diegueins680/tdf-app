import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
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

test('both HTTP runners install task prerequisites before opted-in task fixtures', () => {
  for (const script of ['test-event-operations-http.sh', 'test-event-operations-http-ci.sh']) {
    const source = readFileSync(path.join(root, 'scripts', script), 'utf8');
    const positions = ['2026-09-14_event_operations_api.sql', '2026-09-14_event_task_commit.sql',
      '2026-09-14_event_task_read.sql', 'event_operations_http_fixture.sql'].map(file => source.indexOf(file));
    assert.ok(positions.every(position => position >= 0), `${script} must install every prerequisite`);
    assert.deepEqual(positions, [...positions].sort((a, b) => a - b), `${script} prerequisite order`);
  }
});
