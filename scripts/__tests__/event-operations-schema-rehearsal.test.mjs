import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const script = path.join(root, 'scripts/test-event-operations-schema-rehearsal.sh');
const source = readFileSync(script, 'utf8');

test('schema runner rejects unknown options and extra arguments before creating a container', () => {
  for (const args of [['--database-url=unused'], ['', 'extra'],
    ['--diagnostic-missing-merch-prerequisite', 'extra']]) {
    const result = spawnSync('bash', [script, ...args], {
      cwd: root, encoding: 'utf8', timeout: 5000,
    });
    assert.ifError(result.error);
    assert.equal(result.status, 2);
    assert.equal(result.stdout, '');
  }
});

test('schema runner owns its database and cannot consume a caller DSN or publish a port', () => {
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /-U postgres -d tdf_event_schema_test/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish/);
  const dockerRun = source.match(/docker run[\s\S]+?pgvector\/pgvector:pg17/);
  assert.ok(dockerRun);
  assert.doesNotMatch(dockerRun[0], /\s-p\s/);
  assert.match(source, /set -euo pipefail/);
});

test('authoritative baseline failure is fatal unless the explicit diagnostic option was supplied', () => {
  assert.match(source, /diagnostic=false/);
  assert.match(source, /if \[ "\$diagnostic" = false \]; then exit "\$baseline_status"; fi/);
  assert.match(source, /DIAGNOSTIC ONLY: event checks passed on a supplemented baseline/);
  assert.match(source, /authoritative rehearsal remains BLOCKED/);
});

test('npm and hosted verification always select the authoritative mode without a workaround', () => {
  const pkg = JSON.parse(readFileSync(path.join(root, 'package.json'), 'utf8'));
  assert.equal(pkg.scripts['test:event-operations-schema-rehearsal'],
    'bash scripts/test-event-operations-schema-rehearsal.sh');
  const workflow = readFileSync(path.join(root, '.github/workflows/event-operations-formal.yml'), 'utf8');
  assert.match(workflow, /postgres-complete-schema:/);
  assert.match(workflow, /run: npm run test:event-operations-schema-rehearsal\s*$/);
  assert.doesNotMatch(workflow, /diagnostic-missing-merch|continue-on-error/);
});
