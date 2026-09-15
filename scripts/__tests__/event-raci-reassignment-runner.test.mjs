import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const runner = 'scripts/test-event-raci-reassignment-migration.sh';
const source = readFileSync(runner, 'utf8');
test('RACI runner rejects external configuration before allocating its isolated database', () => {
  for (const args of [['--database-url=unused'], ['extra'], ['', 'extra']]) {
    const result = spawnSync('sh', [runner, ...args], { encoding: 'utf8', timeout: 5000 });
    assert.ifError(result.error); assert.equal(result.status, 2); assert.equal(result.stdout, '');
  }
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /-U postgres -d tdf_raci_command_test/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish|\s-p\s/);
  assert.match(source, /pg_blocking_pids/);
  assert.match(source, /statement_timeout=25000/);
});
test('RACI command CI and full-schema migration order remain mandatory without production activation', () => {
  const workflow = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(workflow, /postgres-raci-reassignment:/);
  assert.match(workflow, /run: sh scripts\/test-event-raci-reassignment-migration.sh/);
  assert.doesNotMatch(workflow, /continue-on-error/);
  for (const file of [runner, 'scripts/__tests__/event-raci-reassignment-runner.test.mjs',
    'tdf-hq/test/integration/event_raci_reassignment_*.sql']) {
    assert.equal(workflow.split(`"${file}"`).length - 1, 2, file);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  const read = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql');
  const up = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment.sql');
  const down = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment_rollback.sql');
  const readDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revisioned_read_rollback.sql');
  assert.ok(read >= 0 && up > read && down > up && readDown > down);
  assert.match(rehearsal, /apply_sql tdf-hq\/test\/integration\/event_raci_reassignment_schema_assertions.sql/);
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json', 'utf8'), /2026-09-15_event_raci_reassignment/);
});
test('RACI formal gate retains the positive model and all six named counterexamples', () => {
  const gate = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  assert.match(gate, /run_tlc RaciReassignment.tla RaciReassignment.cfg raci-reassignment/);
  for (const [mutation, invariant] of Object.entries({ Early: 'CurrentAuthority',
    Version: 'NoStaleReassignment', Replay: 'ExactRetry', Scope: 'TaskKeyIsolation',
    Split: 'NoOrphanResponsibilities', Audit: 'AuditCoupled' })) {
    assert.ok(gate.includes(`expect_counterexample RaciReassignment${mutation}.cfg ${invariant}`));
  }
});
