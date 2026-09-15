import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const runner = 'scripts/test-event-task-revisioned-read-migration.sh';
const source = readFileSync(runner, 'utf8');
test('revisioned read runner owns its database and rejects caller configuration', () => {
  for (const args of [['--database-url=unused'], ['extra'], ['', 'extra']]) {
    const result = spawnSync('sh', [runner, ...args], { encoding: 'utf8', timeout: 5000 });
    assert.ifError(result.error); assert.equal(result.status, 2); assert.equal(result.stdout, '');
  }
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /-U postgres -d tdf_task_revision_read_test/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish|\s-p\s/);
  assert.match(source, /pg_blocking_pids/);
});
test('revisioned read verification remains mandatory and out of production activation', () => {
  const workflow = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(workflow, /postgres-task-revisioned-read:/);
  assert.match(workflow, /run: sh scripts\/test-event-task-revisioned-read-migration.sh/);
  assert.doesNotMatch(workflow, /continue-on-error/);
  for (const file of [runner, 'scripts/__tests__/event-task-revisioned-read-runner.test.mjs',
    'tdf-hq/test/integration/event_task_revisioned_read_*.sql']) {
    assert.equal(workflow.split(`"${file}"`).length - 1, 2, file);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  const revision = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revision.sql');
  const read = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql');
  const readDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revisioned_read_rollback.sql');
  const revisionDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revision_rollback.sql');
  assert.ok(revision >= 0 && read > revision && readDown > read && revisionDown > readDown);
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json', 'utf8'), /2026-09-15_event_task_revisioned_read/);
});
test('formal gate retains coherent-read and expired-grant negative controls', () => {
  const gate = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  assert.match(gate, /run_tlc TaskRevisionRead.tla TaskRevisionRead.cfg task-revision-read/);
  assert.match(gate, /expect_counterexample TaskRevisionReadMixed.cfg CoherentRevisionRead/);
  assert.match(gate, /expect_counterexample TaskRevisionReadEarly.cfg NoExpiredDisclosure/);
});
