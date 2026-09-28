import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const runner = 'scripts/test-event-task-revision-migration.sh';
const source = readFileSync(runner, 'utf8');
test('task revision runner rejects configuration before allocating its own database', () => {
  for (const args of [['--database-url=unused'], ['extra'], ['', 'extra']]) {
    const result = spawnSync('sh', [runner, ...args], { encoding: 'utf8', timeout: 5000 });
    assert.ifError(result.error); assert.equal(result.status, 2); assert.equal(result.stdout, '');
  }
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /-U postgres -d tdf_task_revision_test/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish|\s-p\s/);
});
test('task revision verification is not silently dropped from CI or schema rehearsal', () => {
  const workflow = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(workflow, /postgres-task-revision:/);
  assert.match(workflow, /run: sh scripts\/test-event-task-revision-migration.sh/);
  assert.doesNotMatch(workflow, /continue-on-error/);
  for (const file of [runner, 'tdf-hq/test/integration/event_task_revision_assertions.sql',
    'scripts/__tests__/event-task-revision-runner.test.mjs']) {
    assert.equal(workflow.split(`"${file}"`).length - 1, 2, file);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  assert.match(rehearsal, /apply_sql tdf-hq\/sql\/2026-09-15_event_task_revision.sql/);
  assert.ok(rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_task_revision_rollback.sql')
    < rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-14_event_task_commit_rollback.sql'));
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json', 'utf8'), /2026-09-15_event_task_revision/);
});
test('formal gate retains the positive model and named stale-write controls', () => {
  const gate = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  assert.match(gate, /run_tlc TaskRevision.tla TaskRevision.cfg task-revision/);
  for (const mutation of ['Raci', 'Early']) {
    assert.match(gate, new RegExp(`expect_counterexample TaskRevision${mutation}\\.cfg NoStaleCommit`));
  }
});
