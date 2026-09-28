import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const runner = 'scripts/test-event-task-completion-migration.sh';
test('completion runner rejects external targets and owns an isolated disposable database', () => {
  const source = readFileSync(runner, 'utf8');
  for (const args of [['--database-url=unused'], ['extra'], ['', 'extra']]) {
    const result = spawnSync('sh', [runner, ...args], { encoding: 'utf8', timeout: 5000 });
    assert.ifError(result.error); assert.equal(result.status, 2); assert.equal(result.stdout, '');
  }
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /-U postgres -d tdf_completion_test/);
  assert.match(source, /pg_blocking_pids/);
  assert.match(source, /statement_timeout=25000/);
  const expiryCases = source.slice(source.indexOf('for expiry in metadata activity party raci; do'));
  assert.ok(expiryCases.indexOf('start_coordinator') < expiryCases.indexOf('expiry_query='));
  assert.equal(expiryCases.split("interval '15 seconds'").length - 1, 2);
  assert.match(expiryCases, /command starts before expiry/);
  assert.match(expiryCases, /assert_blocked_by completion_expiry_holder completion_expiry_waiter/);
  assert.match(expiryCases, /clock_timestamp\(\)>=\(\$expiry_query\)/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish|\s-p\s/);
});
test('completion CI and full-schema down/up stay mandatory without production activation', () => {
  const workflow = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(workflow, /postgres-task-completion:/);
  assert.match(workflow, /run: sh scripts\/test-event-task-completion-migration.sh/);
  assert.doesNotMatch(workflow, /continue-on-error/);
  for (const file of [runner, 'scripts/__tests__/event-task-completion-runner.test.mjs',
    'tdf-hq/test/integration/event_task_completion_*.sql']) {
    assert.equal(workflow.split(`"${file}"`).length - 1, 2, file);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  const prerequisite = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment.sql');
  const up = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-16_event_task_completion.sql');
  const down = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-16_event_task_completion_rollback.sql');
  const prerequisiteDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment_rollback.sql');
  assert.ok(prerequisite >= 0 && up > prerequisite && down > up && prerequisiteDown > down);
  assert.match(rehearsal, /apply_sql tdf-hq\/test\/integration\/event_task_completion_schema_assertions.sql/);
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json', 'utf8'), /2026-09-16_event_task_completion/);
});
test('completion formal gate retains the positive model and seven named counterexamples', () => {
  const gate = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  assert.match(gate, /run_tlc TaskCompletion.tla TaskCompletion.cfg task-completion/);
  for (const [mutation, invariant] of Object.entries({ Authority: 'CurrentAuthority',
    Version: 'NoStaleCompletion', Dependencies: 'NoBlockedCompletion', Raci: 'CurrentAccountability',
    Lifecycle: 'ValidLifecycle', Replay: 'ExactRetry', Audit: 'AuditCoupled' })) {
    assert.ok(gate.includes(`expect_counterexample TaskCompletion${mutation}.cfg ${invariant}`));
  }
});
