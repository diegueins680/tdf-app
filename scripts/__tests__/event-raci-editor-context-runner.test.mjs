import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';
const runner = 'scripts/test-event-raci-editor-context-migration.sh';
test('editor context tests cannot target an external database', () => {
  for (const args of [['--database-url=unused'], ['extra'], ['', 'extra']]) {
    const result = spawnSync('sh', [runner, ...args], { encoding: 'utf8', timeout: 5000 });
    assert.ifError(result.error); assert.equal(result.status, 2); assert.equal(result.stdout, '');
  }
  const source = readFileSync(runner, 'utf8');
  assert.match(source, /docker run --rm -d --network none/);
  assert.match(source, /docker rm -f "\$test_container_id"/);
  assert.match(source, /pg_blocking_pids/);
  assert.doesNotMatch(source, /DATABASE_URL|TEST_DSN|--env-file|--publish|\s-p\s/);
});
test('editor context formal controls, CI and migration order remain mandatory', () => {
  const gate = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  for (const [suffix, invariant] of [['Early','PrivateOptions'],['Candidate','EligibleOptions'],['Mixed','CoherentContext']]) {
    assert.ok(gate.includes(`expect_counterexample RaciEditorContext${suffix}.cfg ${invariant}`));
  }
  assert.match(gate, /for command_index in 0 1 2 3 4 5; do/);
  const ci = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(ci, /run: sh scripts\/test-event-raci-editor-context-migration.sh/);
  assert.doesNotMatch(ci, /continue-on-error/);
  for (const path of [runner, 'scripts/__tests__/event-raci-editor-context-runner.test.mjs',
    'tdf-hq/test/integration/event_raci_editor_context_*.sql']) {
    assert.equal(ci.split(`"${path}"`).length-1, 2);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  const command = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment.sql');
  const context = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_editor_context.sql');
  const down = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_editor_context_rollback.sql');
  const commandDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment_rollback.sql');
  assert.ok(command >= 0 && context > command && down > context && commandDown > down);
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json','utf8'), /2026-09-15_event_raci_editor_context/);
});
