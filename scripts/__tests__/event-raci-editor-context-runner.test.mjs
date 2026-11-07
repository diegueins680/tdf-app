import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import test from 'node:test';
import { alloyControls, validateAlloyCoverage } from '../lib/alloy-controls.mjs';
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
  assert.match(gate, /node "\$\{SCRIPT_DIR\}\/verify-alloy-controls\.mjs" --output/);
  const taskRead = alloyControls.find(entry => entry.model === 'TaskReadStructure.als');
  assert.ok(taskRead, 'Task read Alloy model remains mandatory');
  const commands = validateAlloyCoverage(readFileSync('formal/event-operations/TaskReadStructure.als', 'utf8'), taskRead.controls);
  for (const name of ['CandidateRequiresCurrentManager', 'CandidateIsTaskScoped']) {
    assert.ok(commands.some(command => command.type === 'check' && command.name === name));
    assert.ok(taskRead.controls.some(control => control[1] === name));
  }
  const ci = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  assert.match(ci, /run: sh scripts\/test-event-raci-editor-context-migration.sh/);
  assert.doesNotMatch(ci, /continue-on-error/);
  for (const path of [runner, 'scripts/__tests__/event-raci-editor-context-runner.test.mjs',
    'tdf-hq/test/integration/event_raci_editor_context_*.sql']) {
    // Main deliberately verifies every change; path filtering would narrow coverage.
    assert.match(ci, /^  pull_request:\s*$/m);
    assert.match(ci, /^  push:\s*\n\s+branches: \[main\]/m);
    assert.doesNotMatch(ci, /^\s+paths(?:-ignore)?:/m);
  }
  const rehearsal = readFileSync('scripts/test-event-operations-schema-rehearsal.sh', 'utf8');
  const command = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment.sql');
  const context = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_editor_context.sql');
  const down = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_editor_context_rollback.sql');
  const commandDown = rehearsal.indexOf('apply_sql tdf-hq/sql/2026-09-15_event_raci_reassignment_rollback.sql');
  assert.ok(command >= 0 && context > command && down > context && commandDown > down);
  assert.doesNotMatch(readFileSync('scripts/production-migrations.json','utf8'), /2026-09-15_event_raci_editor_context/);
});
