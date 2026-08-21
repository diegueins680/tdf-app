import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import test from 'node:test';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';

const execFileAsync = promisify(execFile);
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

test('validates the deterministic persona catalog and story coverage', async () => {
  const { stdout } = await execFileAsync(process.execPath, ['scripts/persona-test-program.mjs', 'validate'], { cwd: root });
  assert.match(stdout, /26 personas, 78 stories, 17 epics/);
});

test('generated scenarios contain complete lifecycle and evidence fields', async () => {
  const payload = JSON.parse(await readFile(path.join(root, 'test/personas/scenarios.json'), 'utf8'));
  assert.equal(payload.scenarios.length, 78);
  assert.equal(new Set(payload.scenarios.map((scenario) => scenario.epic.id)).size, 17);
  for (const scenario of payload.scenarios) {
    assert.match(scenario.id, /^ST-\d{3}$/);
    assert.ok(scenario.preconditions.length >= 3);
    assert.ok(scenario.testSteps.length >= 7);
    assert.ok(scenario.expectedVisibleBehavior.length >= 3);
    assert.ok(scenario.expectedBackendStateAndSideEffects.length >= 3);
    assert.ok(scenario.expectedNotificationsAndAuditEvents.length >= 3);
    assert.ok(scenario.cleanupRequirements.length >= 3);
    assert.ok(scenario.execution.status);
  }
});

test('traceability matrix has one row per story and every required column', async () => {
  const traceability = await readFile(path.join(root, 'docs/persona-testing/traceability.csv'), 'utf8');
  const lines = traceability.trimEnd().split('\n');
  assert.equal(lines.length, 79);
  for (const column of [
    'Persona', 'Role combination', 'Epic', 'User story', 'Feature', 'Platform',
    'Expected permission', 'Test type', 'Test identifier', 'Execution status',
    'Finding identifiers', 'Fix or GitHub issue', 'Evidence location',
  ]) assert.ok(lines[0].includes(column));
});
