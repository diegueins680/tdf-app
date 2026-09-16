import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';

test('full formal gate requires completion client model and every named negative control', () => {
  const runner = readFileSync('scripts/verify-event-operations-formal.sh', 'utf8');
  assert.match(runner, /run_tlc TaskCompletionClient.tla TaskCompletionClient.cfg task-completion-client/);
  for (const [mutation, invariant] of Object.entries({ Capture: 'OriginalRequestSent',
    Shape: 'ValidatedReceipt', Binding: 'ValidatedReceipt', Retry: 'SingleDispatch' })) {
    assert.ok(runner.includes(`expect_counterexample TaskCompletionClient${mutation}.cfg ${invariant}`));
  }
});

test('formal workflow watches both completion client tests and its verification guard', () => {
  const workflow = readFileSync('.github/workflows/event-operations-formal.yml', 'utf8');
  for (const path of ['tdf-hq-ui/src/api/eventTaskCompletion*.test.ts',
    'scripts/__tests__/event-task-completion-client.test.mjs']) {
    assert.equal(workflow.split(`"${path}"`).length - 1, 2);
  }
  assert.match(workflow, /node --test scripts\/__tests__\/event-task-completion-client.test.mjs/);
});
