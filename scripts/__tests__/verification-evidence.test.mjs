import assert from 'node:assert/strict';
import test from 'node:test';
import { classifyExecution } from '../lib/verification-evidence.mjs';

test('success requires completed execution, exact source stability and completion evidence', () => {
  const log = 'Event operations formal verification passed within the documented finite bounds.';
  let accepted = 0;
  // Complete finite product of these admission predicates, including contradictory process outcomes.
  for (const status of [0, 1, null]) for (const signal of [null, 'SIGTERM'])
    for (const error of [undefined, 'spawn failed']) for (const complete of [true, false])
      for (const stable of [true, false]) {
        const result = classifyExecution({ status, signal, error, log: complete ? log : '', before: 'a', after: stable ? 'a' : 'b' });
        const passes = status === 0 && !signal && !error && complete && stable;
        assert.equal(result === 'bounded-model-checks-passed', passes);
        if (passes) accepted++;
      }
  assert.equal(accepted, 1);
});
