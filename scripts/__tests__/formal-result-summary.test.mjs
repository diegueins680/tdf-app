import assert from 'node:assert/strict';
import test from 'node:test';
import { tlcSummary } from '../lib/formal-result-summary.mjs';

test('counts both admitted negative helpers without interpreting raw errors as results', () => {
  const result = tlcSummary('TDF_TLC_RESULT positive Good.tla Good.cfg\nError: Invariant X is violated\nTDF_TLC_RESULT negative Good.tla Broken.cfg\nExpected mutation counterexample: Other.cfg: Y\nTDF_TLC_RESULT negative Other.tla Other.cfg\n');
  assert.equal(result.positive, 1);
  assert.equal(result.negative, 2);
  assert.equal(result.configurations.length, 3);
});
test('duplicates, conflicting classifications and malformed records reject', () => {
  const first = 'TDF_TLC_RESULT positive A.tla A.cfg';
  for (const second of [first, 'TDF_TLC_RESULT negative A.tla A.cfg', 'TDF_TLC_RESULT invalid A.tla A.cfg']) assert.throws(() => tlcSummary(`${first}\n${second}`), /Duplicate|Malformed/);
});
test('incomplete or historical unmarked logs do not fabricate model executions', () => {
  assert.deepEqual(tlcSummary('Model checking completed. No error has been found.'), { positive: 0, negative: 0, configurations: [] });
});
