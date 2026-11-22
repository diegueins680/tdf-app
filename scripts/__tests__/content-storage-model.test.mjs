import assert from 'node:assert/strict';
import { test } from 'node:test';
import { verifyContentStorageModel } from '../content-storage-model.mjs';
test('finite storage policy satisfies its bounded safety invariants', () => {
  const result = verifyContentStorageModel();
  assert.equal(result.violations.length, 0);
  assert.ok(result.cases > 500);
});
for (const fault of ['ignore-quota', 'duplicate-replay', 'stale-lease', 'authority-fallback', 'unbound-reader', 'foreign-credential', 'uncommitted-public']) {
  test(`detects deliberately unsafe ${fault}`, () => assert.ok(verifyContentStorageModel(fault).violations.length > 0));
}
