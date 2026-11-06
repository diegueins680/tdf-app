import assert from 'node:assert/strict';
import { test } from 'node:test';
import { verifyPosterRecoveryModel } from '../event-planning-ux-model.mjs';

test('poster recovery satisfies the finite owner/retry model', () => {
  const result = verifyPosterRecoveryModel();
  assert.ok(result.states > 20);
  assert.deepEqual(result.violations, []);
});
test('negative controls detect removed continuation guards and duplicate uploads', () => {
  for (const control of [{ guardUpload: false }, { guardConfirmation: false }, { reuseUpload: false }]) {
    assert.ok(verifyPosterRecoveryModel(control).violations.length > 0, JSON.stringify(control));
  }
});
