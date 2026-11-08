import test from 'node:test';
import assert from 'node:assert/strict';
import { verifyEventCrmModel } from '../../formal/event-operations/event-crm-model.mjs';
test('bounded CRM policy preserves invitation authority imports payments and closed transport', () => {
  const result = verifyEventCrmModel(); assert(result.states > 20); assert(result.transitions > result.states);
});
for (const mutation of ['wrong_recipient', 'non_atomic_consume', 'cached_authority', 'cross_event',
  'duplicate_import', 'spreadsheet_payment', 'capture_after_refund', 'bundle_without_joint_hold']) {
  test(`policy model finds unsafe control ${mutation}`, () => assert.throws(() => verifyEventCrmModel(mutation)));
}
