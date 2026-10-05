import assert from 'node:assert/strict';
import test from 'node:test';

// Finite abstraction of one ticket, two scanners and a cancellation/refund.
// This checks design invariants, not refinement of Haskell or PostgreSQL.
function explore({ allowReplay = false, allowOutsider = false, omitAudit = false } = {}) {
  const queue = [{ paid: true, valid: true, used: false, admissions: 0, audits: 0 }];
  const seen = new Set();
  const violations = new Set();
  while (queue.length) {
    const state = queue.shift();
    const key = JSON.stringify(state);
    if (seen.has(key)) continue;
    seen.add(key);
    if (state.admissions > 1) violations.add('at-most-once');
    if (state.admissions !== state.audits) violations.add('durable-audit');
    if (state.admissions > 1) continue;
    for (const actor of ['owner', 'outsider']) {
      if (state.paid && state.valid && (!state.used || allowReplay)
          && (actor === 'owner' || allowOutsider)) {
        if (actor !== 'owner') violations.add('authorized-only');
        queue.push({ ...state, used: true, admissions: state.admissions + 1,
          audits: state.audits + (omitAudit ? 0 : 1) });
      }
    }
    queue.push({ ...state, paid: false });
    queue.push({ ...state, valid: false });
  }
  return { states: seen.size, violations: [...violations].sort() };
}

test('all bounded interleavings preserve admission safety', () => {
  const result = explore();
  assert.equal(result.states, 8);
  assert.deepEqual(result.violations, []);
});
test('negative controls detect replay, authorization bypass and missing audit', () => {
  assert.deepEqual(explore({ allowReplay: true }).violations, ['at-most-once']);
  assert.deepEqual(explore({ allowOutsider: true }).violations, ['authorized-only']);
  assert.deepEqual(explore({ omitAudit: true }).violations, ['durable-audit']);
});
