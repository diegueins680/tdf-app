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

// Two independently refundable tickets; provider uncertainty is non-expiring.
// Bounded design model, not a proof of the SQL/Haskell implementation.
function exploreRefunds({ scanReserved = false, replayRelease = false, cancelProcessing = false } = {}) {
  const queue = [{ tickets: ['issued', 'issued'], refunds: ['none', 'none'], admitted: [false, false], sold: 2 }];
  const seen = new Set();
  const violations = new Set();
  while (queue.length) {
    const state = queue.shift();
    const key = JSON.stringify(state);
    if (seen.has(key)) continue;
    seen.add(key);
    if (state.sold !== state.tickets.filter(value => value !== 'refunded').length) violations.add('exact-inventory');
    if (state.tickets.some((value, i) => value === 'refunded' && state.admitted[i])) violations.add('unused-only');
    if (state.refunds.some((value, i) => value === 'processing' && state.tickets[i] !== 'reserved')) violations.add('uncertain-held');
    if (state.sold < 0) continue;
    const push = (i, ticket, refund, admitted = state.admitted[i], release = 0) => {
      const next = structuredClone(state);
      next.tickets[i] = ticket;
      next.refunds[i] = refund;
      next.admitted[i] = admitted;
      next.sold -= release;
      queue.push(next);
    };
    for (let i = 0; i < 2; i++) {
      const ticket = state.tickets[i];
      const refund = state.refunds[i];
      if (ticket === 'issued' && !state.admitted[i] && refund === 'none') push(i, 'reserved', 'requested');
      if ((ticket === 'issued' || (scanReserved && ticket === 'reserved')) && !state.admitted[i]) {
        push(i, ticket, refund, true);
      }
      if (refund === 'requested') {
        push(i, ticket, 'processing');
        push(i, 'issued', 'cancelled');
      }
      if (refund === 'processing') {
        push(i, 'refunded', 'succeeded', state.admitted[i], 1);
        if (cancelProcessing) push(i, 'issued', 'processing');
      }
      if (refund === 'succeeded' && replayRelease) push(i, ticket, refund, state.admitted[i], 1);
    }
  }
  return { states: seen.size, violations: [...violations].sort() };
}

test('bounded partial refunds conserve inventory and fence uncertain provider outcomes', () => {
  const result = exploreRefunds();
  assert.ok(result.states > 30);
  assert.deepEqual(result.violations, []);
});
test('refund negative controls expose admission race, duplicate release and premature cancellation', () => {
  assert.ok(exploreRefunds({ scanReserved: true }).violations.includes('unused-only'));
  assert.ok(exploreRefunds({ replayRelease: true }).violations.includes('exact-inventory'));
  assert.ok(exploreRefunds({ cancelProcessing: true }).violations.includes('uncertain-held'));
});

// An external refund may precede local binding by more than the eight inbox
// attempts. Design abstraction only: real SQL joins are tested separately.
function exploreLateRefund({ discardOnExhaustion = false, ignoreLateFence = false } = {}) {
  const queue = [{ bound: false, observed: false, fence: false, attempts: 0, admitted: false }];
  const seen = new Set();
  const violations = new Set();
  while (queue.length) {
    const state = queue.shift();
    const key = JSON.stringify(state);
    if (seen.has(key)) continue;
    seen.add(key);
    if (state.admitted) continue;
    if (!state.bound) queue.push({ ...state, bound: true });
    if (!state.observed) queue.push({ ...state, observed: true, fence: true });
    if (state.observed && !state.bound && state.attempts < 8) {
      const attempts = state.attempts + 1;
      queue.push({ ...state, attempts, fence: !(discardOnExhaustion && attempts === 8) });
    }
    if (state.bound && (!state.fence || ignoreLateFence)) {
      if (state.observed) violations.add('refunded-before-admission');
      queue.push({ ...state, admitted: true });
    }
  }
  return { states: seen.size, violations: [...violations] };
}

test('capture binding after exhausted refund retries still fences admission', () => {
  const result = exploreLateRefund();
  assert.ok(result.states > 15);
  assert.deepEqual(result.violations, []);
});

test('late-refund negative controls detect discarded evidence and missed capture joins', () => {
  assert.deepEqual(exploreLateRefund({ discardOnExhaustion: true }).violations, ['refunded-before-admission']);
  assert.deepEqual(exploreLateRefund({ ignoreLateFence: true }).violations, ['refunded-before-admission']);
});
