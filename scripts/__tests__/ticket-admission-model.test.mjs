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

// One legacy refund request, two approval processes and one rejection process.
// Reads can be stale; claim and rejection CAS steps each inspect the current
// durable row atomically. The claim commits before the provider POST. Success,
// lost responses and process crashes are modeled independently; restarting a
// process loses its local permit but never resets the durable request status.
// Assumptions: stable request identity, no administrative status reset, atomic
// committed row updates, one POST per granted local permit. No fairness/liveness,
// provider authenticity, distinct-refund bounds, SQL or Haskell refinement claim.
function exploreLegacyRefundClaims({ deferClaimUntilResponse = false, staleUnconditionalReject = false } = {}) {
  const initial = {
    status: 'pending', approvals: ['start', 'start'], rejection: 'start',
    intentIssued: false, rejectedAfterIntent: false, executions: 0,
  };
  const queue = [{ state: initial, trace: [] }];
  const seen = new Set();
  const witnesses = {};
  const coverage = new Set();
  let transitions = 0;
  for (let cursor = 0; cursor < queue.length; cursor++) {
    const { state, trace } = queue[cursor];
    const key = JSON.stringify(state);
    if (seen.has(key)) continue;
    seen.add(key);
    if (state.executions > 1) witnesses.NoDuplicateExecution ??= trace;
    if (state.rejectedAfterIntent) witnesses.NoRejectionAfterIntent ??= trace;
    // Two POSTs already refute safety; a third adds no useful bounded evidence.
    if (state.executions > 1) continue;
    const push = (label, change) => {
      const next = structuredClone(state);
      change(next);
      transitions++;
      coverage.add(label);
      queue.push({ state: next, trace: [...trace, label] });
    };
    for (let actor = 0; actor < 2; actor++) {
      const name = actor === 0 ? 'A' : 'B';
      const phase = state.approvals[actor];
      if (phase === 'start') {
        push(`${name}:read-${state.status}`, next => {
          next.approvals[actor] = state.status === 'pending' ? 'read-pending' : 'done';
        });
      }
      if (phase === 'read-pending') {
        push(`${name}:claim-${state.status === 'pending' ? 'won' : 'denied'}`, next => {
          if (state.status === 'pending') {
            next.intentIssued = true;
            if (!deferClaimUntilResponse) next.status = 'processing';
            next.approvals[actor] = 'claimed';
          } else next.approvals[actor] = 'done';
        });
      }
      if (phase === 'claimed') {
        push(`${name}:POST`, next => {
          next.executions++;
          next.approvals[actor] = 'waiting';
        });
      }
      if (phase === 'waiting') {
        push(`${name}:response-succeeded`, next => {
          next.status = 'approved';
          next.approvals[actor] = 'done';
        });
        push(`${name}:response-lost`, next => { next.approvals[actor] = 'done'; });
      }
      if (['read-pending', 'claimed', 'waiting'].includes(phase)) {
        push(`${name}:crash-${phase}`, next => { next.approvals[actor] = 'crashed'; });
      }
      if (['crashed', 'done'].includes(phase)) {
        push(`${name}:retry`, next => { next.approvals[actor] = 'start'; });
      }
    }
    if (state.rejection === 'start') {
      push(`reject:read-${state.status}`, next => {
        next.rejection = state.status === 'pending' ? 'read-pending' : 'done';
      });
    }
    if (state.rejection === 'read-pending') {
      const allowed = state.status === 'pending' || staleUnconditionalReject;
      push(`reject:${allowed ? 'committed' : 'denied'}`, next => {
        if (allowed) {
          next.status = 'rejected';
          next.rejectedAfterIntent ||= state.intentIssued;
        }
        next.rejection = 'done';
      });
    }
  }
  return {
    states: seen.size, transitions, violations: Object.keys(witnesses).sort(),
    witnesses, coverage: [...coverage].sort(),
  };
}

test('bounded legacy refund claims survive races, response loss and process restarts', t => {
  const result = exploreLegacyRefundClaims();
  assert.equal(result.states, 208);
  assert.equal(result.transitions, 702);
  assert.deepEqual(result.violations, []);
  for (const label of ['A:POST', 'B:POST', 'A:response-succeeded', 'A:response-lost',
    'A:crash-claimed', 'A:crash-waiting', 'A:retry', 'reject:committed', 'reject:denied']) {
    assert.ok(result.coverage.includes(label), `Missing modeled scenario: ${label}`);
  }
  t.diagnostic(JSON.stringify({ model: 'legacy-refund-claim', states: result.states, transitions: result.transitions }));
});

test('legacy refund control exposes duplicate POSTs when the durable claim follows the response', t => {
  const result = exploreLegacyRefundClaims({ deferClaimUntilResponse: true });
  assert.equal(result.states, 303);
  assert.equal(result.transitions, 882);
  assert.ok(result.violations.includes('NoDuplicateExecution'));
  const trace = result.witnesses.NoDuplicateExecution;
  assert.equal(trace.filter(step => step.endsWith(':POST')).length, 2);
  t.diagnostic(JSON.stringify({ control: 'deferred-claim', states: result.states,
    transitions: result.transitions, counterexample: trace }));
});

test('legacy refund control exposes stale rejection after a committed execution intent', t => {
  const result = exploreLegacyRefundClaims({ staleUnconditionalReject: true });
  assert.equal(result.states, 266);
  assert.equal(result.transitions, 862);
  assert.deepEqual(result.violations, ['NoRejectionAfterIntent']);
  const trace = result.witnesses.NoRejectionAfterIntent;
  assert.ok(trace.includes('reject:read-pending'));
  assert.ok(trace.includes('A:claim-won') || trace.includes('B:claim-won'));
  assert.equal(trace.at(-1), 'reject:committed');
  // Rejected is not Pending: this mutant corrupts the execution boundary but
  // cannot by itself grant a second pending-only CAS permit.
  t.diagnostic(JSON.stringify({ control: 'stale-rejection', states: result.states,
    transitions: result.transitions, counterexample: trace }));
});
