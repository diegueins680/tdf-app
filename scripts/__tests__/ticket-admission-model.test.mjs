import assert from 'node:assert/strict';
import test from 'node:test';

// Two requesters and one eligibility change. The event/order lock covers both
// the current check and insert, and READ COMMITTED refreshes the existence read
// after a waiter acquires the lock. This is a design abstraction, not SQL proof.
function exploreLegacyRequestCreation({ omitLock = false, staleSnapshot = false,
  staleEligibility = false } = {}) {
  let inputs = 0, states = 0, transitions = 0;
  const violations = new Set();
  let counterexample;
  for (const initialCount of [0, 1]) {
    for (const initiallyEligible of [false, true]) {
      inputs++;
      const queue = [{ count: initialCount, eligible: initiallyEligible, mutated: false,
        lock: null, phase: [0, 0], snapshot: [null, null], eligibility: [null, null],
        readCount: [null, null], readEligibility: [null, null], unauthorized: 0, trace: [] }];
      const seen = new Set();
      while (queue.length) {
        const current = queue.shift();
        const { trace, ...identity } = current;
        const key = JSON.stringify(identity);
        if (seen.has(key)) continue;
        seen.add(key);
        const invalid = [];
        if (current.count > Math.max(initialCount, 1)) invalid.push('one-request-per-order');
        if (current.unauthorized) invalid.push('current-eligibility');
        for (const failure of invalid) violations.add(failure);
        if (invalid.length && !counterexample) counterexample = { initialCount, initiallyEligible, ...current };
        const push = (next, action) => {
          transitions++;
          next.trace = [...trace, action];
          queue.push(next);
        };
        if (!current.mutated && current.lock === null) {
          push({ ...structuredClone(current), eligible: false, mutated: true }, 'eligibility-changed');
        }
        for (let actor = 0; actor < 2; actor++) {
          const next = structuredClone(current);
          const phase = current.phase[actor];
          if (phase === 0) {
            next.snapshot[actor] = current.count;
            next.eligibility[actor] = current.eligible;
            next.phase[actor] = current.eligible ? 1 : 4;
            push(next, `${actor}:initial-read`);
          } else if (phase === 1 && (omitLock || current.lock === null)) {
            if (!omitLock) next.lock = actor;
            next.phase[actor] = 2;
            push(next, `${actor}:acquire-lock`);
          } else if (phase === 2) {
            next.readCount[actor] = staleSnapshot ? current.snapshot[actor] : current.count;
            next.readEligibility[actor] = staleEligibility ? current.eligibility[actor] : current.eligible;
            next.phase[actor] = 3;
            push(next, `${actor}:current-check`);
          } else if (phase === 3) {
            if (current.readCount[actor] === 0 && current.readEligibility[actor]) {
              next.count++;
              if (!current.eligible) next.unauthorized++;
            }
            if (current.lock === actor) next.lock = null;
            next.phase[actor] = 4;
            push(next, `${actor}:insert-or-refuse`);
          }
        }
      }
      states += seen.size;
    }
  }
  return { inputs, states, transitions, violations: [...violations].sort(), counterexample };
}

test('bounded legacy request creation serializes absence, eligibility and insertion', t => {
  const result = exploreLegacyRequestCreation();
  assert.equal(result.inputs, 4);
  assert.equal(result.states, 168);
  assert.equal(result.transitions, 203);
  assert.deepEqual(result.violations, []);
  t.diagnostic(JSON.stringify({ model: 'legacy-request-creation', ...result }));
});

test('legacy request controls expose absent locks, stale snapshots and stale eligibility', t => {
  for (const [control, options, violation] of [
    ['omit-request-lock', { omitLock: true }, 'one-request-per-order'],
    ['stale-request-snapshot', { staleSnapshot: true }, 'one-request-per-order'],
    ['stale-request-eligibility', { staleEligibility: true }, 'current-eligibility'],
  ]) {
    const result = exploreLegacyRequestCreation(options);
    assert.ok(result.violations.includes(violation));
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// Two distinct legacy requests release one shared tier. A deliberate stale
// precheck models why stock must be tested by the UPDATE that decrements it.
// This finite design check is not a Haskell/SQL refinement proof.
function exploreLegacyInventory({ stalePrecheck = false, omitEvent = false,
  omitPositiveQuantity = false } = {}) {
  let inputs = 0, states = 0, transitions = 0;
  const violations = new Set();
  let counterexample;
  for (let initial = -1; initial <= 6; initial++) {
    for (let first = -1; first <= 3; first++) {
      for (let second = -1; second <= 3; second++) {
        for (let mask = 0; mask < 4; mask++) {
          inputs++;
          const quantities = [first, second];
          const matches = [Boolean(mask & 1), Boolean(mask & 2)];
          const queue = [{ sold: initial, phase: [0, 0], trace: [] }];
          const seen = new Set();
          while (queue.length) {
            const current = queue.shift();
            const key = JSON.stringify([current.sold, current.phase]);
            if (seen.has(key)) continue;
            seen.add(key);
            const invalid = [];
            if (initial >= 0 && current.sold < 0) invalid.push('nonnegative');
            if (current.phase.some((phase, index) => phase === 2 && !matches[index])) invalid.push('event-scoped');
            if (current.phase.some((phase, index) => phase === 2 && quantities[index] <= 0)) invalid.push('positive-release');
            const released = quantities.reduce((sum, quantity, index) =>
              sum + (current.phase[index] === 2 ? quantity : 0), 0);
            assert.equal(current.sold, initial - released);
            for (const failure of invalid) violations.add(failure);
            if (invalid.length && !counterexample) counterexample = { initial, quantities, matches, ...current };
            for (let actor = 0; actor < 2; actor++) {
              const phase = current.phase[actor];
              if (phase >= 2) continue;
              const quantity = quantities[actor];
              const allowed = (omitPositiveQuantity || quantity > 0)
                && (omitEvent || matches[actor]) && current.sold >= quantity;
              const next = { sold: current.sold, phase: [...current.phase],
                trace: [...current.trace, `${actor}:${phase === 1 ? 'release-after-precheck' : 'conditional-release'}`] };
              if (phase === 1 || allowed) {
                next.phase[actor] = stalePrecheck && phase === 0 ? 1 : 2;
                if (next.phase[actor] === 2) next.sold -= quantity;
              } else next.phase[actor] = 3;
              transitions++;
              queue.push(next);
            }
          }
          states += seen.size;
        }
      }
    }
  }
  return { inputs, states, transitions, violations: [...violations].sort(), counterexample };
}

test('bounded atomic legacy inventory releases remain positive, event-scoped and nonnegative', t => {
  const result = exploreLegacyInventory();
  assert.equal(result.inputs, 800);
  assert.equal(result.states, 3214);
  assert.equal(result.transitions, 3200);
  assert.deepEqual(result.violations, []);
  t.diagnostic(JSON.stringify({ model: 'legacy-inventory-release', ...result }));
});

test('legacy inventory controls expose stale stock reads, foreign tiers and nonpositive quantities', t => {
  for (const [control, options, violation] of [
    ['stale-precheck', { stalePrecheck: true }, 'nonnegative'],
    ['omit-event', { omitEvent: true }, 'event-scoped'],
    ['omit-positive-quantity', { omitPositiveQuantity: true }, 'positive-release'],
  ]) {
    const result = exploreLegacyInventory(options);
    assert.ok(result.violations.includes(violation));
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

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

// Completion boundary after the durable claim and its one provider invocation.
// The 224 inputs enumerate five independent validation predicates and seven
// statuses (the five official statuses, one unknown value and null). "id" means
// syntactically valid observed correlation, not proof of money/intent binding.
// Amount/currency/intent predicates abstract type and exact stored-value checks.
// The request does not ask Stripe to expand payment_intent; only its string ID
// is accepted here, although the provider's general schema permits expansion.
// This is bounded design verification, not decoder/SQL/Haskell refinement,
// provider authentication, partial-ticket allocation or eventual recovery proof.
function exploreLegacyRefundCompletion({ omitGuard = null, releaseWhileHeld = false, retryPostsAgain = false } = {}) {
  const bindings = ['object', 'id', 'amount', 'currency', 'intent'];
  const statuses = ['pending', 'requires_action', 'succeeded', 'failed', 'canceled', 'unknown', null];
  const inputs = [];
  for (let bits = 0; bits < 32; bits++) {
    for (const status of statuses) {
      const evidence = Object.fromEntries(bindings.map((name, index) => [name, Boolean(bits & (1 << index))]));
      inputs.push({ ...evidence, status });
    }
  }
  const queue = inputs.map(evidence => ({
    state: { evidence, phase: 'processing', observedId: false, sold: 3, posts: 1 }, trace: [],
  }));
  const seen = new Set();
  const witnesses = {};
  let transitions = 0;
  let completed = 0;
  let held = 0;
  for (let cursor = 0; cursor < queue.length; cursor++) {
    const { state, trace } = queue[cursor];
    const key = JSON.stringify(state);
    if (seen.has(key)) continue;
    seen.add(key);
    const exact = bindings.every(name => state.evidence[name]) && state.evidence.status === 'succeeded';
    const violation = name => { witnesses[name] ??= { evidence: state.evidence, trace }; };
    if (state.phase === 'completed') {
      completed++;
      if (!exact || !state.observedId) violation('CompleteImpliesExactEvidence');
    }
    if (state.phase === 'held') {
      held++;
      if (state.sold !== 3) violation('HeldPreservesInventory');
    }
    if (state.posts > 1) {
      violation('NoSecondPost');
      continue;
    }
    const push = (label, change) => {
      const next = structuredClone(state);
      change(next);
      transitions++;
      queue.push({ state: next, trace: [...trace, label] });
    };
    const hold = next => {
      next.phase = 'held';
      if (releaseWhileHeld) next.sold = 0;
    };
    if (state.phase === 'processing') {
      if (state.evidence.id || omitGuard === 'id') {
        push('observe-provider-id', next => {
          next.phase = 'observed';
          next.observedId = state.evidence.id;
        });
      } else push('hold-invalid-id', hold);
    }
    if (state.phase === 'observed') {
      const accepted = bindings.every(name => state.evidence[name] || omitGuard === name)
        && (state.evidence.status === 'succeeded' || omitGuard === 'status');
      if (accepted) {
        push(`complete-${String(state.evidence.status)}`, next => {
          next.phase = 'completed';
          next.sold = 0;
        });
      } else push('hold-unverified-evidence', hold);
    }
    if (retryPostsAgain && state.phase === 'held') {
      push('retry:POST', next => { next.posts++; });
    } else push('retry:denied', () => {});
  }
  return { inputs: inputs.length, states: seen.size, transitions, completed, held,
    violations: Object.keys(witnesses).sort(), witnesses };
}

test('bounded legacy refund completion requires exact succeeded evidence and retains uncertain inventory', t => {
  const result = exploreLegacyRefundCompletion();
  assert.equal(result.inputs, 224);
  assert.equal(result.states, 560);
  assert.equal(result.transitions, 896);
  assert.equal(result.completed, 1);
  assert.equal(result.held, 223);
  assert.deepEqual(result.violations, []);
  t.diagnostic(JSON.stringify({ model: 'legacy-refund-completion', inputs: result.inputs,
    states: result.states, transitions: result.transitions, completed: result.completed, held: result.held }));
});

test('legacy completion controls reject omission of every response binding, type or status guard', t => {
  for (const omitGuard of ['object', 'id', 'amount', 'currency', 'intent', 'status']) {
    const result = exploreLegacyRefundCompletion({ omitGuard });
    assert.equal(result.states, omitGuard === 'id' ? 672 : 560);
    assert.equal(result.transitions, omitGuard === 'id' ? 1120 : 896);
    assert.deepEqual(result.violations, ['CompleteImpliesExactEvidence']);
    const counterexample = result.witnesses.CompleteImpliesExactEvidence;
    assert.equal(counterexample.trace.at(-1).startsWith('complete-'), true);
    assert.equal(omitGuard === 'status' ? counterexample.evidence.status === 'succeeded'
      : counterexample.evidence[omitGuard], false);
    t.diagnostic(JSON.stringify({ control: `omit-${omitGuard}`, states: result.states,
      transitions: result.transitions, counterexample }));
  }
});

test('legacy completion control detects inventory release for held evidence', t => {
  const result = exploreLegacyRefundCompletion({ releaseWhileHeld: true });
  assert.equal(result.states, 560);
  assert.equal(result.transitions, 896);
  assert.deepEqual(result.violations, ['HeldPreservesInventory']);
  t.diagnostic(JSON.stringify({ control: 'release-held-inventory', states: result.states,
    transitions: result.transitions, counterexample: result.witnesses.HeldPreservesInventory }));
});

test('legacy completion control detects a second POST while evidence is held', t => {
  const result = exploreLegacyRefundCompletion({ retryPostsAgain: true });
  assert.equal(result.states, 783);
  assert.equal(result.transitions, 896);
  assert.deepEqual(result.violations, ['NoSecondPost']);
  assert.equal(result.witnesses.NoSecondPost.trace.at(-1), 'retry:POST');
  t.diagnostic(JSON.stringify({ control: 'retry-held-POST', states: result.states,
    transitions: result.transitions, counterexample: result.witnesses.NoSecondPost }));
});

// Historical identities are immutable inputs here. Creation uses the same order
// lock and refuses existing history; direct writers and canonical allocations
// are outside this abstraction. Claim commits before an uncertain provider POST.
function exploreLegacyRefundHistory({ omitHistory = false, pendingOnly = false } = {}) {
  const statuses = ['pending', 'rejected', 'processing', 'approved'];
  const inputs = [...statuses.map(status => [status]),
    ...statuses.flatMap(first => statuses.map(second => [first, second]))];
  let states = 0, transitions = 0;
  const coverage = new Set();
  const witnesses = {};
  for (const history of inputs) {
    const queue = [{ rows: history, phase: [0, 0], posts: [0, 0], trace: [] }];
    const seen = new Set();
    while (queue.length) {
      const current = queue.shift();
      const { trace, ...identity } = current;
      const key = JSON.stringify(identity);
      if (seen.has(key)) continue;
      seen.add(key);
      const posts = current.posts.reduce((sum, count) => sum + count, 0);
      if (history.length > 1 && posts) witnesses.AmbiguousHistoryNeverPosts ??= { history, trace };
      if (posts > 1) witnesses.AtMostOnePost ??= { history, trace };
      for (let actor = 0; actor < 2; actor++) {
        const target = history.length === 1 ? 0 : actor;
        const next = structuredClone(current);
        let action;
        if (current.phase[actor] === 0) {
          const visible = pendingOnly ? current.rows.filter(status => status === 'pending').length
            : current.rows.length;
          const eligible = current.rows[target] === 'pending' && (omitHistory || visible === 1);
          next.phase[actor] = eligible ? 1 : 3;
          if (eligible) next.rows[target] = 'processing';
          action = eligible ? 'claim' : 'refuse';
        } else if (current.phase[actor] === 1) {
          next.posts[actor]++;
          next.phase[actor] = 2;
          action = 'POST-response-lost';
        } else if (current.phase[actor] === 2) {
          next.phase[actor] = 3;
          action = 'retry-refused';
        } else continue;
        coverage.add(action);
        transitions++;
        next.trace = [...trace, `${actor}:${action}`];
        queue.push(next);
      }
    }
    states += seen.size;
  }
  return { inputs: inputs.length, states, transitions, coverage: [...coverage].sort(),
    violations: Object.keys(witnesses).sort(), witnesses };
}

test('bounded legacy approval refuses every ambiguous historical identity before POST', t => {
  const result = exploreLegacyRefundHistory();
  assert.equal(result.inputs, 20);
  assert.equal(result.states, 89);
  assert.equal(result.transitions, 92);
  assert.deepEqual(result.violations, []);
  assert.deepEqual(result.coverage, ['POST-response-lost', 'claim', 'refuse', 'retry-refused']);
  t.diagnostic(JSON.stringify({ model: 'legacy-refund-history', ...result }));
});

test('legacy history controls detect omitted history and pending-only filtering', t => {
  const omitted = exploreLegacyRefundHistory({ omitHistory: true });
  assert.equal(omitted.states, 125);
  assert.equal(omitted.transitions, 148);
  assert.deepEqual(omitted.violations, ['AmbiguousHistoryNeverPosts', 'AtMostOnePost']);
  assert.equal(omitted.witnesses.AtMostOnePost.trace.filter(step => step.endsWith('POST-response-lost')).length, 2);
  const filtered = exploreLegacyRefundHistory({ pendingOnly: true });
  assert.equal(filtered.states, 113);
  assert.equal(filtered.transitions, 128);
  assert.deepEqual(filtered.violations, ['AmbiguousHistoryNeverPosts']);
  for (const [control, result] of [['omit-history', omitted], ['pending-only', filtered]]) {
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// Tier editing uses an atomic current-sold predicate; reservation reads the tier
// under its row lock. Event capacity is fixed per input here. This model does
// not refine SQL or prove fresh workflow, pricing, identity or provider behavior.
function exploreTierEdits({ staleEditSold = false, staleTierTotal = false, omitTierLock = false } = {}) {
  let inputs = 0, states = 0, transitions = 0;
  const witnesses = {};
  const coverage = new Set();
  for (const sold of [0, 1]) for (const total of [2, 4]) {
    for (const requested of [0, 1, 4]) for (const quantity of [1, 3]) for (const capacity of [null, 4]) {
      inputs++;
      const initial = { sold, total, requested, quantity, capacity };
      const queue = [{ sold, total, phase: [0, 0], editSold: null, outerTotal: null,
        bound: null, tierLocked: false, trace: [] }];
      const seen = new Set();
      while (queue.length) {
        const current = queue.shift();
        const { trace, ...identity } = current;
        const key = JSON.stringify(identity);
        if (seen.has(key)) continue;
        seen.add(key);
        if (current.sold > current.total) witnesses.SoldNeverExceedsTierTotal ??= { initial, trace };
        const push = (next, label) => {
          transitions++;
          coverage.add(label);
          next.trace = [...trace, label];
          queue.push(next);
        };
        if (current.phase[0] === 0) {
          const next = structuredClone(current);
          next.editSold = current.sold;
          next.phase[0] = requested >= current.sold ? 1 : 3;
          push(next, 'edit:outer-read');
        } else if (current.phase[0] === 1 && !current.tierLocked) {
          const next = structuredClone(current);
          const allowed = requested >= (staleEditSold ? current.editSold : current.sold);
          if (allowed) next.total = requested;
          next.phase[0] = 3;
          push(next, allowed ? 'edit:commit' : 'edit:conflict');
        }
        if (current.phase[1] === 0) {
          const next = structuredClone(current);
          next.outerTotal = current.total;
          next.phase[1] = quantity <= current.total - current.sold ? 1 : 3;
          push(next, 'purchase:outer-read');
        } else if (current.phase[1] === 1) {
          const next = structuredClone(current);
          next.tierLocked = !omitTierLock;
          next.bound = staleTierTotal ? current.outerTotal : current.total;
          next.phase[1] = 2;
          push(next, 'purchase:locked-read');
        } else if (current.phase[1] === 2) {
          const next = structuredClone(current);
          const allowed = quantity <= current.bound - current.sold
            && (capacity === null || current.sold + quantity <= capacity);
          if (allowed) next.sold += quantity;
          next.phase[1] = 3;
          next.tierLocked = false;
          push(next, allowed ? 'purchase:commit' : 'purchase:conflict');
        }
      }
      states += seen.size;
    }
  }
  return { inputs, states, transitions, coverage: [...coverage].sort(),
    violations: Object.keys(witnesses), witnesses };
}

test('bounded tier editing and reservation preserve current sold bounds in either order', t => {
  const result = exploreTierEdits();
  assert.equal(result.inputs, 48);
  assert.equal(result.states, 606);
  assert.equal(result.transitions, 658);
  assert.deepEqual(result.violations, []);
  for (const label of ['edit:commit', 'edit:conflict', 'purchase:commit', 'purchase:conflict']) {
    assert.ok(result.coverage.includes(label), `Missing tier scenario: ${label}`);
  }
  t.diagnostic(JSON.stringify({ model: 'tier-edit-reservation', ...result }));
});

test('tier controls detect stale sold, stale total and omitted tier locking', t => {
  for (const control of ['staleEditSold', 'staleTierTotal', 'omitTierLock']) {
    const result = exploreTierEdits({ [control]: true });
    const bounds = { staleEditSold: [606, 658], staleTierTotal: [598, 658], omitTierLock: [642, 710] };
    assert.equal(result.states, bounds[control][0]);
    assert.equal(result.transitions, bounds[control][1]);
    assert.deepEqual(result.violations, ['SoldNeverExceedsTierTotal']);
    const trace = result.witnesses.SoldNeverExceedsTierTotal.trace;
    assert.ok(trace.includes('edit:commit') && trace.includes('purchase:commit'));
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// Event modes abstract the existing eligibility predicate, not the complete
// workflow graph or mutable capability catalog. One ordinary event mutation
// and one new hold serialize on Event. Previously admitted orders may remain.
function explorePurchaseEligibility({ staleEligibility = false, omitEventLock = false, effectsBeforeCheck = false } = {}) {
  let inputs = 0, states = 0, transitions = 0, retainedAfterClosureStates = 0;
  const witnesses = {};
  const coverage = new Set();
  for (const initial of ['open', 'closed', 'private']) for (const target of ['closed', 'private']) {
    inputs++;
    const queue = [{ mode: initial, lock: null, phase: [0, 0], snapshot: null,
      observed: null, holds: 0, unauthorized: 0, result: null, trace: [] }];
    const seen = new Set();
    while (queue.length) {
      const current = queue.shift();
      const { trace, ...identity } = current;
      const key = JSON.stringify(identity);
      if (seen.has(key)) continue;
      seen.add(key);
      if (current.unauthorized) witnesses.NoNewHoldAfterClosure ??= { initial, target, trace };
      if (current.result && current.result !== 'accepted' && current.holds) {
        witnesses.RefusalHasNoEffects ??= { initial, target, trace };
      }
      if (current.mode !== 'open' && current.result === 'accepted' && !current.unauthorized) {
        retainedAfterClosureStates++;
      }
      const push = (next, label) => {
        transitions++;
        coverage.add(label);
        next.trace = [...trace, label];
        queue.push(next);
      };
      if (current.phase[0] === 0 && current.lock === null) {
        const next = structuredClone(current);
        next.lock = 'editor';
        next.phase[0] = 1;
        push(next, 'editor:lock');
      } else if (current.phase[0] === 1) {
        const next = structuredClone(current);
        next.mode = target;
        next.phase[0] = 2;
        next.lock = null;
        push(next, `editor:${target}`);
      }
      if (current.phase[1] === 0) {
        const next = structuredClone(current);
        next.snapshot = current.mode;
        next.phase[1] = current.mode === 'open' ? 1 : 4;
        if (current.mode !== 'open') next.result = current.mode;
        push(next, current.mode === 'open' ? 'purchase:outer-read' : 'purchase:outer-refuse');
      } else if (current.phase[1] === 1 && (omitEventLock || current.lock === null)) {
        const next = structuredClone(current);
        if (!omitEventLock) next.lock = 'buyer';
        next.phase[1] = 2;
        push(next, 'purchase:lock');
      } else if (current.phase[1] === 2) {
        const next = structuredClone(current);
        next.observed = staleEligibility ? current.snapshot : current.mode;
        next.phase[1] = 3;
        push(next, effectsBeforeCheck ? 'purchase:prepare-effects-before-check' : 'purchase:current-check');
      } else if (current.phase[1] === 3) {
        const next = structuredClone(current);
        const accepted = current.observed === 'open';
        // The mutant commits prepared effects despite a returned refusal;
        // temporary writes that later roll back are not durable violations.
        if (accepted || effectsBeforeCheck) {
          next.holds++;
          if (current.mode !== 'open') next.unauthorized++;
        }
        next.result = accepted ? 'accepted' : current.observed;
        next.phase[1] = 4;
        if (current.lock === 'buyer') next.lock = null;
        push(next, accepted ? 'purchase:accept' : 'purchase:deny');
      }
    }
    states += seen.size;
  }
  return { inputs, states, transitions, retainedAfterClosureStates,
    coverage: [...coverage].sort(), violations: Object.keys(witnesses).sort(), witnesses };
}

test('bounded new-hold admission checks current locked event eligibility before effects', t => {
  const result = explorePurchaseEligibility();
  assert.equal(result.inputs, 6);
  assert.equal(result.states, 56);
  assert.equal(result.transitions, 58);
  assert.equal(result.retainedAfterClosureStates, 2);
  assert.deepEqual(result.violations, []);
  assert.ok(result.retainedAfterClosureStates > 0);
  for (const label of ['editor:closed', 'editor:private', 'purchase:accept', 'purchase:deny', 'purchase:outer-refuse']) {
    assert.ok(result.coverage.includes(label), `Missing eligibility scenario: ${label}`);
  }
  t.diagnostic(JSON.stringify({ model: 'new-hold-eligibility', ...result }));
});

test('eligibility controls detect cached predicates, omitted locking and effects before refusal', t => {
  for (const control of ['staleEligibility', 'omitEventLock', 'effectsBeforeCheck']) {
    const result = explorePurchaseEligibility({ [control]: true });
    assert.equal(result.states, control === 'omitEventLock' ? 64 : 56);
    assert.equal(result.transitions, control === 'omitEventLock' ? 74 : 58);
    assert.match(result.witnesses.NoNewHoldAfterClosure.trace.at(-1), /purchase:(accept|deny)$/);
    assert.ok(result.violations.includes('NoNewHoldAfterClosure'));
    assert.equal(result.violations.includes('RefusalHasNoEffects'), control === 'effectsBeforeCheck');
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// One editor and one new hold share Tier. Event eligibility/terms have already
// succeeded. Financial identity is required at admission, not after later edits.
function exploreTierPricing({ omitPrice = false, omitCurrency = false,
  omitTierLock = false, effectsBeforeCheck = false } = {}) {
  const pairs = [0, 1].flatMap(price => ['USD', 'EUR'].map(currency => ({ price, currency })));
  const same = (a, b) => a.price === b.price && a.currency.toUpperCase() === b.currency.toUpperCase();
  const coverage = new Set(), witnesses = {};
  let inputs = 0, states = 0, transitions = 0, retainedHistoricalStates = 0;
  for (const initial of pairs) for (const target of pairs) {
    if (same(initial, target)) continue;
    inputs++;
    const queue = [{ tier: initial, quote: null, observed: null, lock: null,
      phase: [0, 0], result: null, effects: 0, mismatchAtAdmission: false, trace: [] }];
    const seen = new Set();
    while (queue.length) {
      const current = queue.shift(), { trace, ...identity } = current;
      const key = JSON.stringify(identity);
      if (seen.has(key)) continue;
      seen.add(key);
      if (current.mismatchAtAdmission) witnesses.FinancialSnapshotMatchesAtAdmission ??= { initial, target, trace };
      if (current.result === 'refused' && current.effects) {
        witnesses.RefusalHasNoEffects ??= { initial, target, trace };
      }
      if (current.phase[0] === 2 && current.result === 'accepted'
        && !current.mismatchAtAdmission && !same(current.quote, current.tier)) retainedHistoricalStates++;
      const push = (next, label) => {
        transitions++;
        coverage.add(label);
        next.trace = [...trace, label];
        queue.push(next);
      };
      if (current.phase[0] === 0 && current.lock === null) {
        const next = structuredClone(current);
        next.lock = 'editor'; next.phase[0] = 1;
        push(next, 'editor:lock');
      } else if (current.phase[0] === 1) {
        const next = structuredClone(current);
        next.tier = target; next.lock = null; next.phase[0] = 2;
        push(next, 'editor:commit');
      }
      if (current.phase[1] === 0) {
        const next = structuredClone(current);
        next.quote = current.tier; next.phase[1] = 1;
        push(next, 'purchase:quote');
      } else if (current.phase[1] === 1 && (omitTierLock || current.lock === null)) {
        const next = structuredClone(current);
        if (!omitTierLock) next.lock = 'buyer';
        next.phase[1] = 2;
        push(next, 'purchase:lock');
      } else if (current.phase[1] === 2) {
        const next = structuredClone(current);
        next.observed = current.tier; next.phase[1] = 3;
        push(next, 'purchase:check');
      } else if (current.phase[1] === 3) {
        const next = structuredClone(current);
        const accepted = (omitPrice || current.quote.price === current.observed.price)
          && (omitCurrency || current.quote.currency.toUpperCase() === current.observed.currency.toUpperCase());
        next.effects = accepted || effectsBeforeCheck ? 1 : 0;
        next.mismatchAtAdmission = accepted && !same(current.quote, current.tier);
        next.result = accepted ? 'accepted' : 'refused'; next.phase[1] = 4;
        if (current.lock === 'buyer') next.lock = null;
        push(next, accepted ? 'purchase:accept' : 'purchase:refuse');
      }
    }
    states += seen.size;
  }
  return { inputs, states, transitions, retainedHistoricalStates,
    coverage: [...coverage].sort(), violations: Object.keys(witnesses).sort(), witnesses };
}

test('bounded new holds bind price and currency at admission while retaining historical snapshots', t => {
  const result = exploreTierPricing();
  assert.equal(result.inputs, 12);
  assert.equal(result.states, 216);
  assert.equal(result.transitions, 216);
  assert.equal(result.retainedHistoricalStates, 12);
  assert.deepEqual(result.violations, []);
  for (const label of ['editor:commit', 'purchase:accept', 'purchase:refuse']) {
    assert.ok(result.coverage.includes(label), `Missing pricing scenario: ${label}`);
  }
  t.diagnostic(JSON.stringify({ model: 'new-hold-tier-pricing', ...result }));
});

test('pricing controls detect omitted fields, omitted Tier locking and committed refusal effects', t => {
  for (const control of ['omitPrice', 'omitCurrency', 'omitTierLock', 'effectsBeforeCheck']) {
    const result = exploreTierPricing({ [control]: true });
    assert.equal(result.states, control === 'omitTierLock' ? 264 : 216);
    assert.equal(result.transitions, control === 'omitTierLock' ? 312 : 216);
    const property = control === 'effectsBeforeCheck' ? 'RefusalHasNoEffects' : 'FinancialSnapshotMatchesAtAdmission';
    assert.deepEqual(result.violations, [property]);
    const trace = result.witnesses[property].trace;
    assert.match(trace.at(-1), /purchase:(accept|refuse)$/);
    assert.ok(trace.includes('editor:commit'));
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// A new legacy claim is atomic with respect to the promo editor. Later edits
// may legitimately lower the cap below historical usage; check at claim time.
function explorePromoLimit({ cachedLimit = false, effectsOnRefusal = false } = {}) {
  const limits = [null, 0, 1, 2];
  const allowed = (count, limit) => limit === null || count < limit;
  const schedules = [['quote', 'edit', 'claim'], ['quote', 'claim', 'edit'], ['edit', 'quote', 'claim']];
  const witnesses = {}, coverage = new Set();
  let inputs = 0, histories = 0, retainedHistoricalStates = 0, accepts = 0, refusals = 0;
  for (const count of [0, 1]) for (const initial of limits) {
    if (!allowed(count, initial)) continue;
    for (const target of limits) {
      inputs++;
      for (const trace of schedules) {
        histories++;
        let current = count, limit = initial, quote = undefined, result = null, effects = 0;
        for (const action of trace) {
          if (action === 'edit') limit = target;
          else if (action === 'quote') {
            quote = limit;
            if (!allowed(current, quote)) result = 'refused';
          } else if (result === null) {
            const admitted = allowed(current, cachedLimit ? quote : limit);
            result = admitted ? 'accepted' : 'refused';
            if (admitted) {
              if (!allowed(current, limit)) witnesses.CurrentLimitAtClaim ??= { count, initial, target, trace };
              current++; effects++;
            } else if (effectsOnRefusal) effects++;
          }
          coverage.add(`${action}:${result ?? 'pending'}`);
          if (result === 'refused' && effects) witnesses.RefusalHasNoEffects ??= { count, initial, target, trace };
        }
        if (result === 'accepted') {
          accepts++;
          if (limit !== null && current > limit) retainedHistoricalStates++;
        } else refusals++;
      }
    }
  }
  return { inputs, histories, accepts, refusals, retainedHistoricalStates,
    coverage: [...coverage].sort(), violations: Object.keys(witnesses).sort(), witnesses };
}

test('bounded promo claims enforce the current cap without revoking historical redemptions', t => {
  const result = explorePromoLimit();
  assert.equal(result.inputs, 20);
  assert.equal(result.histories, 60);
  assert.equal(result.accepts, 46);
  assert.equal(result.refusals, 14);
  assert.equal(result.retainedHistoricalStates, 7);
  assert.ok(result.accepts > 0 && result.refusals > 0 && result.retainedHistoricalStates > 0);
  assert.deepEqual(result.violations, []);
  t.diagnostic(JSON.stringify({ model: 'current-promo-limit', ...result }));
});

test('promo controls reject cached limits and committed effects on a refused claim', t => {
  for (const control of ['cachedLimit', 'effectsOnRefusal']) {
    const result = explorePromoLimit({ [control]: true });
    assert.equal(result.inputs, 20);
    assert.equal(result.histories, 60);
    const property = control === 'cachedLimit' ? 'CurrentLimitAtClaim' : 'RefusalHasNoEffects';
    assert.deepEqual(result.violations, [property]);
    assert.deepEqual(result.witnesses[property].trace, ['quote', 'edit', 'claim']);
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});

// One promo editor and one new claimant; fixed valid cap/current count and
// previously validated clock. Nine independent term dimensions use two values
// each, one edited dimension per input. Atomic claim effects are abstract; this
// does not refine Haskell/SQL or prove time/counter/provider behavior.
function explorePromoTerms({ omittedField = null, omitPromoLock = false,
  effectsBeforeCheck = false } = {}) {
  const fields = ['event', 'discountType', 'discountValue', 'currency', 'validFrom',
    'validUntil', 'tierIds', 'minimumPurchase', 'active'];
  const inputsToCheck = fields.flatMap(field => [0, 1].map(value => ({
    field, initial: { [field]: value }, target: { [field]: 1 - value },
  })));
  const same = (a, b) => fields.every(field => a[field] === b[field]);
  const coverage = new Set(), witnesses = {};
  let inputs = 0, states = 0, transitions = 0, retainedHistoricalStates = 0;
  for (const { field, initial, target } of inputsToCheck) {
    inputs++;
    const queue = [{ promo: initial, quote: null, observed: null, lock: null,
      phase: [0, 0], result: null, effects: 0, mismatchAtAdmission: false, trace: [] }];
    const seen = new Set();
    while (queue.length) {
      const current = queue.shift(), { trace, ...identity } = current;
      const key = JSON.stringify(identity);
      if (seen.has(key)) continue;
      seen.add(key);
      if (current.mismatchAtAdmission) witnesses.PromoTermsMatchAtClaim ??= { field, initial, target, trace };
      if (current.result === 'refused' && current.effects) {
        witnesses.RefusalHasNoEffects ??= { field, initial, target, trace };
      }
      if (current.phase[0] === 2 && current.result === 'accepted'
        && !current.mismatchAtAdmission && !same(current.quote, current.promo)) retainedHistoricalStates++;
      const push = (next, label) => {
        transitions++;
        coverage.add(label);
        next.trace = [...trace, label];
        queue.push(next);
      };
      if (current.phase[0] === 0 && current.lock === null) {
        const next = structuredClone(current);
        next.lock = 'editor'; next.phase[0] = 1;
        push(next, 'editor:lock');
      } else if (current.phase[0] === 1) {
        const next = structuredClone(current);
        next.promo = target; next.lock = null; next.phase[0] = 2;
        push(next, 'editor:commit');
      }
      if (current.phase[1] === 0) {
        const next = structuredClone(current);
        next.quote = current.promo; next.phase[1] = 1;
        push(next, 'purchase:quote');
      } else if (current.phase[1] === 1 && (omitPromoLock || current.lock === null)) {
        const next = structuredClone(current);
        if (!omitPromoLock) next.lock = 'buyer';
        next.phase[1] = 2;
        push(next, 'purchase:lock');
      } else if (current.phase[1] === 2) {
        const next = structuredClone(current);
        next.observed = current.promo; next.phase[1] = 3;
        push(next, 'purchase:check');
      } else if (current.phase[1] === 3) {
        const next = structuredClone(current);
        const accepted = fields.every(name => name === omittedField
          || current.quote[name] === current.observed[name]);
        next.effects = accepted || effectsBeforeCheck ? 1 : 0;
        next.mismatchAtAdmission = accepted && !same(current.quote, current.promo);
        next.result = accepted ? 'accepted' : 'refused'; next.phase[1] = 4;
        if (current.lock === 'buyer') next.lock = null;
        push(next, accepted ? 'purchase:accept' : 'purchase:refuse');
      }
    }
    states += seen.size;
  }
  return { inputs, states, transitions, retainedHistoricalStates,
    coverage: [...coverage].sort(), violations: Object.keys(witnesses).sort(), witnesses };
}


test('bounded promo claims bind every quoted term while preserving later historical edits', t => {
  const result = explorePromoTerms();
  assert.equal(result.inputs, 18);
  assert.equal(result.states, 324);
  assert.equal(result.transitions, 324);
  assert.equal(result.retainedHistoricalStates, 18);
  assert.deepEqual(result.violations, []);
  for (const label of ['editor:commit', 'purchase:accept', 'purchase:refuse']) {
    assert.ok(result.coverage.includes(label));
  }
  t.diagnostic(JSON.stringify({ model: 'new-claim-promo-terms', ...result }));
});

test('promo term controls detect every omitted dimension, missing lock and effects on refusal', t => {
  for (const field of ['event', 'discountType', 'discountValue', 'currency', 'validFrom',
    'validUntil', 'tierIds', 'minimumPurchase', 'active']) {
    const result = explorePromoTerms({ omittedField: field });
    assert.deepEqual(result.violations, ['PromoTermsMatchAtClaim']);
    assert.equal(result.witnesses.PromoTermsMatchAtClaim.field, field);
    t.diagnostic(JSON.stringify({ omittedField: field, ...result }));
  }
  for (const [control, property] of [
    ['omitPromoLock', 'PromoTermsMatchAtClaim'], ['effectsBeforeCheck', 'RefusalHasNoEffects'],
  ]) {
    const result = explorePromoTerms({ [control]: true });
    assert.deepEqual(result.violations, [property]);
    t.diagnostic(JSON.stringify({ control, ...result }));
  }
});
