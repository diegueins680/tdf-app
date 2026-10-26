import assert from 'node:assert/strict';
import test from 'node:test';

// Explicit-state design abstraction, not an extraction of React/browser code.
// A -> B -> A exercises identity reuse with distinct selection generations.
const selections = [null, 'A', 'B', 'A'];

function explore(initial, successors, invariant) {
  const queue = [{ state: initial, trace: [] }];
  const visited = new Set();
  let transitions = 0;
  for (let cursor = 0; cursor < queue.length; cursor += 1) {
    const { state, trace } = queue[cursor];
    const key = JSON.stringify(state);
    if (visited.has(key)) continue;
    visited.add(key);
    const violation = invariant(state);
    if (violation) return { states: visited.size, transitions, violation, counterexample: trace };
    for (const [event, next] of successors(state)) {
      transitions += 1;
      queue.push({ state: next, trace: [...trace, event] });
    }
  }
  return { states: visited.size, transitions, violation: null };
}

function selectionModel({ staleSource = false, stalePlay = false, pauseFence = false } = {}) {
  const initial = { generation: 0, source: 0, playing: 0, epoch: 0, commands: 0,
    intent: false, status: 'idle', authorization: [], plays: [], engine: 1 };
  const successors = state => {
    const next = [];
    if (state.generation < 3) {
      const generation = state.generation + 1;
      next.push([`select ${selections[generation]} generation ${generation}`, {
        ...state, generation, source: 0, playing: 0, epoch: state.epoch + 1,
        intent: true, status: 'resolving', authorization: [...state.authorization, generation],
      }]);
    }
    for (const generation of state.authorization) {
      const base = { ...state, authorization: state.authorization.filter(item => item !== generation) };
      if (generation !== state.generation && !staleSource) {
        next.push([`discard stale authorization ${generation}`, base]);
        continue;
      }
      next.push([`authorize ${generation}`, { ...base, source: generation, playing: 0,
        status: state.intent ? 'preparing' : 'paused',
        plays: state.intent ? [...state.plays, [generation, state.epoch]] : state.plays,
      }]);
      next.push([`deny authorization ${generation}`, { ...base, source: 0,
        playing: 0, intent: false, status: 'unavailable' }]);
    }
    for (const [index, [generation, epoch]] of state.plays.entries()) {
      const base = { ...state, plays: state.plays.filter((_item, i) => i !== index) };
      const current = generation === state.generation && epoch === state.epoch;
      if (!current && !stalePlay) {
        next.push([`discard stale play ${generation}/${epoch}`, base]);
        continue;
      }
      next.push([`play resolves ${generation}/${epoch}`, { ...base, playing: generation, status: 'playing' }]);
      // Autoplay policy is an explicit user-action-required outcome. Failure
      // retains the source so a later authorized gesture can retry it.
      for (const status of ['blocked', 'error']) next.push([`play ${status} ${generation}/${epoch}`,
        { ...base, playing: 0, intent: false, status }]);
    }
    if (state.generation && state.commands < 2) {
      if (state.intent) next.push(['pause', { ...state, intent: false, playing: 0, status: 'paused',
        epoch: state.epoch + (pauseFence ? 0 : 1), commands: state.commands + 1 }]);
      if (!state.intent && state.source === state.generation) {
        const epoch = state.epoch + 1;
        next.push(['user play', { ...state, intent: true, status: 'preparing', epoch,
          commands: state.commands + 1, plays: [...state.plays, [state.generation, epoch]] }]);
      }
    }
    // Route transitions retain the same authoritative engine and session.
    next.push(['navigate', { ...state }]);
    return next;
  };
  return explore(initial, successors, state => {
    if (state.engine !== 1) return 'PLAYER-001';
    if (state.source && state.source !== state.generation) return 'PLAYER-002/005';
    if (state.playing && (state.playing !== state.generation || state.source !== state.playing)) return 'PLAYER-002/005';
    if (state.status === 'playing' && (!state.intent || !state.playing)) return 'PLAYER-003';
    return null;
  });
}

function recoveryModel({ ignorePause = false, ignoreSelection = false, noBound = false,
  duplicateTimer = false } = {}) {
  const initial = { generation: 1, timers: [], retries: 0, intent: true, status: 'playing' };
  return explore(initial, state => {
    const next = [];
    if (state.status === 'playing' || state.status === 'recovering') {
      if (state.timers.length === 0 || duplicateTimer) {
        if (state.retries < (noBound ? 3 : 2)) next.push(['transient media error', {
          ...state, retries: state.retries + 1, status: 'recovering',
          timers: [...state.timers, state.generation],
        }]);
        else next.push(['retry limit reached', { ...state, intent: false, status: 'error', timers: [] }]);
      }
    }
    if (state.intent) next.push(['pause', { ...state, intent: false, status: 'paused',
      timers: ignorePause ? state.timers : [] }]);
    if (state.generation === 1) next.push(['select next asset', { ...state, generation: 2,
      retries: 0, status: 'playing', intent: true, timers: ignoreSelection ? state.timers : [] }]);
    for (const [index, generation] of state.timers.entries()) {
      const base = { ...state, timers: state.timers.filter((_value, i) => i !== index) };
      if (generation !== state.generation || !state.intent) next.push(['discard obsolete timer', base]);
      else {
        next.push(['retry succeeds', { ...base, status: 'playing' }]);
        next.push(['retry fails', { ...base, status: 'recovering' }]);
      }
    }
    next.push(['navigate', { ...state }]);
    return next;
  }, state => {
    if (state.retries > 2) return 'LIVE-004 retry bound';
    if (state.timers.length > 1) return 'single recovery timer';
    if (state.timers.some(generation => generation !== state.generation)) return 'PLAYER-005 timer generation';
    if (!state.intent && state.timers.length) return 'pause cancels retry';
    return null;
  });
}

test('bounded selection interleavings preserve source, play and pause fences', () => {
  const result = selectionModel();
  assert.equal(result.violation, null, JSON.stringify(result));
  assert.ok(result.states > 100);
  console.log('selection model', JSON.stringify(result));
});

test('selection guard-removal controls produce concrete counterexamples', () => {
  for (const mutation of ['staleSource', 'stalePlay', 'pauseFence']) {
    const result = selectionModel({ [mutation]: true });
    assert.ok(result.violation, `${mutation} must fail`);
    console.log('selection negative control', mutation, JSON.stringify(result));
  }
});

test('bounded recovery interleavings cancel obsolete work and bound retries', () => {
  const result = recoveryModel();
  assert.equal(result.violation, null, JSON.stringify(result));
  assert.ok(result.states > 10);
  console.log('recovery model', JSON.stringify(result));
});

test('finite recovery outcomes terminate when each scheduled attempt completes', () => {
  // Conditional progress check: no new selection, pause, navigation stutter or
  // new failure after success; each scheduled retry gets one eventual outcome.
  // Exhaust every success/failure branch, including persistent delivery failure.
  const queue = [{ attempts: 0, pending: true, outcome: null, trace: [] }];
  const terminal = [];
  while (queue.length) {
    const state = queue.shift();
    if (!state.pending) { terminal.push(state); continue; }
    const attempts = state.attempts + 1;
    queue.push({ attempts, pending: false, outcome: 'playing', trace: [...state.trace, 'success'] });
    queue.push({ attempts, pending: attempts < 2, outcome: attempts < 2 ? null : 'error',
      trace: [...state.trace, 'failure'] });
  }
  assert.deepEqual(terminal.map(state => [state.trace, state.outcome]), [
    [['success'], 'playing'], [['failure', 'success'], 'playing'], [['failure', 'failure'], 'error'],
  ]);
  assert.ok(terminal.every(state => state.attempts <= 2));
});

test('recovery guard-removal controls produce concrete counterexamples', () => {
  for (const mutation of ['ignorePause', 'ignoreSelection', 'noBound', 'duplicateTimer']) {
    const result = recoveryModel({ [mutation]: true });
    assert.ok(result.violation, `${mutation} must fail`);
    console.log('recovery negative control', mutation, JSON.stringify(result));
  }
});
