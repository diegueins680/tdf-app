import assert from 'node:assert/strict';
import test from 'node:test';
import { MODEL_ASSUMPTIONS, MODEL_BOUNDS, MUTANTS, availableEvents, explore,
  initialState, replay, runVerification, safetyViolations, step, visibleActions,
} from '../../formal/system/music-track-search-model.mjs';

// These tests exercise the DESIGN model and counterexample checker. They are
// not evidence that React/Haskell refines the model or a mathematical proof.
const verified = runVerification();
function walk(events) {
  let state = initialState();
  for (const event of events) {
    assert(availableEvents(state).some(candidate => Object.keys(candidate).length === Object.keys(event).length
      && Object.entries(candidate).every(([key, value]) => event[key] === value)), `Enabled event ${JSON.stringify(event)}`);
    const next = step(state, event);
    assert.deepEqual(safetyViolations(state, event, next), []);
    state = next;
  }
  return state;
}
const requestAlpha = [{ type: 'edit', text: 'alpha' }, { type: 'submit' }];
const renderedB = [...requestAlpha, { type: 'response', generation: 1, outcome: 'ok' }];
const playingB = [...renderedB, { type: 'click', asset: 'B' },
  { type: 'delivery', generation: 1, asset: 'B', outcome: 'ok' }];

test('bounded design explores adversarial interleavings with explicit limits and nonvacuous coverage', () => {
  assert.equal(verified.result, 'MODEL_CHECKED_WITH_BOUNDS');
  assert.equal(verified.kind, 'FINITE_DESIGN_MODEL');
  assert.equal(verified.implementationRefinement, 'UNPROVEN');
  assert.deepEqual(verified.design.counterexamples, []);
  assert.equal(verified.design.safety, 'MODEL_CHECKED_WITH_BOUNDS');
  assert(verified.design.states > 9000);
  assert(verified.design.transitions > 50000);
  assert(verified.design.frontierStatesAtEventBound > 0, 'Report explored frontier instead of claiming unbounded completion');
  for (const [scenario, count] of Object.entries(verified.design.coverage)) assert(count > 0, scenario);
  assert.equal(MODEL_BOUNDS.searchGenerations, 2);
  assert.equal(MODEL_BOUNDS.sessionGenerations, 2);
  assert.equal(MODEL_BOUNDS.maxEvents, 12);
  assert(MODEL_ASSUMPTIONS.some(text => text.includes('reauthorized') || text.includes('rechecked')));
});

test('draft text hides old committed actions until a new query commits and resolves', () => {
  const rendered = walk(renderedB);
  assert.deepEqual(visibleActions(rendered), ['B']);
  const edited = walk([...renderedB, { type: 'edit', text: 'beta' }]);
  assert.equal(edited.committed.query, 'alpha');
  assert.equal(edited.rendered.query, 'alpha');
  assert.deepEqual(visibleActions(edited), []);
  assert(!availableEvents(edited).some(event => event.type === 'click'));
  const pending = walk([...renderedB, { type: 'edit', text: 'beta' }, { type: 'submit' }]);
  assert.equal(pending.committed.query, 'beta');
  assert.deepEqual(visibleActions(pending), []);
});

test('obsolete query and page responses cannot replace current results', () => {
  const queryRace = walk([...requestAlpha, { type: 'edit', text: 'beta' }, { type: 'submit' },
    { type: 'response', generation: 2, outcome: 'ok' },
    { type: 'response', generation: 1, outcome: 'ok' }]);
  assert.equal(queryRace.rendered.query, 'beta');
  assert.deepEqual(visibleActions(queryRace), ['A']);
  const pageRace = walk([...requestAlpha, { type: 'page', page: 1 },
    { type: 'response', generation: 2, outcome: 'ok' },
    { type: 'response', generation: 1, outcome: 'ok' }]);
  assert.equal(pageRace.rendered.page, 1);
  assert.deepEqual(visibleActions(pageRace), ['A']);
});

test('an explicit matching-B click selects B rather than release first track A', () => {
  const rendered = walk(renderedB);
  assert.equal(rendered.selected, null);
  assert.equal(rendered.playback, 'idle');
  const playing = walk(playingB);
  assert.equal(playing.intent, 'B');
  assert.equal(playing.selected, 'B');
  assert.equal(playing.playback, 'playing');
  assert.equal(playing.appliedGeneration, playing.sessionGeneration);
});

test('arrival and navigation preserve an existing selection and never autoplay a new hit', () => {
  const state = walk([...playingB, { type: 'edit', text: 'beta' }, { type: 'submit' },
    { type: 'navigate', route: 'other' }, { type: 'response', generation: 2, outcome: 'ok' },
    { type: 'navigate', route: 'music' }]);
  assert.deepEqual(visibleActions(state), ['A']);
  assert.equal(state.selected, 'B');
  assert.equal(state.intent, 'B');
  assert.equal(state.playback, 'playing');
  assert.equal(state.sessionGeneration, 1);
});

test('revocation between hit/click or click/delivery blocks a new start without recording fallback', () => {
  for (const revokeBeforeClick of [true, false]) {
    const events = [...renderedB];
    if (revokeBeforeClick) events.push({ type: 'revoke', asset: 'B' });
    events.push({ type: 'click', asset: 'B' });
    if (!revokeBeforeClick) events.push({ type: 'revoke', asset: 'B' });
    events.push({ type: 'delivery', generation: 1, asset: 'B', outcome: 'ok' });
    const state = walk(events);
    assert.equal(state.playback, 'blocked');
    assert.equal(state.selected, 'B');
    assert.equal(state.intent, 'B');
    assert.equal(state.authorized.A, true, 'Other recording remains available but cannot replace user intent');
  }
});

test('an obsolete session delivery cannot replace a newer click or satisfy its preparation', () => {
  const state = walk([...renderedB, { type: 'click', asset: 'B' },
    { type: 'click', asset: 'B' },
    { type: 'delivery', generation: 1, asset: 'B', outcome: 'ok' }]);
  assert.equal(state.playback, 'resolving');
  assert.equal(state.sessionGeneration, 2);
  assert.equal(state.appliedGeneration, 0);
  assert.deepEqual(state.deliveries, [{ generation: 2, asset: 'B' }]);
});

test('failed search/delivery outcomes resolve explicitly without changing recording identity', () => {
  const failedSearch = walk([...requestAlpha, { type: 'response', generation: 1, outcome: 'error' }]);
  assert.deepEqual(visibleActions(failedSearch), []);
  assert.equal(failedSearch.rendered.outcome, 'error');
  assert.equal(failedSearch.requests.length, 0);
  const failedDelivery = walk([...renderedB, { type: 'click', asset: 'B' },
    { type: 'delivery', generation: 1, asset: 'B', outcome: 'error' }]);
  assert.equal(failedDelivery.playback, 'error');
  assert.equal(failedDelivery.selected, 'B');
});

test('every mutant produces a replayable concrete safety counterexample', () => {
  const expected = {
    acceptStaleResults: 'SEARCH-STALE-RESPONSE',
    firstTrackSubstitution: 'PLAYER-EXACT-RENDERED-HIT',
    autoplayOnResponse: 'PLAYER-NO-RESPONSE-AUTOPLAY',
    acceptStaleSession: 'PLAYER-STALE-SESSION',
    deniedRecordingFallback: 'MEDIA-NEW-PLAY-AUTHORIZED',
    exposeDraftActions: 'SEARCH-DRAFT-ACTIONS',
  };
  assert.deepEqual(Object.keys(verified.negativeControls), Object.keys(MUTANTS));
  for (const [name, property] of Object.entries(expected)) {
    const result = verified.negativeControls[name];
    assert.equal(result.safety, 'FAILED', name);
    assert.equal(result.expectedCounterexampleDetected, true, name);
    const witness = result.counterexamples.find(counterexample => counterexample.property === property);
    assert(witness, `${name} should violate ${property}`);
    assert(witness.events.length <= MODEL_BOUNDS.maxEvents);
    const replayed = replay(witness.events, { [name]: true });
    assert(replayed.violations.some(item => item.property === property), name);
    assert.deepEqual(replayed.state, witness.after, `${name} trace reaches recorded failing state`);
  }
});

test('conditional finite completion is separate from unconditional liveness and implementation proof', () => {
  const result = verified.design.liveness;
  assert.equal(result.status, 'PARTIAL');
  assert.equal(result.conditionalTerminalReachability, 'MODEL_CHECKED_WITH_BOUNDS');
  assert.equal(result.checkedStates, verified.design.states);
  assert(result.maxDrainSteps > 0 && result.maxDrainSteps <= 4);
  assert(result.scope.includes('No unconditional liveness'));
  assert(verified.implementationBoundary.includes('separate TESTED/PARTIAL'));
  assert.throws(() => explore({ maxEvents: MODEL_BOUNDS.maxEvents + 1 }), /bound/);
  assert.throws(() => explore({ mutants: { undeclaredMutation: true } }), /declared/);
  assert.throws(() => replay([{ type: 'click', asset: 'B' }]), /not enabled/);
});
