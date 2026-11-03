// Finite DESIGN model. No implementation source is parsed and no refinement
// theorem follows from this executable abstraction or its ordinary unit tests.
import { pathToFileURL } from 'node:url';

export const MODEL_BOUNDS = Object.freeze({
  queries: ['alpha', 'beta'], recordings: ['A', 'B'], pages: [0, 1],
  hitsPerPage: 1, searchGenerations: 2, sessionGenerations: 2, maxEvents: 12,
});
export const MODEL_ASSUMPTIONS = Object.freeze([
  'One application context has one persistent authoritative playback engine; tabs and native decoders are outside this model.',
  'A and B are distinct recordings, each abstracted to one delivery asset. Multiple renditions, real identifiers, transport signatures and catalog/query text normalization are outside this finite abstraction.',
  'Backend retrieval returns only assets authorized at retrieval time in the exact recording/release context; SQL and metadata correctness are trusted here and tested separately.',
  'Rendered actions are derived from the current committed query/page/generation and are hidden when draft text differs or the search surface is not active.',
  'Each explicit click names the exact rendered recording. A and B share a release whose first track is A; selecting a release first track is not equivalent to selecting its matching hit.',
  'A source resolution response is bound to the selected recording and playback generation. Authority is rechecked before a new playback start; denial cannot substitute another recording.',
  'Existing playback may continue after revocation until its separately defined credential/cache enforcement bound. This model checks new starts, not immediate decoder interruption or credential expiry.',
  'Search and delivery responses can arrive in either order or fail; navigation and draft edits can interleave. User commands are bounded, and no fairness of the network or user is assumed by safety exploration.',
  'Conditional completion checks stop new user commands and assume every remaining search/delivery response is eventually supplied. They establish bounded terminal reachability, not unconditional temporal liveness.',
]);

export const MUTANTS = Object.freeze({
  acceptStaleResults: 'Accept an obsolete query/page response',
  firstTrackSubstitution: 'Select release first track A instead of rendered hit B',
  autoplayOnResponse: 'Start playback when search results arrive without a click',
  acceptStaleSession: 'Apply an obsolete playback-session delivery',
  deniedRecordingFallback: 'Replace a denied selected recording with another recording',
  exposeDraftActions: 'Keep actions enabled after draft text diverges from committed text',
});
const copy = state => structuredClone(state);
const encode = value => JSON.stringify(value);
const sameEvent = (left, right) => Object.keys(left).length === Object.keys(right).length
  && Object.entries(left).every(([key, value]) => right[key] === value);
const hitFor = (query, page) => query === 'alpha' ? ['B', 'A'][page] : ['A', 'B'][page];
const frame = state => encode([state.selected, state.intent, state.sessionGeneration,
  state.playback, state.appliedGeneration, state.deliveries]);

export function initialState() {
  return { draft: '', committed: null, rendered: null, searchGeneration: 0,
    requests: [], route: 'music', authorized: { A: true, B: true },
    selected: null, intent: null, sessionGeneration: 0, appliedGeneration: 0,
    playback: 'idle', deliveries: [] };
}
export function visibleActions(state, mutants = {}) {
  if (state.route !== 'music' || !state.rendered || !state.committed) return [];
  const current = state.rendered.generation === state.committed.generation
    && state.rendered.query === state.committed.query
    && state.rendered.page === state.committed.page;
  if (!current || (!mutants.exposeDraftActions && state.draft !== state.committed.query)) return [];
  return state.rendered.hits;
}
export function availableEvents(state, mutants = {}) {
  const events = [];
  for (const text of ['', ...MODEL_BOUNDS.queries]) {
    if (state.draft !== text) events.push({ type: 'edit', text });
  }
  if (state.searchGeneration < MODEL_BOUNDS.searchGenerations && state.draft) {
    events.push({ type: 'submit' });
    if (state.committed && state.draft === state.committed.query) {
      for (const page of MODEL_BOUNDS.pages) {
        if (page !== state.committed.page) events.push({ type: 'page', page });
      }
    }
  }
  for (const request of state.requests) {
    events.push({ type: 'response', generation: request.generation, outcome: 'ok' });
    events.push({ type: 'response', generation: request.generation, outcome: 'error' });
  }
  if (state.sessionGeneration < MODEL_BOUNDS.sessionGenerations) {
    for (const asset of visibleActions(state, mutants)) events.push({ type: 'click', asset });
  }
  for (const request of state.deliveries) {
    events.push({ type: 'delivery', ...request, outcome: 'ok' });
    events.push({ type: 'delivery', ...request, outcome: 'error' });
  }
  for (const asset of MODEL_BOUNDS.recordings) {
    if (state.authorized[asset]) events.push({ type: 'revoke', asset });
  }
  events.push({ type: 'navigate', route: state.route === 'music' ? 'other' : 'music' });
  return events;
}
export function step(state, event, mutants = {}) {
  const next = copy(state);
  switch (event.type) {
    case 'edit': next.draft = event.text; break;
    case 'submit':
    case 'page': {
      const request = { generation: state.searchGeneration + 1, query: state.draft,
        page: event.type === 'page' ? event.page : 0 };
      next.searchGeneration = request.generation;
      next.committed = request;
      next.rendered = null;
      next.requests.push(request);
      break;
    }
    case 'response': {
      const request = state.requests.find(item => item.generation === event.generation);
      if (!request) throw new Error('Response has no pending request');
      next.requests = state.requests.filter(item => item.generation !== event.generation);
      const asset = hitFor(request.query, request.page);
      const hits = event.outcome === 'ok' && state.authorized[asset] ? [asset] : [];
      if (state.committed.generation === request.generation || mutants.acceptStaleResults) {
        next.rendered = { ...request, hits, outcome: event.outcome };
      }
      if (mutants.autoplayOnResponse && hits.length) {
        next.selected = hits[0];
        next.playback = 'playing';
        next.appliedGeneration = state.sessionGeneration;
      }
      break;
    }
    case 'click': {
      if (!visibleActions(state, mutants).includes(event.asset)) throw new Error('Action is not rendered/current');
      next.intent = event.asset;
      next.selected = mutants.firstTrackSubstitution ? 'A' : event.asset;
      next.sessionGeneration++;
      next.playback = 'resolving';
      next.deliveries.push({ generation: next.sessionGeneration, asset: next.selected });
      break;
    }
    case 'delivery': {
      const request = state.deliveries.find(item => item.generation === event.generation);
      if (!request || request.asset !== event.asset) throw new Error('Delivery has no bound pending request');
      next.deliveries = state.deliveries.filter(item => item.generation !== event.generation);
      if (event.generation !== state.sessionGeneration && !mutants.acceptStaleSession) break;
      next.appliedGeneration = event.generation;
      if (event.outcome === 'error') { next.playback = 'error'; break; }
      if (state.authorized[request.asset]) {
        next.selected = request.asset;
        next.playback = 'playing';
      } else {
        const other = MODEL_BOUNDS.recordings.find(asset => asset !== request.asset && state.authorized[asset]);
        if (mutants.deniedRecordingFallback && other) {
          next.selected = other;
          next.playback = 'playing';
        } else next.playback = 'blocked';
      }
      break;
    }
    case 'revoke': next.authorized[event.asset] = false; break;
    case 'navigate': next.route = event.route; break;
    default: throw new Error(`Unknown model event ${event.type}`);
  }
  return next;
}
export function safetyViolations(before, event, after, mutants = {}) {
  const failed = [];
  if (after.rendered && (!after.committed
      || after.rendered.generation !== after.committed.generation
      || after.rendered.query !== after.committed.query
      || after.rendered.page !== after.committed.page)) failed.push('SEARCH-STALE-RESPONSE');
  if (visibleActions(after, mutants).length && after.draft !== after.committed.query) {
    failed.push('SEARCH-DRAFT-ACTIONS');
  }
  if (after.selected !== after.intent) failed.push('PLAYER-SELECTION-IDENTITY');
  if (event.type === 'click' && after.selected !== event.asset) failed.push('PLAYER-EXACT-RENDERED-HIT');
  if (event.type === 'response' && frame(before) !== frame(after)) failed.push('PLAYER-NO-RESPONSE-AUTOPLAY');
  if (event.type === 'navigate' && frame(before) !== frame(after)) failed.push('PLAYER-NAVIGATION-CONTINUITY');
  if (after.playback === 'playing' && after.appliedGeneration !== after.sessionGeneration) {
    failed.push('PLAYER-STALE-SESSION');
  }
  const starts = after.playback === 'playing' && (before.playback !== 'playing'
    || after.selected !== before.selected || after.appliedGeneration !== before.appliedGeneration);
  if (starts && (event.type !== 'delivery' || !before.authorized[event.asset]
      || event.generation !== before.sessionGeneration || after.selected !== before.intent)) {
    failed.push('MEDIA-NEW-PLAY-AUTHORIZED');
  }
  return failed;
}

export function replay(events, mutants = {}) {
  let state = initialState();
  const violations = [];
  for (const [index, event] of events.entries()) {
    if (!availableEvents(state, mutants).some(candidate => sameEvent(candidate, event))) {
      throw new Error(`Counterexample event ${index} is not enabled`);
    }
    const next = step(state, event, mutants);
    violations.push(...safetyViolations(state, event, next, mutants).map(property => ({ index, property })));
    state = next;
  }
  return { state, violations };
}
function drain(state) {
  let next = state, steps = 0;
  while (next.requests.length || next.deliveries.length) {
    const event = next.requests.length
      ? { type: 'response', generation: next.requests[0].generation, outcome: 'ok' }
      : { type: 'delivery', ...next.deliveries[0], outcome: 'ok' };
    next = step(next, event);
    steps++;
    if (steps > MODEL_BOUNDS.searchGenerations + MODEL_BOUNDS.sessionGenerations) {
      throw new Error('Conditional drain exceeds finite pending-operation bound');
    }
  }
  const searchResolved = !next.committed || next.rendered?.generation === next.committed.generation;
  return { terminal: !next.requests.length && !next.deliveries.length && searchResolved
    && ['idle', 'playing', 'blocked', 'error'].includes(next.playback), steps };
}

export function explore({ mutants = {}, maxEvents = MODEL_BOUNDS.maxEvents,
  stopAtFirstViolation = false } = {}) {
  if (!Number.isInteger(maxEvents) || maxEvents < 0 || maxEvents > MODEL_BOUNDS.maxEvents) {
    throw new Error('Event bound must be within the declared finite model');
  }
  for (const [name, enabled] of Object.entries(mutants)) {
    if (!Object.hasOwn(MUTANTS, name) || enabled !== true) {
      throw new Error('Only declared, explicitly enabled mutants are supported');
    }
  }
  const initial = initialState();
  const queue = [{ state: initial, depth: 0, parent: -1, event: null }];
  const seen = new Set([encode(initial)]);
  const failures = new Map();
  const coverage = { draftHidesActions: 0, staleSearchArrival: 0, clickB: 0,
    staleSessionArrival: 0, revokedBetweenHitAndClick: 0, navigationWhilePlaying: 0,
    pageRequest: 0, searchFailure: 0, deliveryFailure: 0 };
  let transitions = 0, frontierStates = 0, drainedStates = 0, maxDrainSteps = 0;
  function trace(index, event) {
    const events = [event];
    while (queue[index].parent !== -1) {
      events.push(queue[index].event);
      index = queue[index].parent;
    }
    return events.reverse();
  }
  for (let index = 0; index < queue.length; index++) {
    const { state, depth } = queue[index];
    if (!Object.keys(mutants).length) {
      const completion = drain(state);
      if (!completion.terminal) throw new Error('Conditional completion failed');
      drainedStates++;
      maxDrainSteps = Math.max(maxDrainSteps, completion.steps);
    }
    if (depth === maxEvents) { frontierStates++; continue; }
    for (const event of availableEvents(state, mutants)) {
      const next = step(state, event, mutants);
      transitions++;
      if (event.type === 'edit' && state.rendered?.hits.length && next.committed
          && next.draft !== next.committed.query) coverage.draftHidesActions++;
      if (event.type === 'response' && event.generation !== state.committed.generation) coverage.staleSearchArrival++;
      if (event.type === 'click' && event.asset === 'B') coverage.clickB++;
      if (event.type === 'delivery' && event.generation !== state.sessionGeneration) coverage.staleSessionArrival++;
      if (event.type === 'click' && !state.authorized[event.asset]) coverage.revokedBetweenHitAndClick++;
      if (event.type === 'navigate' && state.playback === 'playing') coverage.navigationWhilePlaying++;
      if (event.type === 'page') coverage.pageRequest++;
      if (event.type === 'response' && event.outcome === 'error') coverage.searchFailure++;
      if (event.type === 'delivery' && event.outcome === 'error') coverage.deliveryFailure++;
      for (const property of safetyViolations(state, event, next, mutants)) {
        if (!failures.has(property)) failures.set(property, {
          property, events: trace(index, event), before: state, after: next,
        });
      }
      if (failures.size && stopAtFirstViolation) return {
        states: seen.size, transitions, exploredEventBound: maxEvents,
        counterexamples: [...failures.values()], safety: 'FAILED', coverage,
      };
      const key = encode(next);
      if (!seen.has(key)) {
        seen.add(key);
        queue.push({ state: next, depth: depth + 1, parent: index, event });
      }
    }
  }
  return { states: seen.size, transitions, exploredEventBound: maxEvents,
    frontierStatesAtEventBound: frontierStates, coverage,
    counterexamples: [...failures.values()],
    safety: failures.size ? 'FAILED' : 'MODEL_CHECKED_WITH_BOUNDS',
    liveness: { status: 'PARTIAL',
      conditionalTerminalReachability: 'MODEL_CHECKED_WITH_BOUNDS',
      checkedStates: drainedStates, maxDrainSteps,
      scope: 'From each design state reached within the twelve-event safety bound, with no new user commands and every pending response eventually supplied, at most four additional response events reach resolved search and PLAYING/BLOCKED/ERROR/IDLE playback. No unconditional liveness or fairness proof.' },
  };
}
export function runVerification() {
  const design = explore();
  const negativeControls = Object.fromEntries(Object.keys(MUTANTS).map(name => {
    const result = explore({ mutants: { [name]: true }, stopAtFirstViolation: true });
    return [name, { ...result, expectedCounterexampleDetected: result.counterexamples.length > 0 }];
  }));
  return { schemaVersion: 1, kind: 'FINITE_DESIGN_MODEL', bounds: MODEL_BOUNDS,
    assumptions: MODEL_ASSUMPTIONS, design, negativeControls,
    implementationRefinement: 'UNPROVEN',
    implementationBoundary: 'SQL, React and real browser/native tests are separate TESTED/PARTIAL evidence; this independent model does not extract implementation formulas or prove conformance.',
    result: design.safety === 'MODEL_CHECKED_WITH_BOUNDS'
      && Object.values(negativeControls).every(result => result.expectedCounterexampleDetected)
      ? 'MODEL_CHECKED_WITH_BOUNDS' : 'FAILED' };
}
if (process.argv[1] && pathToFileURL(process.argv[1]).href === import.meta.url) {
  const report = runVerification();
  process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
  if (report.result === 'FAILED') process.exitCode = 1;
}
