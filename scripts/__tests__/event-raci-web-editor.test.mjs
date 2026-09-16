import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const root = new URL('../../', import.meta.url);
const read = path => readFileSync(new URL(path, root), 'utf8');

test('editor formal guard mutations stay mandatory with named invariant failures', () => {
  const runner = read('scripts/verify-event-operations-formal.sh');
  assert.match(runner, /run_tlc RaciWebEditor.tla RaciWebEditor.cfg/);
  for (const [mutation, invariant] of Object.entries({ Consent: 'ExplicitConfirmation', Context: 'CurrentEditor',
    Flight: 'OneFlight', Retry: 'SameRetry', Receipt: 'ValidatedSuccess' })) {
    assert(runner.includes(`expect_counterexample RaciWebEditor${mutation}.cfg ${invariant}`));
  }
  const workflow = read('.github/workflows/event-operations-formal.yml');
  assert.equal(workflow.split('tdf-hq-ui/src/components/events/EventRaciEditor.tsx').length - 1, 2);
  assert.match(workflow, /run: node --test scripts\/__tests__\/event-raci-web-editor.test.mjs/);
});

test('browser editor uses isolated API interception and retained task regression journeys', () => {
  const config = read('playwright.event-raci.config.mjs');
  assert.match(config, /isolatedBrowserConfig/);
  assert.match(config, /event-raci-editor.spec.mjs/);
  assert.match(config, /event-task-view.spec.mjs/);
  const fixture = read('e2e/web/event-raci-editor.spec.mjs');
  assert.match(fixture, /localApiFixturePattern\(baseURL\)/);
  assert.match(fixture, /url.origin !== origin/);
  assert.match(fixture, /expect\(writes\[1\]\).toEqual\(writes\[0\]\)/);
  assert.match(fixture, /status: 404/);
});
