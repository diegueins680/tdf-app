#!/usr/bin/env node
import assert from 'node:assert/strict';
import test from 'node:test';

test('exports discoverImprovementIdea as a function', async () => {
  const { discoverImprovementIdea } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof discoverImprovementIdea, 'function');
});

test('exports main as a function', async () => {
  const { main } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof main, 'function');
});

test('exports validateLoopConfig as a function', async () => {
  const { validateLoopConfig } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof validateLoopConfig, 'function');
});

test('exports resolveLoopPushBranch as a function', async () => {
  const { resolveLoopPushBranch } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof resolveLoopPushBranch, 'function');
});

test('exports syncAndPollLatestRemoteCi as a function', async () => {
  const { syncAndPollLatestRemoteCi } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof syncAndPollLatestRemoteCi, 'function');
});

test('exports waitForGreenCi as a function', async () => {
  const { waitForGreenCi } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof waitForGreenCi, 'function');
});

test('exports reconcileNonMainBranchesOntoMain as a function', async () => {
  const { reconcileNonMainBranchesOntoMain } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof reconcileNonMainBranchesOntoMain, 'function');
});

test('exports syncSubmodulesRecursively as a function', async () => {
  const { syncSubmodulesRecursively } = await import('./continuous-improvement-loop.mjs');
  assert.strictEqual(typeof syncSubmodulesRecursively, 'function');
});

test('validateLoopConfig returns object with pushBranch for valid config', async () => {
  const { validateLoopConfig } = await import('./continuous-improvement-loop.mjs');
  const result = await validateLoopConfig('/Users/diegosaa/GitHub/tdf-app', { model: 'gpt-4o', task: 'test', dryRun: true });
  assert.strictEqual(typeof result, 'object');
  assert.strictEqual(typeof result.pushBranch, 'string');
});

test('validateLoopConfig throws when targeting main without override', async () => {
  const { validateLoopConfig } = await import('./continuous-improvement-loop.mjs');
  await assert.rejects(async () => validateLoopConfig('/Users/diegosaa/GitHub/tdf-app', { model: 'gpt-4o', task: 'test', dryRun: false, allowPushToMain: false }), /main/);
});

test('discoverImprovementIdea returns an object with title, markdown, and lane', async () => {
  const { discoverImprovementIdea } = await import('./continuous-improvement-loop.mjs');
  const idea = await discoverImprovementIdea('/Users/diegosaa/GitHub/tdf-app', { persistState: false });
  assert.strictEqual(typeof idea, 'object');
  assert.strictEqual(typeof idea.title, 'string');
  assert.ok(idea.title.length > 0, 'title should be non-empty');
  assert.strictEqual(typeof idea.markdown, 'string');
  assert.ok(idea.markdown.length > 0, 'markdown should be non-empty');
  assert.strictEqual(typeof idea.lane, 'string');
  assert.ok(idea.lane.length > 0, 'lane should be non-empty');
});

test('main dry-run throws without push branch override', async () => {
  const { main } = await import('./continuous-improvement-loop.mjs');
  // When on main with no pushBranch configured and allowPushToMain false, main should reject
  await assert.rejects(async () =>
    main('/Users/diegosaa/GitHub/tdf-app', {
      model: 'gpt-4o',
      task: 'test',
      dryRun: true,
      maxIterations: 1,
      stopOnNoChanges: true,
      iterationDelaySeconds: 0,
    }),
    /main/
  );
});

test('discoverImprovementIdea lane is one of the expected values', async () => {
  const { discoverImprovementIdea } = await import('./continuous-improvement-loop.mjs');
  const idea = await discoverImprovementIdea('/Users/diegosaa/GitHub/tdf-app', { persistState: false });
  const expectedLanes = ['login-proof', 'store-publish', 'continuous-improvement', 'developer-experience', 'infrastructure', 'logical', 'ux', 'ui', 'backend', 'formal'];
  assert.ok(expectedLanes.includes(idea.lane), `lane "${idea.lane}" should be one of ${expectedLanes.join(', ')}`);
});
