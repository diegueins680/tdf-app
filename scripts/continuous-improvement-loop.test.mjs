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

test('validateLoopConfig returns true for minimal valid config', async () => {
  const { validateLoopConfig } = await import('./continuous-improvement-loop.mjs');
  const result = await validateLoopConfig({ model: 'gpt-4o', task: 'test', cwd: '/Users/diegosaa/GitHub/tdf-app' });
  assert.strictEqual(result, true);
});

test('validateLoopConfig throws for missing model', async () => {
  const { validateLoopConfig } = await import('./continuous-improvement-loop.mjs');
  await assert.rejects(async () => validateLoopConfig({ task: 'test', cwd: '/Users/diegosaa/GitHub/tdf-app' }), /model/);
});
