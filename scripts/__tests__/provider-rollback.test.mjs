import test from 'node:test';
import assert from 'node:assert/strict';
import { parseArgs, recoverReleaseMachines, rollbackCompatibility, selectRecoveryTarget, withCompatibleRollback } from '../production-release.mjs';

const legacy = '2f01b20b0c2a2e2088570c3dc5deba6197266452';
const modern = '5c11577a5d31f079b3e070a7810a6b04a48d99f4';
const floor = 'c53b33e7ef868fb7b64f876199ed66be0f617efc';
const context = { migrations: [{ id: '2026-09-18_provider_subject_identity' }] };
const ancestry = async (required, candidate) => {
  assert.equal(required, floor);
  return candidate === modern || candidate === floor;
};

test('first provider rollout refuses the actual prior binary before any deploy callback', async () => {
  let writes = 0;
  assert.deepEqual(await rollbackCompatibility(context, legacy, ancestry), {
    sha: legacy, requiredCommit: floor, compatible: false,
  });
  await assert.rejects(withCompatibleRollback(context, legacy, () => { writes++; }, ancestry), /recover forward/);
  assert.equal(writes, 0);
});

test('compatible rollback preserves the deploy result and surfaces its failure', async () => {
  assert.equal(await withCompatibleRollback(context, modern, async () => 'verified receipt', ancestry), 'verified receipt');
  await assert.rejects(withCompatibleRollback(context, floor, async () => { throw new Error('canary unavailable'); }, ancestry), /canary unavailable/);
});

test('missing Git history and invalid source identifiers fail closed', async () => {
  const deploy = () => assert.fail('must not mutate machines');
  await assert.rejects(withCompatibleRollback(context, modern, deploy, async () => { throw new Error('missing commit'); }), /missing commit/);
  await assert.rejects(withCompatibleRollback(context, 'main', deploy, ancestry));
});

test('earlier releases retain their existing rollback behavior', async () => {
  assert.equal(await withCompatibleRollback({ migrations: [] }, legacy, () => 'old-policy', () => assert.fail('no provider floor')), 'old-policy');
});

test('a compatible immutable fallback replaces only an unsafe prior image', async () => {
  const snapshot = { sha: legacy, image: 'prior@sha256:legacy', imageDigest: 'sha256:legacy', rollbackPolicy: { compatible: false } };
  const fallback = { sha: modern, image: 'recovery@sha256:modern', acceptableImageDigests: ['sha256:modern', 'sha256:amd64'] };
  assert.throws(() => selectRecoveryTarget(snapshot), /No verified compatible recovery artifact/);
  const target = selectRecoveryTarget(snapshot, fallback);
  assert.deepEqual(target, fallback);
  let deployed;
  await withCompatibleRollback(context, target.sha, () => { deployed = target.image; }, ancestry);
  assert.equal(deployed, fallback.image);
  const compatible = { ...snapshot, sha: floor, rollbackPolicy: { compatible: true } };
  assert.equal(selectRecoveryTarget(compatible, fallback).sha, floor);
  const args = parseArgs(['preflight', '--sha', modern, '--recovery-sha', floor]);
  assert.equal(args.recoverySha, floor);
});

test('bounded conformance: mixed machine histories, failure order and retries never deploy an unsafe prior', async () => {
  // Enumerate the same two-machine prior states as ProviderRollback.tla. Repeat
  // attempts represent the canary catch and outer fleet recovery catch.
  for (let mask = 0; mask < 4; mask++) {
    const priors = [0, 1].map(i => mask & (1 << i) ? modern : legacy);
    for (const order of [[0, 1], [1, 0]]) {
      const deployed = [];
      for (const machine of [...order, ...order]) {
        try {
          await withCompatibleRollback(context, priors[machine], () => deployed.push(machine), ancestry);
        } catch (error) {
          assert.equal(priors[machine], legacy);
          assert.match(error.message, /Unsafe authentication rollback blocked/);
        }
      }
      assert.ok(deployed.every(machine => priors[machine] === modern));
      assert.equal(deployed.length, priors.filter(sha => sha === modern).length * 2);
    }
  }
});


test('canary failure recovers untouched legacy replicas and all touched replicas', async () => {
  for (let mask = 0; mask < 4; mask++) {
    const machines = [0, 1].map(id => ({ id, releaseSnapshot: { rollbackPolicy: { compatible: Boolean(mask & (1 << id)) } } }));
    for (const canary of [0, 1]) {
      const writes = [];
      const result = await recoverReleaseMachines(machines, new Set([canary]), async machine => {
        writes.push(machine.id);
        return { machineId: machine.id };
      });
      const expected = machines.filter(m => m.id === canary || !m.releaseSnapshot.rollbackPolicy.compatible).map(m => m.id).reverse();
      assert.deepEqual(writes, expected);
      assert.equal(result.rollbacks.length, expected.length);
      assert.deepEqual(result.errors, []);
    }
  }
});

test('recovery attempts every required replica despite a failure and records incomplete recovery', async () => {
  const machines = [0, 1].map(id => ({ id, releaseSnapshot: { rollbackPolicy: { compatible: false } } }));
  const attempted = [];
  const result = await recoverReleaseMachines(machines, new Set([0]), async machine => {
    attempted.push(machine.id);
    if (machine.id === 1) throw new Error('provider unavailable');
    return { machineId: 0 };
  });
  assert.deepEqual(attempted, [1, 0]);
  assert.deepEqual(result.errors, [{ machineId: 1, error: 'provider unavailable' }]);
  assert.deepEqual(result.rollbacks, [{ machineId: 0 }]);
});

test('pre-deployment failure does not mutate the untouched fleet', async () => {
  const machines = [{ id: 0, releaseSnapshot: { rollbackPolicy: { compatible: false } } }];
  assert.deepEqual(await recoverReleaseMachines(machines, new Set(), () => assert.fail('no deploy attempt')), { rollbacks: [], errors: [] });
});
