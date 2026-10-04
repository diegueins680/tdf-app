import test from 'node:test';
import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
import { parseArgs, recoverReleaseMachines, requiredIdentityCommit, disabledEscrowWritesAt, rollbackCompatibility, selectRecoveryTarget, withCompatibleRollback } from '../production-release.mjs';

const legacy = '2f01b20b0c2a2e2088570c3dc5deba6197266452';
const modern = '5c11577a5d31f079b3e070a7810a6b04a48d99f4';
const floor = 'c53b33e7ef868fb7b64f876199ed66be0f617efc';
const context = { migrations: [{ id: '2026-09-18_provider_subject_identity' }] };
const ancestry = async (required, candidate) => {
  assert.equal(required, floor);
  return candidate === modern || candidate === floor;
};

test('intake recovery cannot restore a provider-only writer that ignores submission receipts', async () => {
  const intake = { migrations: [...context.migrations, { id: '2026-09-18_live_intake_idempotency' }] };
  const required = '02115f7d1b0786f3cdd4287a9466dd22682f603b';
  assert.equal(requiredIdentityCommit(intake), required);
  let writes = 0;
  await assert.rejects(withCompatibleRollback(intake, modern, () => { writes++; }, async (floor, prior) => {
    assert.equal(floor, required); assert.equal(prior, modern); return false;
  }), /source request receipts/);
  assert.equal(writes, 0);
});

test('each source request migration requires the reviewed combined writer before recovery can mutate', async () => {
  const required = '6eab8592744015124b0162ce9e9361f51a04f538';
  const compatible = 'bdd9e24bddaaa96e2da72d75041b1e1b20236e04';
  const oldIntake = 'e1a825bda26dbb16b1c732e551cc4880d5626943';
  for (const id of ['2026-09-18_course_identity_requests', '2026-09-18_trial_identity_requests', '2026-09-18_ads_identity_requests']) {
    const scoped = { migrations: [...context.migrations, { id }] };
    let writes = 0;
    const history = async (floor, prior) => { assert.equal(floor, required); return prior === compatible; };
    await assert.rejects(withCompatibleRollback(scoped, oldIntake, () => { writes++; }, history), /source request receipts/);
    assert.equal(writes, 0);
    assert.equal(await withCompatibleRollback(scoped, compatible, () => { writes++; return 'verified'; }, history), 'verified');
    assert.equal(writes, 1);
    await assert.rejects(withCompatibleRollback(scoped, compatible, () => assert.fail('missing history must block'), async () => { throw new Error('history unavailable'); }), /history unavailable/);
  }
});

test('first provider rollout refuses the actual prior binary before any deploy callback', async () => {
  let writes = 0;
  assert.deepEqual(await rollbackCompatibility(context, legacy, ancestry), {
    sha: legacy, requiredCommit: floor, escrowWritesDisabled: null, compatible: false,
  });
  await assert.rejects(withCompatibleRollback(context, legacy, () => { writes++; }, ancestry), /recover forward/);
  assert.equal(writes, 0);
});

test('combined source-request requirements cannot weaken the identity recovery floor', () => {
  const earlier = ['2026-09-18_provider_subject_identity', '2026-09-18_live_intake_idempotency'];
  for (let mask = 0; mask < 4; mask++) {
    const migrations = earlier.filter((_, index) => mask & (1 << index)).map(id => ({ id }));
    for (const id of ['2026-09-18_course_identity_requests', '2026-09-18_trial_identity_requests', '2026-09-18_ads_identity_requests']) {
      for (const order of [[{ id }, ...migrations], [...migrations, { id }]]) {
        assert.equal(requiredIdentityCommit({ migrations: order }), '6eab8592744015124b0162ce9e9361f51a04f538');
      }
    }
  }
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



const escrowSource = readFileSync(new URL('../../tdf-hq/src/TDF/Server.hs', import.meta.url), 'utf8');
const escrowApiSource = readFileSync(new URL('../../tdf-hq/src/TDF/API.hs', import.meta.url), 'utf8');
const escrowFix = '33083d469727da73e73ea4a2c2be5aaec130b137';
const auditedBeforeDisablement = 'd517d6ed12a0f9ed3929df124507c7f4b025d16d';
test('exact Git source rejects the previously eligible audit image after escrow disablement', async () => {
  const target = { sha: escrowFix, migrations: [{ id: '2026-09-18_course_identity_requests' }] };
  assert.equal(await disabledEscrowWritesAt(escrowFix), true);
  assert.equal((await rollbackCompatibility(target, auditedBeforeDisablement)).compatible, false);
  assert.equal((await rollbackCompatibility(target, escrowFix)).compatible, true);
  await assert.rejects(withCompatibleRollback(target, auditedBeforeDisablement,
    () => assert.fail('must never restore nominal financial writers')), /Unsafe financial-write/);
});

test('identity and financial-write protection must hold together before any recovery mutation', async () => {
  const target = { ...context, sha: modern };
  for (const identity of [false, true]) for (const escrow of [false, true]) {
    const history = async (required, candidate) => {
      assert.equal(required, floor); assert.equal(candidate, legacy); return identity;
    };
    const readSource = async (candidate, path) => {
      assert.equal(candidate, legacy);
      if (path === 'tdf-hq/src/TDF/API.hs') return escrowApiSource;
      assert.equal(path, 'tdf-hq/src/TDF/Server.hs');
      return escrow ? escrowSource : 'legacy nominal writers';
    };
    let writes = 0;
    const deploy = () => { writes++; return 'compatible recovery'; };
    if (identity && escrow) {
      assert.equal(await withCompatibleRollback(target, legacy, deploy, history, readSource), 'compatible recovery');
      assert.equal(writes, 1);
    } else {
      await assert.rejects(withCompatibleRollback(target, legacy, deploy, history, readSource), /rollback blocked/);
      assert.equal(writes, 0);
    }
  }
});

test('source read failure is inconclusive and cannot authorize recovery mutation', async () => {
  await assert.rejects(withCompatibleRollback({ migrations: [], sha: modern }, legacy,
    () => assert.fail('must not deploy without candidate source'), async () => true,
    async () => { throw new Error('history unavailable'); }), /history unavailable/);
});

test('financial compatibility follows source, including squash and later reintroduction', async () => {
  const target = { sha: modern, migrations: [] };
  const noMarkerAncestry = () => assert.fail('financial contract must not depend on a commit marker');
  assert.equal((await rollbackCompatibility(target, legacy, noMarkerAncestry, async (_, path) => path.endsWith('/API.hs') ? escrowApiSource : escrowSource)).compatible, true);
  assert.equal((await rollbackCompatibility(target, modern, noMarkerAncestry,
    async (_, path) => path.endsWith('/API.hs') ? escrowApiSource : escrowSource.replace('createServiceMarketplaceBooking _ _ =', 'createServiceMarketplaceBooking user request ='))).compatible, false);
});

test('actual recovery guard rejects safe unused stubs with financial routes rebound', async () => {
  const target = { sha: modern, migrations: [] };
  await assert.rejects(withCompatibleRollback(target, legacy,
    () => assert.fail('must not deploy a rerouted financial writer'), async () => true,
    async (_, path) => path.endsWith('/API.hs') ? escrowApiSource
      : escrowSource.replace(':<|> releaseServiceMarketplaceEscrow user', ':<|> legacyRelease user')), /Unsafe financial-write/);
});
