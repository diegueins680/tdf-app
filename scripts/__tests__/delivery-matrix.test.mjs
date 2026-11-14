import assert from 'node:assert/strict';
import test from 'node:test';
import { mkdtempSync, mkdirSync, copyFileSync, rmSync } from 'node:fs';
import { spawnSync, execFileSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { assessReceipt, buildDeliveryMatrix, DELIVERY_CHECKS } from '../lib/delivery-matrix.mjs';

const identity = { rootRevision: 'a'.repeat(40), sourceDigest: 'b'.repeat(64), rootDirty: false,
  mobilePin: 'c'.repeat(40), mobileCheckoutRevision: 'c'.repeat(40), mobileDirty: false };
const now = Date.parse('2026-11-13T12:00:00Z');
function receipt(check) {
  return { schemaVersion: 1, checkId: check.id, level: check.level, result: 'passed',
    rootRevision: identity.rootRevision, sourceDigest: identity.sourceDigest, mobileRevision: identity.mobilePin,
    completedAt: '2026-11-13T11:00:00Z', provenance: 'synthetic://test-fixture', scope: check.criterion,
    limitations: ['Synthetic fixture; not execution evidence.'], counts: { passed: 2, failed: 0, skipped: 0 },
    artifacts: [{ reference: 'synthetic://artifact', sha256: 'd'.repeat(64) }],
    physicalDevice: true, binarySha256: 'd'.repeat(64), signatureVerified: true,
    completeSchema: true, migrationManifestSha256: 'e'.repeat(64),
    concurrentScheduleObserved: true, negativeControlsPassed: true,
    observedRevision: identity.rootRevision, imageSha256: 'f'.repeat(64), runtimeConfigSha256: 'e'.repeat(64),
    isolated: true, recoverySetId: 'fixture', restored: { database: true, publicAssets: true, privateUploads: true, secrets: true },
    contentHashesVerified: true, privateAccessVerified: true, releaseDecision: 'READY' };
}
const check = id => DELIVERY_CHECKS.find(c => c.id === id);
const assess = (id, patch) => assessReceipt(check(id), { ...receipt(check(id)), ...patch }, identity, now);

test('no evidence stays incomplete; missing Mobile cannot be silently skipped', () => {
  const report = buildDeliveryMatrix({ ...identity, mobileCheckoutRevision: null, mobileDirty: null }, {}, { now });
  assert.equal(report.status, 'INCOMPLETE');
  assert.equal(report.rows.filter(r => r.status === 'missing').length, DELIVERY_CHECKS.length);
  assert.ok(report.blockers.some(x => x.includes('Mobile checkout')));
  assert.equal(report.releaseAuthorized, false);
});

test('even fully consistent synthetic receipts never grant release authority', () => {
  const receipts = Object.fromEntries(DELIVERY_CHECKS.map(c => [c.id, receipt(c)]));
  const report = buildDeliveryMatrix(identity, receipts, { now });
  assert.equal(report.status, 'RECEIPTS_CONSISTENT');
  assert.equal(report.releaseAuthorized, false);
  for (const changed of [{ rootDirty: true }, { mobileDirty: true }, { mobileCheckoutRevision: 'd'.repeat(40) }]) {
    assert.equal(buildDeliveryMatrix({ ...identity, ...changed }, receipts, { now }).status, 'INCOMPLETE');
  }
});

test('wrong revision, dirty-source digest and mobile revision are refused', () => {
  for (const patch of [{ rootRevision: 'd'.repeat(40) }, { sourceDigest: 'd'.repeat(64) }, { mobileRevision: 'd'.repeat(40) }]) {
    assert.equal(assess('api-contract', patch).status, 'inconsistent');
  }
});

test('failures, skips, zero cases and incomplete provenance are not passes', () => {
  for (const patch of [{ result: 'skipped' }, { counts: { passed: 0, failed: 0, skipped: 0 } },
    { counts: { passed: 10, failed: 1, skipped: 0 } }, { counts: { passed: 10, failed: 0, skipped: 1 } },
    { provenance: '' }, { scope: '' }, { artifacts: [] }, { limitations: undefined },
    { completedAt: 'not a date' }, { completedAt: '2026-11-14T12:00:00Z' }]) {
    assert.equal(assess('booking-concurrency', patch).status, 'inconsistent', JSON.stringify(patch));
  }
});

test('models, simulators and build success cannot replace runtime or device acceptance', () => {
  assert.equal(assess('payment-concurrency', { level: 'inductive-model' }).status, 'inconsistent');
  assert.equal(assess('ios-device', { physicalDevice: false }).status, 'inconsistent');
  assert.equal(assess('android-device', { level: 'build' }).status, 'inconsistent');
  assert.equal(assess('windows-installed', { signatureVerified: false }).status, 'inconsistent');
});

test('concurrency evidence must include full schema, observed schedules and negative controls', () => {
  for (const field of ['completeSchema', 'concurrentScheduleObserved', 'negativeControlsPassed']) {
    assert.equal(assess('payment-concurrency', { [field]: false }).status, 'inconsistent');
  }
  assert.equal(assess('capacity-concurrency', { migrationManifestSha256: null }).status, 'inconsistent');
});

test('health alone and database-only recovery cannot satisfy delivery criteria', () => {
  assert.equal(assess('backend-runtime', { imageSha256: undefined }).status, 'inconsistent');
  for (const missing of ['database', 'publicAssets', 'privateUploads', 'secrets']) {
    const restored = { ...receipt(check('coordinated-restore')).restored, [missing]: false };
    assert.equal(assess('coordinated-restore', { restored }).status, 'inconsistent');
  }
  assert.equal(assess('coordinated-restore', { privateAccessVerified: false }).status, 'inconsistent');
});

test('successful proof execution does not erase a blocked formal release decision', () => {
  assert.equal(assess('formal-assurance', { releaseDecision: 'BLOCKED' }).status, 'inconsistent');
});

test('old runtime observations cannot masquerade as current availability', () => {
  assert.equal(assess('backend-runtime', { completedAt: '2026-11-11T12:00:00Z' }).status, 'inconsistent');
});

test('CLI refuses missing Mobile and malformed arguments with distinct exit codes', t => {
  const root = mkdtempSync(path.join(tmpdir(), 'tdf-delivery-cli-'));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(path.join(root, 'scripts/lib'), { recursive: true });
  for (const name of ['delivery-matrix.mjs', 'lib/delivery-matrix.mjs', 'lib/verification-evidence.mjs']) {
    copyFileSync(new URL(`../${name}`, import.meta.url), path.join(root, 'scripts', name));
  }
  const git = (...args) => execFileSync('git', ['-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.test', ...args], { cwd: root, stdio: 'pipe' });
  git('init', '-q');
  git('add', 'scripts');
  git('update-index', '--add', '--cacheinfo', `160000,${identity.mobilePin},tdf-mobile`);
  git('commit', '-qm', 'fixture');
  mkdirSync(path.join(root, 'tdf-mobile'));
  const invoke = (...args) => spawnSync(process.execPath, ['scripts/delivery-matrix.mjs', ...args], { cwd: root, encoding: 'utf8' });
  const result = invoke('--require-complete');
  assert.equal(result.status, 2, result.stderr);
  const report = JSON.parse(result.stdout);
  assert.equal(report.identity.mobileCheckoutRevision, null);
  assert.equal(report.status, 'INCOMPLETE');
  assert.equal(invoke().status, 0);
  assert.equal(invoke('--mobile').status, 1);
  assert.equal(invoke('--execute').status, 1);
});
