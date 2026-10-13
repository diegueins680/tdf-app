import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  ACCESSIBLE_STATUSES, describeFinding, distributionExpiry, DISTRIBUTION_STATUSES, PUBLISHED_MANIFEST_URL, validatePublishedDistribution,
} from '../check-mobile-distribution.mjs';

const script = fileURLToPath(new URL('../check-mobile-distribution.mjs', import.meta.url));
const workflow = readFileSync(new URL('../../.github/workflows/check-mobile-distribution.yml', import.meta.url), 'utf8');
const landing = readFileSync(new URL('../../tdf-hq-ui/src/mobile/distribution.ts', import.meta.url), 'utf8');
const now = Date.parse('2026-10-11T18:00:00Z');
const later = '2026-10-18T18:00:00Z';
const ios = (validUntil, extra = {}) => ({
  status: 'testflight_external', capacity: 'available', verifiedAt: '2026-10-05T00:00:00Z', validUntil,
  url: 'https://testflight.apple.com/join/abc', ...extra,
});
const android = (validUntil, extra = {}) => ({
  status: 'closed_testing', capacity: 'available', admission: 'approval_required', verifiedAt: '2026-10-05T00:00:00Z', validUntil,
  url: 'https://play.google.com/apps/testing/com.tdf.records', ...extra,
});
const manifest = (iosChannel, androidChannel) => ({ ios: iosChannel, android: androidChannel });
const states = value => distributionExpiry(value, now).map(finding => `${finding.platform}:${finding.state}`);

test('a channel with more than the warning window left is current', () => {
  assert.deepEqual(states(manifest(ios('2026-10-13T18:00:00Z'), android(later))), ['ios:current', 'android:current']);
});

test('a channel inside the warning window is expiring, each platform on its own', () => {
  assert.deepEqual(states(manifest(ios('2026-10-12T04:57:43Z'), android(later))), ['ios:expiring', 'android:current']);
  assert.deepEqual(states(manifest(ios(later), android('2026-10-13T17:59:59Z'))), ['ios:current', 'android:expiring']);
});

test('a channel at or past validUntil is expired, matching the landing page boundary', () => {
  assert.deepEqual(states(manifest(ios('2026-10-11T18:00:00Z'), android('2026-10-11T17:00:00Z'))), ['ios:expired', 'android:expired']);
});

test('a channel whose status offers no store link cannot lapse', () => {
  const closed = { status: 'coming_soon', verifiedAt: '2026-10-01T00:00:00Z', validUntil: '2026-10-02T00:00:00Z' };
  assert.deepEqual(states(manifest(closed, android(later))), ['android:current']);
});

test('a channel verified in the future fails, because the landing hides it until then', () => {
  const value = manifest(ios(later, { verifiedAt: '2026-10-11T18:00:01Z' }), android(later));
  assert.throws(() => distributionExpiry(value, now), /ios verifiedAt 2026-10-11T18:00:01Z is in the future/);
  assert.deepEqual(states(manifest(ios(later, { verifiedAt: '2026-10-11T18:00:00Z' }), android(later))), ['ios:current', 'android:current']);
});

test('a beta channel withheld for capacity cannot lapse; a public one needs no capacity', () => {
  for (const capacity of ['full', 'unknown', undefined]) {
    assert.deepEqual(states(manifest(ios('2026-10-12T00:00:00Z', { capacity }), android(later))), ['android:current']);
  }
  const store = ios('2026-10-12T00:00:00Z', { status: 'public', capacity: undefined, url: 'https://apps.apple.com/app/id6779786470' });
  assert.deepEqual(states(manifest(store, android(later))), ['ios:expiring', 'android:current']);
});

test('the warning window is configurable and must be positive', () => {
  const value = manifest(ios('2026-10-13T18:00:00Z'), android('2026-10-13T18:00:00Z'));
  assert.deepEqual(distributionExpiry(value, now, 72).map(finding => finding.state), ['expiring', 'expiring']);
  assert.throws(() => distributionExpiry(value, now, 0));
});

// The landing rejects the whole manifest for each of these and then offers no store link at all.
for (const [name, change] of [
  ['an accessible iOS status without a URL', value => { delete value.ios.url; }],
  ['an accessible Android status without a URL', value => { delete value.android.url; }],
  ['a URL on a status that offers none', value => { value.ios.status = 'coming_soon'; }],
  ['an unknown status', value => { value.android.status = 'beta'; }],
  ['a Play status on iOS', value => { value.ios.status = 'closed_testing'; }],
  ['a TestFlight status on Android', value => { value.android.status = 'testflight_external'; }],
  ['a missing platform', value => { delete value.android; }],
  ['validUntil not after verifiedAt', value => { value.ios.validUntil = value.ios.verifiedAt; }],
  ['an unparseable validUntil', value => { value.ios.validUntil = 'soon'; }],
  ['a numeric validUntil', value => { value.ios.validUntil = 1760000000000; }],
  ['an unparseable verifiedAt', value => { value.android.verifiedAt = 'yesterday'; }],
  ['a look-alike TestFlight host', value => { value.ios.url = 'https://testflight.apple.com.evil.test/join/abc'; }],
  ['a TestFlight URL with a query', value => { value.ios.url = 'https://testflight.apple.com/join/abc?token=x'; }],
  ['credentials in the URL', value => { value.ios.url = 'https://user:secret@testflight.apple.com/join/abc'; }],
  ['a non-https URL', value => { value.android.url = 'http://play.google.com/apps/testing/com.tdf.records'; }],
  ['another Play package', value => { value.android.url = 'https://play.google.com/apps/testing/com.other'; }],
  ['closed testing without an admission policy', value => { delete value.android.admission; }],
  ['an unknown capacity', value => { value.ios.capacity = 'plenty'; }],
  ['an enrollment URL outside Google Groups', value => { value.android.enrollmentUrl = 'https://evil.test/g/tdf'; }],
]) test(`fails for ${name}, as the landing does, instead of exiting green`, () => {
  const value = manifest(ios(later), android(later));
  assert.equal(distributionExpiry(value, now).length, 2);
  change(value);
  assert.throws(() => validatePublishedDistribution(value));
  assert.throws(() => distributionExpiry(value, now));
});

test('rejects a manifest that is not an object', () => {
  for (const value of [null, undefined, 'manifest', 7]) assert.throws(() => distributionExpiry(value, now));
});

test('accepts the public store destinations the landing accepts', () => {
  const value = manifest(
    ios(later, { status: 'public', capacity: undefined, url: 'https://apps.apple.com/app/id6779786470' }),
    android(later, { status: 'public', admission: undefined, url: 'https://play.google.com/store/apps/details?id=com.tdf.records' }));
  assert.deepEqual(states(value), ['ios:current', 'android:current']);
});

test('the status lists are the ones the landing declares', () => {
  const declared = name => {
    const literal = landing.match(new RegExp(`const ${name} = new Set<DistributionStatus>\\(\\[([^\\]]+)\\]\\)`));
    return [...literal[1].matchAll(/'([a-z_]+)'/g)].map(match => match[1]);
  };
  assert.deepEqual(DISTRIBUTION_STATUSES, declared('statuses'));
  assert.deepEqual(ACCESSIBLE_STATUSES, declared('accessible'));
});

test('messages say what the visitor loses and what the operator does', () => {
  const [expiring] = distributionExpiry(manifest(ios('2026-10-12T04:57:43Z'), android(later)), now);
  assert.match(describeFinding(expiring), /^ios \(testflight_external\) expires in 11\.0 h at 2026-10-12T04:57:43Z; re-verify/);
  const [expired] = distributionExpiry(manifest(ios('2026-10-11T12:00:00Z'), android(later)), now);
  assert.match(describeFinding(expired), /expired 6\.0 h ago .* no longer offers its store link/);
});

test('the command exits non-zero for a lapsing or invalid file and zero for a current one', () => {
  const directory = mkdtempSync(join(tmpdir(), 'tdf-distribution-'));
  try {
    const day = 86_400_000;
    const at = offset => new Date(Date.now() + offset).toISOString();
    const write = (name, value) => { const path = join(directory, name); writeFileSync(path, JSON.stringify(value)); return path; };
    const current = write('current.json', manifest(ios(at(5 * day)), android(at(6 * day))));
    const lapsing = write('lapsing.json', manifest(ios(at(5 * day)), android(at(day / 2))));
    const linkless = manifest(ios(at(5 * day)), android(at(6 * day)));
    delete linkless.ios.url;
    assert.match(execFileSync('node', [script, '--file', current], { encoding: 'utf8' }), /android \(closed_testing\) is valid for/);
    const failed = spawnSync('node', [script, '--file', lapsing], { encoding: 'utf8' });
    assert.equal(failed.status, 1);
    assert.match(failed.stdout, /::error::android \(closed_testing\) expires in/);
    const invalid = spawnSync('node', [script, '--file', write('linkless.json', linkless)], { encoding: 'utf8' });
    assert.equal(invalid.status, 1);
    assert.match(invalid.stderr, /::error::Mobile distribution check failed: ios channel URL contradicts status/);
    const unsupported = spawnSync('node', [script, '--renew'], { encoding: 'utf8' });
    assert.equal(unsupported.status, 1);
    assert.match(unsupported.stderr, /Unsupported argument: --renew/);
  } finally { rmSync(directory, { recursive: true, force: true }); }
});

test('the committed manifest passes the landing invariants', () => {
  const committed = JSON.parse(readFileSync(new URL('../../tdf-hq-ui/public/mobile-distribution.json', import.meta.url), 'utf8'));
  assert.equal(distributionExpiry(committed, Date.parse(committed.ios.verifiedAt)).length, 2);
});

test('the scheduled workflow only reads the published manifest', () => {
  assert.equal(PUBLISHED_MANIFEST_URL, 'https://www.tdfrecords.net/mobile-distribution.json');
  assert.match(workflow, /\n  schedule:\n(?: +#.*\n)* +- cron: '0 13 \* \* \*'\n/);
  assert.match(workflow, /permissions:\n  contents: read\n/);
  assert.equal(workflow.match(/run: node scripts\/check-mobile-distribution\.mjs\n/g)?.length, 1);
  assert.doesNotMatch(workflow, /contents: write|git push|pull_request_target/);
});
