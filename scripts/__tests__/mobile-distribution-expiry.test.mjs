import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describeFinding, distributionExpiry, PUBLISHED_MANIFEST_URL } from '../check-mobile-distribution.mjs';

const script = fileURLToPath(new URL('../check-mobile-distribution.mjs', import.meta.url));
const workflow = readFileSync(new URL('../../.github/workflows/check-mobile-distribution.yml', import.meta.url), 'utf8');
const now = Date.parse('2026-10-11T18:00:00Z');
const channel = (validUntil, extra = {}) => ({
  status: 'testflight_external', capacity: 'available', verifiedAt: '2026-10-05T00:00:00Z', validUntil,
  url: 'https://testflight.apple.com/join/abc', ...extra,
});
const manifest = (ios, android) => ({ ios, android });
const states = value => distributionExpiry(value, now).map(finding => `${finding.platform}:${finding.state}`);

test('a channel with more than the warning window left is current', () => {
  assert.deepEqual(states(manifest(channel('2026-10-13T18:00:00Z'), channel('2026-10-18T18:00:00Z'))), ['ios:current', 'android:current']);
});

test('a channel inside the warning window is expiring, each platform on its own', () => {
  assert.deepEqual(states(manifest(channel('2026-10-12T04:57:43Z'), channel('2026-10-18T18:00:00Z'))), ['ios:expiring', 'android:current']);
  assert.deepEqual(states(manifest(channel('2026-10-18T18:00:00Z'), channel('2026-10-13T17:59:59Z'))), ['ios:current', 'android:expiring']);
});

test('a channel at or past validUntil is expired, matching the landing page boundary', () => {
  assert.deepEqual(states(manifest(channel('2026-10-11T18:00:00Z'), channel('2026-10-11T17:00:00Z'))), ['ios:expired', 'android:expired']);
});

test('a channel that offers no store link cannot lapse', () => {
  const closed = { status: 'coming_soon', verifiedAt: '2026-10-01T00:00:00Z', validUntil: '2026-10-02T00:00:00Z' };
  assert.deepEqual(states(manifest(closed, channel('2026-10-18T18:00:00Z'))), ['android:current']);
});

test('the warning window is configurable and must be positive', () => {
  const value = manifest(channel('2026-10-13T18:00:00Z'), channel('2026-10-13T18:00:00Z'));
  assert.deepEqual(distributionExpiry(value, now, 72).map(finding => finding.state), ['expiring', 'expiring']);
  assert.throws(() => distributionExpiry(value, now, 0));
});

for (const [name, value] of [
  ['no manifest', null],
  ['a missing platform', { ios: channel('2026-10-18T18:00:00Z') }],
  ['an unparseable date', manifest(channel('soon'), channel('2026-10-18T18:00:00Z'))],
  ['a numeric date', manifest(channel(1760000000000), channel('2026-10-18T18:00:00Z'))],
]) test(`rejects ${name} instead of reporting it current`, () => {
  assert.throws(() => distributionExpiry(value, now));
});

test('messages say what the visitor loses and what the operator does', () => {
  const [expiring] = distributionExpiry(manifest(channel('2026-10-12T04:57:43Z'), channel('2026-10-18T18:00:00Z')), now);
  assert.match(describeFinding(expiring), /^ios \(testflight_external\) expires in 11\.0 h at 2026-10-12T04:57:43Z; re-verify/);
  const [expired] = distributionExpiry(manifest(channel('2026-10-11T12:00:00Z'), channel('2026-10-18T18:00:00Z')), now);
  assert.match(describeFinding(expired), /expired 6\.0 h ago .* no longer offers its store link/);
});

test('the command exits non-zero for a lapsing file and zero for a current one', () => {
  const directory = mkdtempSync(join(tmpdir(), 'tdf-distribution-'));
  try {
    const day = 86_400_000;
    const at = offset => new Date(Date.now() + offset).toISOString();
    const current = join(directory, 'current.json');
    const lapsing = join(directory, 'lapsing.json');
    writeFileSync(current, JSON.stringify(manifest(channel(at(5 * day)), channel(at(6 * day)))));
    writeFileSync(lapsing, JSON.stringify(manifest(channel(at(5 * day)), channel(at(day / 2)))));
    assert.match(execFileSync('node', [script, '--file', current], { encoding: 'utf8' }), /android \(testflight_external\) is valid for/);
    const failed = spawnSync('node', [script, '--file', lapsing], { encoding: 'utf8' });
    assert.equal(failed.status, 1);
    assert.match(failed.stdout, /::error::android \(testflight_external\) expires in/);
    const unsupported = spawnSync('node', [script, '--renew'], { encoding: 'utf8' });
    assert.equal(unsupported.status, 1);
    assert.match(unsupported.stderr, /Unsupported argument: --renew/);
  } finally { rmSync(directory, { recursive: true, force: true }); }
});

test('the committed manifest has the shape the check reads', () => {
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
