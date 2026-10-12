#!/usr/bin/env node
// Read-only expiry check for the published Mobile distribution manifest.
// The landing hides a channel's store link once its validUntil passes; this
// reports a channel before that happens. It never renews or edits the manifest:
// renewal needs a fresh store observation (docs/mobile-program/README.md).
import { readFile } from 'node:fs/promises';
import { pathToFileURL } from 'node:url';

export const PUBLISHED_MANIFEST_URL = 'https://www.tdfrecords.net/mobile-distribution.json';
export const DEFAULT_WARNING_HOURS = 48;
const HOUR = 3_600_000;

const STATUSES = ['unavailable', 'coming_soon', 'internal_testing', 'closed_testing', 'open_testing', 'testflight_internal', 'testflight_external', 'store_preorder', 'public'];
const ACCESSIBLE = ['closed_testing', 'open_testing', 'testflight_external', 'store_preorder', 'public'];
export const DISTRIBUTION_STATUSES = { all: STATUSES, accessible: ACCESSIBLE };

// The same invariants as validateDistribution in tdf-hq-ui/src/mobile/distribution.ts.
// The landing rejects the whole manifest when one fails and then offers no store
// link on either platform, so a manifest it would reject must fail here too.
export function validatePublishedDistribution(manifest) {
  if (!manifest || typeof manifest !== 'object') throw new Error('Invalid mobile distribution');
  for (const platform of ['ios', 'android']) {
    const c = manifest[platform];
    if (!c || typeof c !== 'object') throw new Error(`Missing ${platform} channel`);
    if (!STATUSES.includes(c.status) || typeof c.verifiedAt !== 'string' || typeof c.validUntil !== 'string'
      || !Number.isFinite(Date.parse(c.verifiedAt)) || !Number.isFinite(Date.parse(c.validUntil))
      || Date.parse(c.validUntil) <= Date.parse(c.verifiedAt)) throw new Error(`Invalid ${platform} distribution state`);
    if (platform === 'ios' && ['internal_testing', 'closed_testing', 'open_testing'].includes(c.status)) throw new Error('Wrong iOS channel');
    if (platform === 'android' && c.status.startsWith('testflight')) throw new Error('Wrong Android channel');
    if (ACCESSIBLE.includes(c.status) !== Boolean(c.url)) throw new Error(`${platform} channel URL contradicts status`);
    if (c.capacity && !['available', 'full', 'unknown'].includes(c.capacity)) throw new Error(`Invalid ${platform} capacity`);
    if (c.url) {
      const u = new URL(c.url);
      if (u.protocol !== 'https:' || u.username || u.password || u.port || u.hash) throw new Error(`Unsafe ${platform} distribution URL`);
      const valid = platform === 'ios'
        ? c.status === 'testflight_external' ? u.hostname === 'testflight.apple.com' && /^\/join\/[a-zA-Z0-9]+$/.test(u.pathname) && !u.search
          : u.hostname === 'apps.apple.com' && u.pathname.endsWith('/id6779786470')
        : u.hostname === 'play.google.com' && (c.status === 'public' || c.status === 'store_preorder'
          ? u.pathname === '/store/apps/details' && u.searchParams.get('id') === 'com.tdf.records'
          : u.pathname === '/apps/testing/com.tdf.records');
      if (!valid) throw new Error(`Unexpected ${platform} distribution destination`);
    }
    if (c.status === 'closed_testing' && !['direct', 'approval_required'].includes(c.admission ?? '')) throw new Error('Closed testing requires admission policy');
    if (c.enrollmentUrl) {
      const u = new URL(c.enrollmentUrl);
      if (c.status !== 'closed_testing' || u.protocol !== 'https:' || u.hostname !== 'groups.google.com' || !/^\/g\/[\w-]+$/.test(u.pathname)
        || u.search || u.hash || u.username || u.password || u.port) throw new Error(`Unsafe ${platform} enrollment URL`);
    }
  }
  return manifest;
}

// One finding per platform whose store link the landing can offer: 'current',
// 'expiring' or 'expired'. Mirrors availableChannel in distribution.ts: a channel
// verified in the future is hidden there, so it is an error here; a beta channel
// whose capacity is not 'available' is withheld on purpose and cannot lapse.
export function distributionExpiry(manifest, now, warningHours = DEFAULT_WARNING_HOURS) {
  if (!Number.isFinite(now) || !(warningHours > 0)) throw new Error('Invalid check time or warning window');
  validatePublishedDistribution(manifest);
  const findings = [];
  for (const platform of ['ios', 'android']) {
    const channel = manifest[platform];
    if (!channel.url) continue; // A status that offers no link; validated above.
    if (Date.parse(channel.verifiedAt) > now) throw new Error(`${platform} verifiedAt ${channel.verifiedAt} is in the future; /app hides the link until then`);
    if (channel.status !== 'public' && channel.status !== 'store_preorder' && channel.capacity !== 'available') continue;
    const hoursLeft = (Date.parse(channel.validUntil) - now) / HOUR;
    const state = hoursLeft <= 0 ? 'expired' : hoursLeft < warningHours ? 'expiring' : 'current';
    findings.push({ platform, status: channel.status, validUntil: channel.validUntil, hoursLeft, state });
  }
  return findings;
}

export function describeFinding(finding) {
  const hours = Math.abs(finding.hoursLeft).toFixed(1);
  if (finding.state === 'expired') return `${finding.platform} (${finding.status}) expired ${hours} h ago at ${finding.validUntil}; /app no longer offers its store link`;
  if (finding.state === 'expiring') return `${finding.platform} (${finding.status}) expires in ${hours} h at ${finding.validUntil}; re-verify the store channel and renew the manifest`;
  return `${finding.platform} (${finding.status}) is valid for ${hours} h, until ${finding.validUntil}`;
}

async function load(args) {
  const file = args.indexOf('--file');
  if (file !== -1) {
    if (!args[file + 1]) throw new Error('--file needs a path');
    return JSON.parse(await readFile(args[file + 1], 'utf8'));
  }
  const response = await fetch(PUBLISHED_MANIFEST_URL, { cache: 'no-store', redirect: 'error', signal: AbortSignal.timeout(30_000) });
  if (!response.ok) throw new Error(`Published manifest returned HTTP ${response.status}`);
  return response.json();
}

async function main(args) {
  const unknown = args.filter((arg, index) => arg !== '--file' && args[index - 1] !== '--file');
  if (unknown.length) throw new Error(`Unsupported argument: ${unknown[0]}`);
  const findings = distributionExpiry(await load(args), Date.now());
  for (const finding of findings) {
    console.log(`${finding.state === 'current' ? '' : '::error::'}${describeFinding(finding)}`);
  }
  if (!findings.length) console.log('No store link is offered; nothing can expire.');
  return findings.some(finding => finding.state !== 'current') ? 1 : 0;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main(process.argv.slice(2)).then(code => { process.exitCode = code; }, error => {
    console.error(`::error::Mobile distribution check failed: ${error.message}`);
    process.exitCode = 1;
  });
}
