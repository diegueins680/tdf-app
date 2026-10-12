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

// One finding per platform that links to a store: 'current', 'expiring' or 'expired'.
export function distributionExpiry(manifest, now, warningHours = DEFAULT_WARNING_HOURS) {
  if (!manifest || typeof manifest !== 'object') throw new Error('Invalid mobile distribution');
  if (!Number.isFinite(now) || !(warningHours > 0)) throw new Error('Invalid check time or warning window');
  const findings = [];
  for (const platform of ['ios', 'android']) {
    const channel = manifest[platform];
    if (!channel || typeof channel !== 'object') throw new Error(`Missing ${platform} channel`);
    const validUntil = Date.parse(channel.validUntil);
    if (typeof channel.validUntil !== 'string' || !Number.isFinite(validUntil)) throw new Error(`Invalid ${platform} validUntil`);
    if (!channel.url) continue; // Nothing is offered, so nothing can lapse.
    const hoursLeft = (validUntil - now) / HOUR;
    const state = hoursLeft <= 0 ? 'expired' : hoursLeft < warningHours ? 'expiring' : 'current';
    findings.push({ platform, status: String(channel.status), validUntil: channel.validUntil, hoursLeft, state });
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
  if (!findings.length) console.log('No store channel is offered; nothing can expire.');
  return findings.some(finding => finding.state !== 'current') ? 1 : 0;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main(process.argv.slice(2)).then(code => { process.exitCode = code; }, error => {
    console.error(`::error::Mobile distribution check failed: ${error.message}`);
    process.exitCode = 1;
  });
}
