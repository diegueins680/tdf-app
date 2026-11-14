#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { readFileSync, realpathSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { sourceManifest } from './lib/verification-evidence.mjs';
import { buildDeliveryMatrix } from './lib/delivery-matrix.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
let mobile = path.join(root, 'tdf-mobile');
let evidence;
let requireComplete = false;
try {
  while (args.length) {
    const flag = args.shift();
    if (flag === '--require-complete') requireComplete = true;
    else if (['--mobile', '--evidence'].includes(flag)) {
      const value = args.shift();
      if (!value || value.startsWith('--')) throw new Error(`Missing value for ${flag}`);
      if (flag === '--mobile') mobile = path.resolve(value);
      else evidence = path.resolve(value);
    } else throw new Error(`Unknown argument: ${flag}`);
  }
  const git = (cwd, ...argv) => execFileSync('git', argv, { cwd, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
  const before = sourceManifest(root);
  const identity = {
    rootRevision: git(root, 'rev-parse', 'HEAD'), sourceDigest: before.digest,
    rootDirty: git(root, 'status', '--porcelain', '--untracked-files=normal').length > 0,
    mobilePin: /^160000 commit ([a-f0-9]{40})\ttdf-mobile$/.exec(git(root, 'ls-tree', 'HEAD', 'tdf-mobile'))?.[1] ?? null,
    mobileCheckoutRevision: null, mobileDirty: null,
  };
  try {
    if (realpathSync(git(mobile, 'rev-parse', '--show-toplevel')) !== realpathSync(mobile)) throw new Error('Mobile is not a checkout');
    identity.mobileCheckoutRevision = git(mobile, 'rev-parse', 'HEAD');
    identity.mobileDirty = git(mobile, 'status', '--porcelain', '--untracked-files=normal').length > 0;
  } catch { /* Missing Mobile is explicit incomplete evidence, never a pass. */ }
  const input = evidence ? JSON.parse(readFileSync(evidence, 'utf8')) : { schemaVersion: 1, receipts: {} };
  if (input.schemaVersion !== 1 || !input.receipts || Array.isArray(input.receipts) || typeof input.receipts !== 'object') {
    throw new Error('Evidence must be an object with schemaVersion=1 and receipts keyed by check ID.');
  }
  if (sourceManifest(root).digest !== before.digest || git(root, 'rev-parse', 'HEAD') !== identity.rootRevision) {
    throw new Error('Source changed during collection.');
  }
  const report = buildDeliveryMatrix(identity, input.receipts);
  console.log(JSON.stringify(report, null, 2));
  if (requireComplete && report.status !== 'RECEIPTS_CONSISTENT') process.exitCode = 2;
} catch (error) {
  console.error(JSON.stringify({ status: 'EXECUTION_FAILED', releaseAuthorized: false, error: error.message }));
  process.exitCode = 1;
}
