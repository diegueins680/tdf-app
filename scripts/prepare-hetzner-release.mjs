#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadMigrationContract, sha256 } from './lib/migration-contract.mjs';
import { prepareHetznerRelease } from './lib/hetzner-preparation.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8',
  maxBuffer: 16 * 1024 * 1024, stdio: ['ignore', 'pipe', 'pipe'] });
const readBlob = (sha, name) => git('show', `${sha}:${name}`);
function isAncestor(ancestor, target) {
  try { git('merge-base', '--is-ancestor', ancestor, target); return true; }
  catch (error) { if (error.status === 1) return false; throw error; }
}

try {
  if (process.argv.length !== 6 || process.argv[2] !== '--runtime' || process.argv[4] !== '--output') {
    throw new Error('Usage: node scripts/prepare-hetzner-release.mjs --runtime RECENT_JSON --output NEW_JSON');
  }
  if (git('status', '--porcelain').trim()) throw new Error('Preparation requires a clean source checkout');
  const sourceRevision = git('rev-parse', 'HEAD').trim();
  const runtimeBytes = readFileSync(path.resolve(process.argv[3]));
  const receipt = JSON.parse(runtimeBytes);
  if (typeof receipt.sourceRevision !== 'string' || !/^[a-f0-9]{40}$/.test(receipt.sourceRevision)
      || !isAncestor(receipt.sourceRevision, sourceRevision)) {
    throw new Error('Runtime inspection source is not in candidate ancestry');
  }
  const provenance = {
    inspectorSha256: sha256(readBlob(sourceRevision, 'ops/hetzner/inspect-runtime.py')),
    launcherSha256: sha256(readBlob(sourceRevision, 'scripts/inspect-hetzner-production.mjs')),
  };
  const contract = await loadMigrationContract(sourceRevision, readBlob, isAncestor);
  const plan = prepareHetznerRelease({ contract, receipt, provenance });
  if (git('rev-parse', 'HEAD').trim() !== sourceRevision || git('status', '--porcelain').trim()) {
    throw new Error('Preparation source changed during execution');
  }
  const output = path.resolve(process.argv[5]);
  writeFileSync(output, JSON.stringify({ ...plan, preparedAt: new Date().toISOString(),
    runtimeReceiptSha256: sha256(runtimeBytes),
    preparationSourceSha256: sha256(readFileSync(fileURLToPath(import.meta.url))),
  }, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  console.log(JSON.stringify({ output, sourceRevision, ...plan.counts, executionAllowed: false }));
} catch (error) {
  // Git or malformed input may contain private paths/data. Print only our own
  // validation messages; never child-process stdout/stderr or receipt contents.
  const safe = error?.status === undefined && error?.code === undefined && !(error instanceof SyntaxError);
  console.error(safe ? error.message : 'Release preparation failed; no production operation was attempted.');
  process.exitCode = 1;
}
