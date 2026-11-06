import { createHash } from 'node:crypto';
import { readFile, realpath, stat, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const maximum = 9223372036854775807n;
function integer(value, positive = false) {
  if (typeof value === 'number' && !Number.isSafeInteger(value)) throw new Error('Unsafe numeric inventory value');
  const text = String(value);
  if (!/^(0|[1-9][0-9]*)$/.test(text) || BigInt(text) > maximum || (positive && BigInt(text) === 0n)) throw new Error('Invalid inventory integer');
  return text;
}

/** Prepare a review artifact only; this function never connects to a database. */
export async function prepareStorageReconciliation(inputPath, now = Date.now()) {
  const absolute = await realpath(inputPath), base = path.dirname(absolute);
  const raw = await readFile(absolute), input = JSON.parse(raw.toString('utf8'));
  const observed = Date.parse(input.observedAt);
  if (!Number.isFinite(observed) || observed > now + 15_000 || now - observed > 15 * 60_000) throw new Error('Inventory is not fresh');
  if (!Array.isArray(input.accounts) || input.accounts.length < 1 || input.accounts.length > 100) throw new Error('Inventory requires 1 to 100 reviewed accounts');
  const seen = new Set(), entries = [];
  for (const account of input.accounts) {
    const partyId = integer(account.partyId, true), legacyBytes = integer(account.legacyBytes), revision = integer(account.expectedRevision);
    if (seen.has(partyId) || account.ownershipVerified !== true || account.sizesVerified !== true) throw new Error('Unresolved or duplicate inventory account');
    seen.add(partyId);
    if (typeof account.evidenceFile !== 'string' || path.isAbsolute(account.evidenceFile)) throw new Error('Evidence must be a relative file');
    const evidence = await realpath(path.resolve(base, account.evidenceFile));
    if (!evidence.startsWith(base + path.sep) || !(await stat(evidence)).isFile() || (await stat(evidence)).size > 50 * 1024 * 1024) throw new Error('Evidence is outside the bounded inventory folder');
    const digest = createHash('sha256').update(await readFile(evidence)).digest('hex');
    if (digest !== account.evidenceSha256) throw new Error('Evidence bytes changed');
    entries.push({ partyId, legacyBytes, expectedRevision: revision, evidenceSha256: digest });
  }
  return {
    schemaVersion: 1, executionAllowed: false, observedAt: input.observedAt,
    manifestSha256: createHash('sha256').update(raw).digest('hex'), entries,
    requires: 'Reviewed ownership/size evidence, current revision and canonical release-writer admission; supplied verification flags are assertions, not independently established facts.',
    sql: entries.map((entry) => `SELECT content_storage_reconcile(${entry.partyId}::bigint,${entry.legacyBytes}::bigint,'${entry.evidenceSha256}',${entry.expectedRevision}::bigint);`).join('\n'),
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [input, output] = process.argv.slice(2);
  if (!input || !output) throw new Error('Usage: node scripts/prepare-content-storage-reconciliation.mjs INPUT_JSON NEW_PRIVATE_PLAN_JSON');
  const plan = await prepareStorageReconciliation(input);
  await writeFile(output, JSON.stringify(plan, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  console.log(JSON.stringify({ accounts: plan.entries.length, executionAllowed: false, manifestSha256: plan.manifestSha256 }));
}
