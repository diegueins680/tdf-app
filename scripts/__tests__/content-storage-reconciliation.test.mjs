import { createHash } from 'node:crypto';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import { test } from 'node:test';
import { prepareStorageReconciliation } from '../prepare-content-storage-reconciliation.mjs';

test('binds a non-executing plan to exact fresh evidence and exact integers', async () => {
  const root = await mkdtemp(path.join(os.tmpdir(), 'tdf-storage-plan-'));
  try {
    const evidence = 'synthetic ownership and byte inventory';
    await writeFile(path.join(root, 'evidence.txt'), evidence);
    const input = { observedAt: new Date().toISOString(), accounts: [{ partyId: '12', legacyBytes: '5000000000', expectedRevision: '0', ownershipVerified: true, sizesVerified: true, evidenceFile: 'evidence.txt', evidenceSha256: createHash('sha256').update(evidence).digest('hex') }] };
    const target = path.join(root, 'input.json');
    await writeFile(target, JSON.stringify(input));
    const plan = await prepareStorageReconciliation(target);
    assert.equal(plan.executionAllowed, false);
    assert.match(plan.sql, /12::bigint,5000000000::bigint/);
    await writeFile(path.join(root, 'evidence.txt'), 'changed');
    await assert.rejects(prepareStorageReconciliation(target), /Evidence bytes changed/);
    await writeFile(path.join(root, 'evidence.txt'), evidence);
    input.accounts[0].legacyBytes = '0); DROP TABLE party; --';
    await writeFile(target, JSON.stringify(input));
    await assert.rejects(prepareStorageReconciliation(target), /Invalid inventory integer/);
    assert.equal(await readFile(path.join(root, 'evidence.txt'), 'utf8'), evidence);
  } finally { await rm(root, { recursive: true, force: true }); }
});
