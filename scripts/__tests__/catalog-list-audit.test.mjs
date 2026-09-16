import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

async function readReconciliation() {
  const ledger = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/event-catalog-retirements.json'), 'utf8'));
  const active = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/catalog-list-decisions.json'), 'utf8')).decisions;
  return { ledger, active };
}

test('event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
  const { ledger, active } = await readReconciliation();
  assert.equal(ledger.schemaVersion, 1);
  assert.equal(ledger.retirements.length, 9);
  assert.match(ledger.parentRevision, /^[a-f0-9]{40}$/);
  assert.match(ledger.currentMobileRevision, /^[a-f0-9]{40}$/);
  assert.equal(new Set(active.map(entry => entry.id)).size, active.length);
  assert.equal(new Set(ledger.retirements.map(entry => entry.originalDecision.id)).size, 9);
  assert.equal(new Set(ledger.retirements.map(entry => entry.replacementId)).size, 9);
  for (const entry of ledger.retirements) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'),
      entry.originalDecisionSha256, `historical decision changed: ${entry.name}`);
    assert.match(entry.historicalRootRevision, /^[a-f0-9]{40}$/);
    assert.match(entry.historicalSourceRevision, /^[a-f0-9]{40}$/);
    assert.notEqual(entry.originalDecision.id, entry.replacementId);
    assert.ok(!active.some(item => item.id === entry.originalDecision.id));
    const replacement = active.find(item => item.id === entry.replacementId);
    assert.ok(replacement, `missing successor for ${entry.name}`);
    assert.equal(replacement.reviewed, true);
    assert.ok(replacement.justification.length >= 80);
    assert.ok(replacement.risk.length >= 24);
    assert.ok(entry.change.length >= 24);
  }
});

test('reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active } = await readReconciliation();
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-reconciliation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');
  const decisionsPath = path.join(tempRoot, 'decisions.json');
  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    for (const file of new Set(ledger.retirements.map(entry => entry.file))) {
      await writeFile(repoDir, file, await fs.readFile(path.join(workspaceRoot, file), 'utf8'));
      await execFileAsync('git', ['add', '--', file], { cwd: repoDir });
    }
    const current = ledger.retirements.map(entry => active.find(item => item.id === entry.replacementId));
    await fs.writeFile(decisionsPath, JSON.stringify(current));
    const args = [auditScriptPath, '--root', repoDir, '--decisions', decisionsPath, '--output', reportPath];
    await execFileAsync(process.execPath, args, { cwd: repoDir });
    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    for (const entry of ledger.retirements) {
      const candidate = report.candidates.find(item => item.id === entry.replacementId);
      assert.ok(candidate, `source drift needs a new review: ${entry.file} ${entry.name}`);
      assert.equal(candidate.file, entry.file);
      assert.equal(candidate.kind, entry.kind);
      assert.equal(candidate.name, entry.name);
      assert.equal(candidate.values.length, entry.newValueCount);
      assert.equal(candidate.decision, current.find(item => item.id === candidate.id).classification);
      assert.ok(!report.candidates.some(item => item.id === entry.originalDecision.id));
    }
    // The ledger must never function as a stale-ID waiver or runtime approval input.
    await fs.writeFile(decisionsPath, JSON.stringify(ledger.retirements.map(entry => entry.originalDecision)));
    await assert.rejects(execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repoDir }),
      error => error.code === 1 && /9 stale decision\(s\)/.test(error.stderr));
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

async function writeFile(repoDir, filePath, content) {
  const fullPath = path.join(repoDir, filePath);
  await fs.mkdir(path.dirname(fullPath), { recursive: true });
  await fs.writeFile(fullPath, content, 'utf8');
}

test('catalog audit excludes ignored local source files from candidate fingerprints', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-list-audit-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');

  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, '.gitignore', '*.env\n');
    await writeFile(
      repoDir,
      'scripts/tracked.mjs',
      "export const STATUS_OPTIONS = ['active', 'inactive'];\n",
    );
    await writeFile(repoDir, 'scripts/local.env', 'SUPPORTED_LOCALES=en,es\n');
    await execFileAsync('git', ['add', '.gitignore', 'scripts/tracked.mjs'], { cwd: repoDir });

    await execFileAsync(
      process.execPath,
      [auditScriptPath, '--root', repoDir, '--output', reportPath],
      { cwd: repoDir },
    );

    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    assert.deepEqual(
      report.candidates.map(({ file, name }) => ({ file, name })),
      [{ file: 'scripts/tracked.mjs', name: 'STATUS_OPTIONS' }],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});
