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
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

async function readReconciliation() {
  const ledger = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/event-catalog-retirements.json'), 'utf8'));
  const active = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/catalog-list-decisions.json'), 'utf8')).decisions;
  const successors = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/event-catalog-current-successors.json'), 'utf8')).successors;
  const integration = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/event-integration-retirements.json'), 'utf8'));
  return { ledger, active, successors, integration };
}

test('music/main reconciliation retains both reviews and the protected migration prefix', async () => {
  const ledger = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/music-main-reconciliation.json'), 'utf8'));
  const { active, successors, integration } = await readReconciliation();
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value,
    (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
      ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(ledger.sharedDecisionConflicts.length, 51);
  assert.equal(new Set(ledger.sharedDecisionConflicts.map(entry => entry.id)).size, 51);
  let preservedReviews = 0;
  for (const entry of ledger.sharedDecisionConflicts) {
    assert.deepEqual(entry.sources.map(source => source.sourceRevision),
      [ledger.musicParent, ledger.mainParent]);
    for (const source of entry.sources) {
      assert.equal(source.sourceEntry.id, entry.id);
      assert.match(source.sourceFileSha256, /^[a-f0-9]{64}$/);
      assert.match(source.sourceEntryCanonicalSha256, /^[a-f0-9]{64}$/);
    }
    let decision = active.find(item => item.id === entry.id);
    if (!decision) {
      // An intentionally extended registry retires its old fingerprint. Require
      // both the explicit current-source mapping and the complete prior review;
      // retaining a stale active approval would contradict the strict audit.
      const history = (value, parentKey) => {
        const chain = []; const seen = new Set();
        while (value) {
          assert.ok(!seen.has(value), 'review lineage must be acyclic');
          seen.add(value); chain.push(value); value = value[parentKey];
        }
        return chain;
      };
      const successor = active.find(item => history(item, 'previousDecision').some(prior => prior.id === entry.id));
      assert.equal(successor?.reviewed, true);
      const mapping = successors.find(item => item.currentId === successor.id);
      assert.ok(mapping, 'every historical approval needs an explicit current-source mapping');
      assert.ok(history(mapping, 'previousResolution').some(prior => prior.currentId === entry.id),
        'review lineage must match the source-resolution lineage');
      decision = history(successor, 'previousDecision').find(prior => prior.id === entry.id);
    }
    assert.equal(decision?.reviewed, true);
    if (entry.preservedReviewCanonicalSha256) {
      assert.equal(canonicalHash(decision), entry.preservedReviewCanonicalSha256);
      assert.equal(decision.reviewMethod, 'successor-of-reviewed-decisions');
      preservedReviews += 1;
    } else {
      assert.deepEqual(entry.differingFields, ['evidence']);
    }
  }
  assert.equal(preservedReviews, 10);
  const manifest = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'scripts/production-migrations.json'), 'utf8')).migrations;
  assert.ok(manifest.length >= 208);
  assert.equal(canonicalHash(manifest.slice(0, 206)), ledger.protectedMainManifestCanonicalSha256);
  assert.equal(new Set(manifest.map(entry => entry.id)).size, manifest.length);
  for (const [index, migration] of ledger.musicMigrations.entries()) {
    const { sqlSha256, introducedSqlSha256, preservedFromRevision,
      historicalContentChangedBeforeIntegration, ...entry } = migration;
    assert.deepEqual(manifest[206 + index], entry);
    assert.equal(preservedFromRevision, ledger.musicParent);
    assert.match(introducedSqlSha256, /^[a-f0-9]{64}$/);
    assert.equal(historicalContentChangedBeforeIntegration, introducedSqlSha256 !== sqlSha256);
    assert.equal(createHash('sha256').update(await fs.readFile(path.join(workspaceRoot,
      entry.path))).digest('hex'), sqlSha256);
  }
  const currentId = successors.find(entry => entry.name === 'migrations').currentId;
  const current = active.find(entry => entry.id === currentId);
  if (currentId !== ledger.currentManifestDecisionId) {
    const extension = JSON.parse(await fs.readFile(path.join(workspaceRoot,
      'docs/catalog-persistence/public-ux-main-reconciliation-20261021.json'), 'utf8'));
    assert.ok(extension.retiredManifestDecisions.some(entry =>
      entry.id === ledger.currentManifestDecisionId && entry.source.sourceRevision === extension.mainParent));
    assert.equal(canonicalHash(manifest.slice(0, 208)), extension.protectedMainManifestCanonicalSha256);
  }
  assert.equal(current?.reviewed, true);
  assert.equal(successors.find(entry => entry.name === 'migrations').currentId, current.id);
  assert.equal(integration.retirements.find(entry => entry.source.name === 'migrations').replacementId,
    current.id);
  for (const entry of ledger.retiredManifestDecisions) {
    assert.ok(!active.some(item => item.id === entry.id));
    assert.match(entry.source.sourceEntryCanonicalSha256, /^[a-f0-9]{64}$/);
  }
});

test('event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
  const { ledger, active, successors } = await readReconciliation();
  assert.equal(ledger.schemaVersion, 1);
  assert.equal(ledger.retirements.length, 9);
  assert.match(ledger.parentRevision, /^[a-f0-9]{40}$/);
  assert.match(ledger.currentMobileRevision, /^[a-f0-9]{40}$/);
  assert.equal(new Set(active.map(entry => entry.id)).size, active.length);
  assert.equal(new Set(ledger.retirements.map(entry => entry.originalDecision.id)).size, 9);
  assert.equal(new Set(ledger.retirements.map(entry => entry.replacementId)).size, 9);
  assert.equal(successors.length, ledger.retirements.length);
  for (const entry of ledger.retirements) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'),
      entry.originalDecisionSha256, `historical decision changed: ${entry.name}`);
    assert.match(entry.historicalRootRevision, /^[a-f0-9]{40}$/);
    assert.match(entry.historicalSourceRevision, /^[a-f0-9]{40}$/);
    assert.notEqual(entry.originalDecision.id, entry.replacementId);
    const successor = successors.find(item => item.historicalReplacementId === entry.replacementId);
    assert.ok(successor, `missing current review mapping: ${entry.name}`);
    assert.deepEqual([successor.file, successor.kind, successor.name], [entry.file, entry.kind, entry.name]);
    // A fingerprint can recur after a later intentional main-branch contract change.
    // It is active only if explicitly mapped to a currently reviewed source below.
    if (entry.originalDecision.id !== successor.currentId) {
      assert.ok(!active.some(item => item.id === entry.originalDecision.id));
    }
    const replacement = active.find(item => item.id === successor.currentId);
    assert.ok(replacement, `missing successor for ${entry.name}`);
    assert.equal(replacement.reviewed, true);
    assert.ok(replacement.justification.length >= 80);
    assert.ok(replacement.risk.length >= 24);
    assert.ok(entry.change.length >= 24);
  }
});

test('reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active, successors } = await readReconciliation();
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
    const current = successors.map(entry => active.find(item => item.id === entry.currentId));
    await fs.writeFile(decisionsPath, JSON.stringify(current));
    const args = [auditScriptPath, '--root', repoDir, '--decisions', decisionsPath, '--output', reportPath];
    await execFileAsync(process.execPath, args, { cwd: repoDir });
    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    for (const entry of successors) {
      const candidate = report.candidates.find(item => item.id === entry.currentId);
      assert.ok(candidate, `source drift needs a new review: ${entry.file} ${entry.name}`);
      assert.equal(candidate.file, entry.file);
      assert.equal(candidate.kind, entry.kind);
      assert.equal(candidate.name, entry.name);
      assert.equal(candidate.values.length, entry.valueCount);
      assert.equal(candidate.decision, current.find(item => item.id === candidate.id).classification);
    }
    // The ledger must never function as a stale-ID waiver or runtime approval input.
    const retired = ledger.retirements.map(entry => entry.originalDecision);
    const staleCount = retired.filter(entry => !report.candidates.some(item => item.id === entry.id)).length;
    assert.equal(staleCount, 8, 'only the explicitly reviewed onboarding fingerprint may recur');
    await fs.writeFile(decisionsPath, JSON.stringify(retired));
    await assert.rejects(execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repoDir }),
      error => error.code === 1 && error.stderr.includes(`${staleCount} stale decision(s)`));
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('main integration archives exact old decisions and retains reviewed successors or source-removal evidence', async () => {
  const { active, integration } = await readReconciliation();
  assert.equal(integration.retirements.length, 11);
  for (const entry of integration.retirements) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'), entry.originalDecisionSha256);
    assert.ok(!active.some(item => item.id === entry.originalDecision.id));
    assert.ok(entry.reason.length > 80);
    if (entry.replacementId) {
      assert.equal(active.find(item => item.id === entry.replacementId)?.reviewed, true);
    } else {
      const source = await fs.readFile(path.join(workspaceRoot, entry.source.file), 'utf8');
      if (entry.source.name === 'command') {
        assert.doesNotMatch(source, /switch\s*\(command\)/);
        assert.match(source, /export async function runLifecycle/);
        assert.match(source, /--check.*--setup.*--refresh/);
      } else {
        assert.equal(entry.source.name, 'OnboardingIntent');
        assert.doesNotMatch(source, /type OnboardingIntent\s*=/);
        assert.match(source, /export type \{ OnboardingIntent \} from '\.\.\/api\/onboarding'/);
      }
    }
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

test('decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-decision-builder-test-'));
  const inventoryPath = path.join(tempRoot, 'inventory.json');
  const existingPath = path.join(tempRoot, 'existing.json');
  const outputPath = path.join(tempRoot, 'decisions.json');
  const preserved = {
    id: 'existing-id',
    classification: 'genuine-technical-constant',
    disposition: 'retain-in-code',
    specializedModel: 'technical_constant_allowlist',
    priority: 'P3-retain',
    risk: 'Existing reviewed risk remains unchanged.',
    justification: 'Existing reviewed justification remains unchanged.',
    reviewed: true,
  };
  const historicalRetirements = { sourceMain: 'immutable-source', decisions: [preserved] };
  const candidate = (overrides) => ({
    id: 'new-id',
    file: 'scripts/artist-enrichment.mjs',
    line: 10,
    kind: 'object-registry',
    name: 'delayOptions',
    values: ['baseMs', 'jitterRatio'],
    valueCount: 2,
    sourceKind: 'production',
    surface: 'automation',
    domain: 'cross-cutting-or-technical',
    consumerCount: 1,
    exactDuplicateIds: [],
    similarCandidateIds: [],
    ...overrides,
  });

  try {
    await fs.writeFile(inventoryPath, JSON.stringify({
      candidates: [
        candidate({ id: preserved.id, name: 'preserved' }),
        candidate({}),
        candidate({
          id: 'builder-options-id',
          file: 'scripts/build-catalog-decisions.mjs',
          name: 'options',
          values: ['input', 'output', 'existing', 'reviewBatch'],
        }),
        candidate({
          id: 'onboarding-id',
          file: 'tdf-hq/docs/openapi/api.yaml',
          kind: 'openapi-enum',
          name: 'firstValue',
          values: ['artist_followed', 'access_requested'],
        }),
      ],
    }));
    await fs.writeFile(existingPath, JSON.stringify({
      schemaVersion: 1,
      reconciliation: {
        preservedDecisions: 99,
        ticketMainMergeRetirements: historicalRetirements,
      },
      decisions: [
        preserved,
        { ...preserved, id: 'stale-id' },
        {
          ...preserved,
          id: 'builder-options-id',
          classification: 'dynamic-business-catalog',
          disposition: 'migrate-and-remove-authority-from-code',
          reviewMethod: 'deterministic-policy-v2',
        },
      ],
    }));

    await execFileAsync(process.execPath, [
      decisionBuilderPath,
      '--input', inventoryPath,
      '--output', outputPath,
      '--existing', existingPath,
      '--review-batch', 'unit-test-review',
    ]);

    const report = JSON.parse(await fs.readFile(outputPath, 'utf8'));
    assert.ok(!report.decisions.some(entry => entry.id === 'stale-id'));
    assert.deepEqual(report.decisions[0], preserved);
    assert.equal(report.decisions[1].classification, 'genuine-technical-constant');
    assert.equal(report.decisions[1].reviewBatch, 'unit-test-review');
    assert.equal(report.decisions[1].evidence.file, 'scripts/artist-enrichment.mjs');
    assert.equal(report.decisions[2].classification, 'genuine-technical-constant');
    assert.equal(report.decisions[2].disposition, 'retain-in-code');
    assert.equal(report.decisions[2].reviewMethod, 'deterministic-policy-v3');
    assert.match(report.decisions[2].justification, /CLI execution mechanics/);
    assert.equal(report.decisions[3].classification, 'dynamic-business-catalog');
    assert.equal(report.decisions[3].specializedModel, 'onboarding_intent, onboarding_first_value, user_onboarding_progress');
    assert.deepEqual(report.reconciliation, {
      ticketMainMergeRetirements: historicalRetirements,
      preservedDecisions: 1,
      reviewedByPolicy: 3,
      removedStaleDecisions: 1,
      reviewBatch: 'unit-test-review',
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('ticket main merge retains stale approvals only as provenance', async () => {
  const document = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/catalog-list-decisions.json'), 'utf8'));
  const retired = document.reconciliation.ticketMainMergeRetirements;
  assert.equal(retired.sourceMain, 'b06a4d3926e03a0b72a23732e454a33c72eb9705');
  assert.equal(retired.decisions.length, 3);
  for (const entry of retired.decisions) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'), entry.originalDecisionSha256);
    assert.ok(!document.decisions.some(item => item.id === entry.originalDecision.id));
    if (entry.replacementId) {
      assert.equal(document.decisions.find(item => item.id === entry.replacementId)?.reviewed, true);
    } else {
      assert.equal(entry.originalDecision.id, 'ec1a316784aa9992384a');
      assert.doesNotMatch(await fs.readFile(path.join(workspaceRoot, 'tdf-mobile/app/ddex/index.tsx'), 'utf8'), /const STATUS_LABELS/);
    }
  }
});
