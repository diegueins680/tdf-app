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

test('memory226 and ledger succession preserve213/214/220/221/222/224/225 reviews and reject stale approvals', async () => {
  const document = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'docs/catalog-persistence/catalog-list-decisions.json'), 'utf8'));
  const transition = document.reconciliation.directorySavedSearchParity;
  const financial = document.reconciliation.directoryFinancialMainIntegration;
  const { active, successors, integration } = await readReconciliation();
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value,
    (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
      ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  // Immutable protected-main cfe75b source, independently hashed before this
  // extension. Matching a recorded ID alone cannot preserve the prior review.
  const previousDecisionHash = '97749e12750374a44787b17c6db4dc82f01f25c5ca1d8f716703a545c809b5a0';
  const protectedPrefixHash = '7b3616b6095436eb7eb46038d6ccaa289ff4cb22e602960c06d971bf915add86';
  const previous = transition.decisions.find(entry => entry.id === '3f336173e03f10fc023c');
  assert.equal(canonicalHash(previous), previousDecisionHash);
  assert.equal(transition.previousDecisionCanonicalSha256, previousDecisionHash);
  assert.equal(previous.evidence.valueCount, 213);
  assert.equal(previous.reviewed, true);
  assert.ok(!active.some(entry => entry.id === previous.id));
  const manifest = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    'scripts/production-migrations.json'), 'utf8')).migrations;
  assert.equal(canonicalHash(manifest.slice(0, 213)), protectedPrefixHash);
  assert.equal(transition.protectedMainManifestCanonicalSha256, protectedPrefixHash);
  const protectedFinancialPrefixHash = 'c025cf3d2ab92f64bfb92d3656384d7fe0da6919457440d3c24301d7f6b8f232';
  assert.equal(canonicalHash(manifest.slice(0, 220)), protectedFinancialPrefixHash);
  assert.equal(financial.protectedMainManifestCanonicalSha256, protectedFinancialPrefixHash);
  assert.equal(financial.sourceMain, '5114fe95ac8e328b58162a8dcae5c9015a47ce09');
  assert.equal(manifest.length, 226);
  assert.equal(canonicalHash(manifest.slice(0, 222)),
    '3de55b856b5d3d21671a9449be31f6169c93f004946a0e0752b784b4d5a0922f',
    'Preserve the full222-entry protected main78fa351b prefix');
  assert.deepEqual(manifest[221], {
    id: '2026-10-25_ledger_posted_header',
    path: 'tdf-hq/sql/2026-10-25_ledger_posted_header.sql',
    introducedBy: '5cc26ad46c958a029ec4539c60cc1ca77fec1d01',
  });
  assert.equal(canonicalHash(manifest.slice(0, 221)),
    '1413652c0d640dbb2241fb5c004eaa212245a62dfcb031109f54226d3af31fd9',
    'Preserve every object of protected main1c9c833, including its directory successor');
  assert.equal(canonicalHash(manifest.slice(222)),
    '683f89261ab16dca098411d90b94317800bdeb0aa3508b4eced4985b53c8b22b',
    'Append only the four unchanged memory entries from reviewed candidate31c16bffe');
  assert.deepEqual(manifest[220], {
    id: '2026-10-25_directory_saved_search_parity',
    path: 'tdf-hq/sql/2026-10-25_directory_saved_search_parity.sql',
    introducedBy: 'eb2f71d62740783369a6eb2a6d35fc024484cb71',
  });
  const decision214 = financial.decisions.find(entry => entry.id === 'c94cd70c1272ec9ede1b');
  const decision220 = financial.decisions.find(entry => entry.id === 'b7cc47848f3f9882770f');
  const historical = [previous, decision214, decision220];
  const expectedHashes = [previousDecisionHash,
    '409d5381c7051905c3e6b6ad7acd2e151c2d2e6787481ed0ab45d3be987b1fd4',
    '8679ac1843933f3f9c70ef822396c2ca0661b09eb6c9d30f07d94212039eee28'];
  for (const [index, decision] of historical.entries()) {
    assert.equal(canonicalHash(decision), expectedHashes[index]);
    assert.equal(financial.preservedDecisionCanonicalSha256[decision.id], expectedHashes[index]);
    assert.equal(decision.reviewed, true);
    assert.ok(!active.some(entry => entry.id === decision.id));
  }
  assert.equal(transition.successorDecisionId, decision214.id, 'Keep the original213-to214 review intact');
  assert.equal(decision214.evidence.valueCount, 214);
  assert.equal(decision220.evidence.valueCount, 220);
  const pointer = successors.find(entry => entry.name === 'migrations');
  const finalCurrent = active.find(entry => entry.id === pointer.currentId);
  assert.equal(finalCurrent?.reviewed, true);
  const history = []; const seen = new Set();
  const visit = value => {
    if (!value) return;
    assert.ok(!seen.has(value), 'review graph must be acyclic'); seen.add(value); history.push(value);
    visit(value.previousDecision);
    for (const branch of value.concurrentReviewHistory ?? []) visit(branch);
  };
  visit(finalCurrent);
  const current = history.find(entry => entry.id === financial.currentDecisionId);
  assert.equal(current?.id, '8229b8202552298c955e');
  assert.equal(canonicalHash(current),
    '12f80527fcf53a3e23afc774396b08573644a371eb91e1cf1b8f854bb049651f',
    'Preserve the exact 221-entry protected-main review, not a reconstructed approval');
  const memoryParent = history.find(entry => entry.id === '79a6def1e91baee6e937');
  assert.equal(canonicalHash(memoryParent),
    '978e4a28b767ba92ef815fe45ac919ae7550742d9fd9b8cc1007e09e4d0291d9',
    'Preserve the entire 224-entry memory/financial review graph');
  assert.equal(memoryParent.reviewed, true);
  assert.equal(memoryParent.evidence.valueCount, 224);
  assert.equal(finalCurrent.classification, current.classification);
  assert.equal(finalCurrent.disposition, current.disposition);
  assert.equal(finalCurrent.evidence.valueCount, 226);
  assert.ok(!active.some(entry => entry.id === current.id || entry.id === memoryParent.id));
  const memory225 = history.find(entry => entry.id === 'cff15f30aa13965e5729');
  const header222 = history.find(entry => entry.id === '6ff395b6131079017c9d');
  assert.equal(canonicalHash(memory225),
    '9f3858a91f37caf5b4b1916fbc280a6ce891cd34536d0ee8953ce2331b8f3fb5');
  assert.equal(canonicalHash(header222),
    '866eef0dde46336e216336f13bdf9f83570314d2c17b9cf93f354de0f8ad9404');
  assert.equal(memory225.evidence.valueCount, 225);
  assert.equal(header222.evidence.valueCount, 222);
  assert.equal(memory225.reviewed, true);
  assert.equal(header222.reviewed, true);
  const header = document.reconciliation.ledgerHeaderMainIntegration;
  const combined = JSON.parse(await fs.readFile(path.join(workspaceRoot, header.document), 'utf8'));
  assert.equal(header.currentDecisionId, header222.id);
  assert.equal(current.id, financial.currentDecisionId);
  assert.equal(header222.classification, current.classification);
  assert.equal(header222.disposition, current.disposition);
  assert.equal(combined.mainParent, 'e938c72ba4b87010214f6838f06bfac45a0f45c0');
  assert.equal(combined.mainDecisionCanonicalSha256, canonicalHash(current));
  assert.equal(combined.headerDecisionCanonicalSha256, canonicalHash(combined.headerManifestDecision));
  assert.ok(!active.some(entry => [current.id, combined.headerManifestDecision.id, memory225.id, header222.id].includes(entry.id)));
  const retiredParents = [...historical, current, memoryParent, memory225, header222, combined.headerManifestDecision];
  assert.equal(current.classification, decision220.classification);
  assert.equal(current.disposition, decision220.disposition);
  assert.deepEqual(current.sourceReviewReferences.slice(0, decision220.sourceReviewReferences.length),
    decision220.sourceReviewReferences);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(current.evidence.valueCount, 221);
  assert.equal(pointer.currentId, finalCurrent.id);
  assert.equal(pointer.valueCount, 226);
  assert.equal(pointer.change, finalCurrent.justification);
  const retirement = integration.retirements.find(entry => entry.source.name === 'migrations');
  assert.equal(retirement.replacementId, finalCurrent.id);
  assert.equal(retirement.currentResolution.id, finalCurrent.id);
  assert.equal(retirement.currentResolution.valueCount, 226);
  assert.equal(retirement.reason, finalCurrent.justification);
  for (const decision of retiredParents) {
    assert.ok(pointer.previousCurrentIds.includes(decision.id));
    assert.ok(retirement.previousReplacementIds.includes(decision.id));
  }

  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-alert-successor-test-'));
  try {
    const repoDir = path.join(tempRoot, 'repo');
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, 'scripts/production-migrations.json', JSON.stringify({ migrations: manifest }));
    await execFileAsync('git', ['add', '--', 'scripts/production-migrations.json'], { cwd: repoDir });
    const decisionsPath = path.join(tempRoot, 'decisions.json');
    const args = [auditScriptPath, '--root', repoDir, '--decisions', decisionsPath, '--fail-on-unreviewed'];
    await fs.writeFile(decisionsPath, JSON.stringify([finalCurrent]));
    await execFileAsync(process.execPath, args, { cwd: repoDir });
    for (const decision of retiredParents) {
      await fs.writeFile(decisionsPath, JSON.stringify([decision]));
      await assert.rejects(execFileAsync(process.execPath, args, { cwd: repoDir }),
        error => error.code === 1 && error.stderr.includes('1 stale decision(s)')
          && error.stderr.includes('1 unreviewed candidate(s)'));
    }
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
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
