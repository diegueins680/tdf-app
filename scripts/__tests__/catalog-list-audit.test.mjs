import assert from 'node:assert/strict';
import { execFile, execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import { isDeepStrictEqual, promisify } from 'node:util';
import { DECISIONS_FILE, expandAtRevision, readExpandedDecisions } from '../lib/catalog-decision-history.mjs';

// Complete retained parent assertions; dated inputs are explicitly projected.
{

// Complete dated parent assertions, reconstructed only from retained whole reviews.
{

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

// 2026-11-02 registry refresh: the only field ever added inside an already
// reviewed manifest prefix is the compatible applied checksum of the
// regenerated 2026-09-18 identity registry. Historical prefix hashes are taken
// over the manifest as it was reviewed, i.e. without that field.
const REGISTRY_REFRESH_ENTRY = '2026-09-18_identity_review_dependencies';
// 2026-11-02 session-expiry integration: protected main's migration is
// interleaved at index 227, ahead of the seven audit migrations. The audit's
// earlier reviews are verified over the manifest without that entry.
const SESSION_EXPIRY_ENTRY = '2026-10-31_api_token_session_expiry';
const APPLE_POLICY_ENTRY = '2026-10-13_apple_sign_in_policy';
const APPLE_ISSUER_ENTRY = '2026-11-07_apple_provider_identity_issuer';
// 2026-11-08 marketplace offers (ADR 0134) append after the Apple issuer entry.
const MARKETPLACE_OFFERS_ENTRY = '2026-11-08_marketplace_offers';
// 2026-11-10 auth rate limit (owner decisions of the formal audit) appends last.
const AUTH_RATE_LIMIT_ENTRY = '2026-11-10_auth_rate_limit';
// 2026-11-12 marketplace agreements (ADR 0135) append after the auth_rate_limit entry.
const MARKETPLACE_AGREEMENTS_ENTRY = '2026-11-12_marketplace_agreements';
// 2026-11-11 delegated event administrators: one migration appended last to the
// 251-entry manifest of protected main 8ec7d85c. Earlier reviews are verified
// over the manifest without that entry.
const EVENT_ADMIN_ENTRY = '2026-11-11_event_admin_delegation';
const EVENT_ADMIN_PROTECTED_MAIN = '8ec7d85c672e844e28c38e49c087b102032582d8';
const EVENT_ADMIN_INTRODUCTION = '2703eb3a1ac1604829e902262fe2dbfbbf4f4ca8';
// The marketplace agreement payments migration (ADR 0136) appends one entry to
// the 252-entry manifest of protected main d4bb9603. Earlier reviews are
// verified with the records stepped back to the main they reviewed.
const MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY = '2026-11-12_marketplace_agreement_payments';
const AGREEMENT_PAYMENTS_PROTECTED_MAIN = 'd4bb96030b1182e165ff39b614315c6a2554aee7';
const AGREEMENT_PAYMENTS_INTRODUCTION = '26c83d9e6525d859b0da0bebf959ad6d1c806159';

async function reviewedJson(revision, file) {
  const { stdout } = await execFileAsync('git', ['show', `${revision}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 });
  return JSON.parse(stdout);
}
const historicalReview = (entry, count) => {
  while (entry && entry.evidence?.valueCount !== count) entry = entry.previousDecision;
  assert.ok(entry, `Missing exact reviewed ${count}-entry predecessor`);
  return entry;
};
const auditView = entries => entries.filter(entry => entry.id !== SESSION_EXPIRY_ENTRY);
const asReviewedBeforeRegistryRefresh = entries => entries.map(entry => {
  if (entry.id !== REGISTRY_REFRESH_ENTRY) return entry;
  const { compatibleAppliedChecksums, ...reviewed } = entry;
  assert.deepEqual(compatibleAppliedChecksums,
    ['5636028805b77a1705bb364c87669e8d46983c93d9fc177da9e2187b98972f4f']);
  return reviewed;
});

const DELIVERY_PROTOCOL_REVIEW = '755cf14ac92e4f590879';
const PLAYER_SERVER_MAIN_BASE = '2ebb5c38bef08f3205877dc9521026b40b68d4ef';
const PLAYER_SERVER_REVIEW_HEAD = '85255658262166ab4aeb0e7e167b3a93c4a33d61';
const manifestPath = 'scripts/production-migrations.json';
const catalogPath = 'docs/catalog-persistence/catalog-list-decisions.json';
const playerMigrations = new Set([
  '2026-10-30_artist_featured_track',
  '2026-11-06_music_media_health',
  '2026-11-06_music_legacy_source_compatibility',
  '2026-11-07_music_featured_source_health',
  '2026-11-07_artist_featured_identity_guard',
]);

async function manifestBeforePlayer() {
  const manifest = (await mainManifestView());
  return { ...manifest, migrations: manifest.migrations.filter(row => !playerMigrations.has(row.id)) };
}

// Lists whose source file moved without any change to their values (ADR 0133).
// A list id follows its file, so each move is recorded as a successor review
// that nests the exact earlier review; checks against an earlier main read the
// successor back to that review.
const MOVED_FILE_REVIEW_BATCHES = new Set([
  'paypal-webhook-domain-module-2026-11-10',
  'web-auth-state-layer-2026-11-17',
  'web-feature-registry-layer-2026-11-17',
]);

function movedFileSuccessors(active) {
  const moved = active.filter(row => MOVED_FILE_REVIEW_BATCHES.has(row.reviewBatch));
  for (const row of moved) {
    assert.equal(row.reviewMethod, 'successor-of-reviewed-decision');
    assert.ok(row.previousDecision?.reviewed, `moved list ${row.id} must nest a reviewed decision`);
    assert.equal(row.classification, row.previousDecision.classification);
    assert.equal(row.disposition, row.previousDecision.disposition);
  }
  return moved;
}

// Every moved list whose earlier review protected main held nests that exact review.
function assertMovedListsNest(active, priorMain) {
  const prior = new Map(priorMain.map(row => [row.id, row]));
  return movedFileSuccessors(active).filter(row => prior.has(row.previousDecision.id)).map(row => {
    assert.deepEqual(row.previousDecision, prior.get(row.previousDecision.id));
    return row.previousDecision.id;
  });
}

// The continuous improvement loop rework (pull request #750) deleted two automation
// scripts: their reviews are retired as provenance and one new list is reviewed.
const LOOP_REWORK_RECONCILIATION = 'continuousImprovementLoopRework20261113';

async function readLoopRework() {
  return JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8')).reconciliation[LOOP_REWORK_RECONCILIATION];
}

async function readHistoricalReconciliation() {
  const current = await readReconciliation();
  const rework = await readLoopRework();
  const added = new Set(rework.addedDecisionIds);
  const active = current.active.filter(row => !added.has(row.id));
  for (const entry of [...rework.decisions].sort((left, right) => left.position - right.position)) {
    active.splice(entry.position, 0, entry.decision);
  }
  return { ...current, active };
}

async function readReconciliation(beforePlayer = false) {
  const ledger = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/event-catalog-retirements.json', 'utf8'));
  const register = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const mainRegister = await reviewedJson(INTEGRATION_MAIN, catalogPath);
  const active = mainRegister.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Dated main review missing: ${row.id}`);
    return found;
  });
  // Reviews whose source list was deleted outright (no successor list). They
  // leave the active register and are kept, unchanged, as history.
  const retired = (register.reconciliation.sourceRemovalRetirements ?? []).flatMap(entry => entry.decisions);
  let successors = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/event-catalog-current-successors.json', 'utf8')).successors;
  let integration = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/event-integration-retirements.json', 'utf8'));
  const parentPointers = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-catalog-current-successors.json');
  successors = parentPointers.successors.map(row => {
    const found = findWholeResolution(successors, row);
    assert.ok(found, `Dated main resolution missing: ${row.name}`);
    return found;
  });
  const parentRetirements = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-integration-retirements.json');
  integration = { ...integration, retirements: parentRetirements.retirements.map(row => {
    const found = findWholeResolution(integration, row);
    assert.ok(found, `Dated main retirement missing: ${row.source.name}`);
    return found;
  }) };
  if (!beforePlayer) return { ledger, active, retired, successors, integration };

  // Earlier reviews retain their own manifest fingerprint and catalog size.
  const predecessor = (await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const tip = active.find(row => row.id === pointer.currentId);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  const byId = new Map(active.map(row => row === tip ? row.previousDecision : row)
    .map(row => [row.id, row]));
  // A moved list's successor stands in for the review it nests.
  for (const row of movedFileSuccessors(active)) byId.set(row.previousDecision.id, row);
  // Web Mis entradas (EVT-MY-TICKETS-001) succeeds the generated mobile registry
  // review; the earlier catalog still names that exact review, nested unchanged.
  for (const row of active.filter(candidate => candidate.reviewBatch === 'web-my-tickets-20261119'
    && candidate.reviewMethod === 'successor-of-reviewed-decision')) {
    byId.set(row.previousDecision.id, row.previousDecision);
  }
  // EVT-TICKET-SALE-001 extended the typed client registry; its successor
  // review nests the 86-method review unchanged and stands in for it.
  const ticketSaleClient = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  byId.set(ticketSaleClient.previousDecision.id, ticketSaleClient);
  // The loop rework (pull request #750) retired two reviews of deleted scripts as
  // provenance; the earlier catalog still lists them, so they stand in for themselves.
  for (const entry of (await readLoopRework()).decisions) byId.set(entry.decision.id, entry.decision);
  return {
    ledger,
    retired,
    active: predecessor.map(row => byId.get(row.id)).filter(Boolean),
    successors: successors.map(row => row === pointer ? row.previousResolution : row),
    integration: { ...integration, retirements: integration.retirements
      .map(row => row === retirement ? row.previousResolution : row) },
  };
}

test('dated-UX-parent: main-parent: music/main reconciliation retains both reviews and the protected migration prefix', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/music-main-reconciliation.json', 'utf8'));
  const { active, successors, integration } = await readReconciliation(true);
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
  const manifest = asReviewedBeforeRegistryRefresh((await mainManifestView()).migrations);
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
    const extension = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/public-ux-main-reconciliation-20261021.json', 'utf8'));
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

test('dated-UX-parent: main-parent: memory226 and ledger succession preserve213/214/220/221/222/224/225 reviews and reject stale approvals', async () => {
  const document = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const transition = document.reconciliation.directorySavedSearchParity;
  const financial = document.reconciliation.directoryFinancialMainIntegration;
  const { active, successors, integration } = await readReconciliation(true);
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
  const storageMerge = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const rawManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references' && !storageMerge.storageMigrationIds.includes(entry.id));
  const manifest = asReviewedBeforeRegistryRefresh(rawManifest);
  assert.equal(canonicalHash(manifest.slice(0, 213)), protectedPrefixHash);
  assert.equal(transition.protectedMainManifestCanonicalSha256, protectedPrefixHash);
  const protectedFinancialPrefixHash = 'c025cf3d2ab92f64bfb92d3656384d7fe0da6919457440d3c24301d7f6b8f232';
  assert.equal(canonicalHash(manifest.slice(0, 220)), protectedFinancialPrefixHash);
  assert.equal(financial.protectedMainManifestCanonicalSha256, protectedFinancialPrefixHash);
  assert.equal(financial.sourceMain, '5114fe95ac8e328b58162a8dcae5c9015a47ce09');
  assert.equal(manifest.length, 239);
  assert.deepEqual(manifest[227], {
    id: SESSION_EXPIRY_ENTRY,
    path: 'tdf-hq/sql/2026-10-31_api_token_session_expiry.sql',
    introducedBy: '8187065f81fad94677c8ba27760ab2cb74f58f79',
  });
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
  assert.equal(canonicalHash(manifest.slice(223, 227)),
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
  const latestCurrent = active.find(entry => entry.id === pointer.currentId);
  const finalCurrent = historicalReview(latestCurrent, 239);
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
  assert.equal(finalCurrent.evidence.valueCount, 239);
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
  const provider = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.providerDomainMainIntegration.document), 'utf8'));
  const aggregate = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.eventMemoryProviderMain20261026.document), 'utf8'));
  const memory226 = history.find(entry => entry.id === 'f48c34252ff8930b3d74');
  const provider223 = history.find(entry => entry.id === provider.currentDecisionId);
  assert.equal(finalCurrent.id, 'd7fc1df6cbade4a92d25');
  // Contract store (2026-11-04, CONTRACT-STORE-001): one migration appended after
  // the 238-entry review, which stays retired and hash-preserved.
  const reviewed238 = history.find(entry => entry.id === '050f309300a1688162fa');
  assert.equal(canonicalHash(reviewed238),
    '8c6aa26845f40678a9625f41a2d67b5dc367d290f4b0432639fd8472b85f4b8b');
  assert.equal(reviewed238.evidence.valueCount, 238);
  assert.equal(reviewed238.reviewed, true);
  assert.equal(finalCurrent.previousDecision.id, reviewed238.id);
  const contractStore = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3ContractStore20261104.document), 'utf8'));
  assert.equal(contractStore.currentDecisionId, finalCurrent.id);
  assert.deepEqual(contractStore.previousManifestDecision, reviewed238);
  assert.equal(contractStore.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 238)));
  assert.equal(contractStore.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(238)));
  assert.deepEqual(contractStore.appendedMigrationIds, ['2026-11-04_contract_store']);
  // Session invoice issuance (2026-11-04, PAY-INVOICE-ISSUE-001): one migration
  // appended after the 237-entry review, which stays retired and hash-preserved.
  const reviewed237 = history.find(entry => entry.id === 'ef3b8933af3854450a02');
  assert.equal(canonicalHash(reviewed237),
    'b3e101545dbce2e26e9603fbc81d735463b22f0cb486b4ae9342f724131fa145');
  assert.equal(reviewed237.evidence.valueCount, 237);
  assert.equal(reviewed237.reviewed, true);
  assert.equal(reviewed238.previousDecision.id, reviewed237.id);
  const invoiceIssue = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3SessionInvoiceIssue20261104.document), 'utf8'));
  assert.equal(invoiceIssue.currentDecisionId, reviewed238.id);
  assert.deepEqual(invoiceIssue.previousManifestDecision, reviewed237);
  assert.equal(invoiceIssue.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 237)));
  assert.equal(invoiceIssue.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(237, 238)));
  assert.deepEqual(invoiceIssue.appendedMigrationIds, ['2026-11-04_session_invoice_issue']);
  // Directory review states (2026-11-04, F-09): one migration appended after the
  // 236-entry review merged to main, which stays retired and hash-preserved.
  const merged236 = history.find(entry => entry.id === '438668da134480da0a9e');
  assert.equal(canonicalHash(merged236),
    'fae5221769f1e07714cd6160b57b8ae09146c3840d305f109d81baa176396032');
  assert.equal(merged236.evidence.valueCount, 236);
  assert.equal(merged236.reviewed, true);
  assert.equal(reviewed237.previousDecision.id, merged236.id);
  const reviewStates = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3DirectoryReviewStates20261104.document), 'utf8'));
  assert.equal(reviewStates.currentDecisionId, reviewed237.id);
  assert.deepEqual(reviewStates.previousManifestDecision, merged236);
  assert.equal(reviewStates.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 236)));
  assert.deepEqual(reviewStates.appendedMigrationIds, ['2026-11-04_directory_review_status_without_pending']);
  // Domo deposit validator (2026-11-04): one migration appended after the
  // reconciled 235-entry review, which stays retired and hash-preserved.
  const reconciled235 = history.find(entry => entry.id === '27def070486792cad395');
  assert.equal(canonicalHash(reconciled235),
    '085659be48eed02b09860767b22855e80f1fe17bddca460289a6ab13504fca03');
  assert.equal(reconciled235.evidence.valueCount, 235);
  assert.equal(reconciled235.reviewed, true);
  assert.equal(merged236.previousDecision.id, reconciled235.id);
  const depositValidator = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2DomoDepositValidator20261104.document), 'utf8'));
  assert.equal(depositValidator.currentDecisionId, merged236.id);
  assert.equal(depositValidator.previousDecisionId, reconciled235.id);
  assert.deepEqual(depositValidator.previousManifestDecision, reconciled235);
  assert.equal(depositValidator.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 235)));
  assert.equal(depositValidator.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(235, 236)));
  assert.deepEqual(depositValidator.appendedMigrationIds, ['2026-11-04_domo_quote_deposit_policy_validator']);
  // Session-expiry integration (2026-11-02): two concurrent successors of the
  // 227-entry review are reconciled; both stay retired and hash-preserved.
  const audit234 = history.find(entry => entry.id === 'd9468b767dadec9cf3d8');
  const sessionExpiry228 = history.find(entry => entry.id === '040c32c5b44ceadb2dde');
  assert.equal(canonicalHash(audit234),
    '61cbf1e27157b704dcad31fb8800df2d3498c2b57efb3dbc519f63f8b38e1c18');
  assert.equal(canonicalHash(sessionExpiry228),
    'e863458f044f4994d17134c7cd04ae00e8d48c95c2f59f23f076c6d27cfcf85e');
  assert.equal(audit234.evidence.valueCount, 234);
  assert.equal(sessionExpiry228.evidence.valueCount, 228);
  assert.equal(audit234.reviewed, true);
  assert.equal(sessionExpiry228.reviewed, true);
  assert.equal(reconciled235.previousDecision.id, audit234.id);
  assert.deepEqual(reconciled235.concurrentReviewHistory.map(entry => entry.id), [sessionExpiry228.id]);
  assert.equal(sessionExpiry228.previousDecision.id, 'a2bb0e828d5b94c1cb31');
  for (const parent of [audit234, sessionExpiry228]) {
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const sessionExpiry = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2SessionExpiryMainIntegration20261102.document), 'utf8'));
  assert.equal(sessionExpiry.currentDecisionId, reconciled235.id);
  assert.equal(sessionExpiry.previousDecisionId, audit234.id);
  assert.equal(sessionExpiry.concurrentDecisionId, sessionExpiry228.id);
  assert.deepEqual(sessionExpiry.mainManifestDecision, sessionExpiry228);
  assert.deepEqual(sessionExpiry.auditManifestDecision, audit234);
  assert.equal(sessionExpiry.interleavedIndex, 227);
  assert.equal(rawManifest.length, 239);
  // Protected main's 228-entry review is the exact prefix of the merged manifest.
  assert.equal(sessionExpiry.protectedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 228)));
  // The audit's 234-entry review is the merged manifest without main's entry.
  // Earlier reviews were taken over the 235-entry manifest before the deposit validator.
  const reconciledRaw = rawManifest.slice(0, 235);
  assert.equal(sessionExpiry.auditViewCanonicalSha256, canonicalHash(auditView(reconciledRaw)));
  assert.equal(auditView(reconciledRaw).length, 234);
  assert.equal(sessionExpiry.auditSuffixCanonicalSha256, canonicalHash(rawManifest.slice(228, 235)));
  const auditRaw = auditView(reconciledRaw);
  const auditReviewed = auditView(manifest.slice(0, 235));
  // Registry refresh (2026-11-02): one migration appended after the 233-entry
  // formal-audit review, which stays retired and preserved by canonical hash.
  const main233 = history.find(entry => entry.id === '903e61463c0222f30d9b');
  assert.equal(canonicalHash(main233),
    '9adad9f469eb233e58e2cbfac22651f797d7aba5e438d1f10ce880c436e0566c');
  assert.equal(main233.evidence.valueCount, 233);
  assert.equal(main233.reviewed, true);
  assert.equal(audit234.previousDecision.id, main233.id);
  const refresh = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2RegistryRefresh20261102.document), 'utf8'));
  assert.equal(refresh.currentDecisionId, audit234.id);
  assert.equal(refresh.previousDecisionId, main233.id);
  assert.equal(refresh.mainDecisionCanonicalSha256, canonicalHash(main233));
  assert.deepEqual(refresh.mainManifestDecision, main233);
  assert.equal(refresh.preservedPrefixCanonicalSha256, canonicalHash(auditRaw.slice(0, 233)));
  assert.equal(refresh.preservedPrefixCanonicalSha256BeforeCompatibleChecksum, canonicalHash(auditReviewed.slice(0, 233)));
  assert.equal(refresh.appendedSuffixCanonicalSha256, canonicalHash(auditRaw.slice(233)));
  assert.deepEqual(refresh.appendedMigrationIds, ['2026-11-02_identity_party_reference_refresh']);
  assert.equal(refresh.reviewedPrefixCanonicalSha256WithoutCompatibleChecksum['227'],
    '3fb0233724d5f6080b6d8933e46f0cd79c34b73fd3070e3e17b742ce1ff5113e');
  // The raw manifest differs from the reviewed one in exactly that one field.
  assert.equal(rawManifest[refresh.inPrefixChange.index].id, REGISTRY_REFRESH_ENTRY);
  assert.deepEqual(rawManifest[refresh.inPrefixChange.index].compatibleAppliedChecksums, refresh.inPrefixChange.added);
  for (const [index, entry] of rawManifest.entries()) {
    if (index === refresh.inPrefixChange.index) {
      const { compatibleAppliedChecksums: _added, ...rest } = entry;
      assert.deepEqual(rest, manifest[index]);
    } else assert.deepEqual(entry, manifest[index]);
  }
  // Formal audit phase 2 (2026-11-02): six audit migrations appended after the
  // exact 227-entry memory/provider review, which stays retired and preserved.
  const main227 = history.find(entry => entry.id === 'a2bb0e828d5b94c1cb31');
  assert.equal(canonicalHash(main227),
    '2a868127eeb27cf8ffcaf4d162dd6123bf0ae8ad9ce54901c0cabb631cf43e6e');
  assert.equal(main227.evidence.valueCount, 227);
  assert.equal(main227.reviewed, true);
  assert.equal(main233.previousDecision.id, main227.id);
  assert.equal(canonicalHash(manifest.slice(0, 227)),
    JSON.parse(await fs.readFile(path.join(workspaceRoot,
      document.reconciliation.formalAuditPhase2MainIntegration20261102.document), 'utf8')).preservedPrefixCanonicalSha256);
  assert.equal(canonicalHash(memory226),
    'd08e3ff9409b47ab035fc67269d79010fd3738333fdc971c305ded3ddfeadf9b');
  assert.equal(canonicalHash(provider223),
    '6bcc3d19c2aea1adb0ce6ede68d59ec60b025b953a083bc5be6090f4be90ed1a');
  assert.deepEqual(aggregate.memoryManifestDecision, memory226);
  assert.deepEqual(aggregate.mainManifestDecision, provider223);
  assert.equal(aggregate.memoryDecisionCanonicalSha256, canonicalHash(memory226));
  assert.equal(aggregate.mainDecisionCanonicalSha256, canonicalHash(provider223));
  assert.equal(canonicalHash(manifest.slice(0, 223)),
    '3123446d4ae490b41d13beb6ca62f17da39fbc90539791c9b5373709c05b1703');
  assert.equal(aggregate.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 223)));
  assert.equal(aggregate.memorySuffixCanonicalSha256, canonicalHash(manifest.slice(223, 227)));
  assert.deepEqual(manifest[222], {
    id: '2026-10-16_provider_domain_payment_binding',
    path: 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql',
    introducedBy: '456be073c944dd3d76eb9e3c2169fe7fbb4041ef',
  });
  assert.equal(provider.mainManifestDecision.id, header222.id);
  assert.equal(provider.mainManifestDecision.reviewed, true);
  assert.equal(provider.mainManifestDecision.evidence.valueCount, 222);
  assert.equal(canonicalHash(provider.mainManifestDecision),
    '866eef0dde46336e216336f13bdf9f83570314d2c17b9cf93f354de0f8ad9404');
  assert.equal(provider.mainDecisionCanonicalSha256, canonicalHash(provider.mainManifestDecision));
  assert.equal(canonicalHash(provider.providerManifestDecision),
    '018a1dc08aafe97b3af2257fe2ad24bac9a8f47f724975df0eec07e729ba8058');
  assert.equal(provider.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 222)));
  for (const parent of [memory226, provider223]) {
    assert.equal(parent.reviewed, true);
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const retiredParents = [...historical, current, memoryParent, memory225, header222,
    combined.headerManifestDecision, memory226, provider223, provider.providerManifestDecision, main227, main233,
    audit234, sessionExpiry228, reconciled235, merged236, reviewed237, reviewed238];
  assert.equal(current.classification, decision220.classification);
  assert.equal(current.disposition, decision220.disposition);
  assert.deepEqual(current.sourceReviewReferences.slice(0, decision220.sourceReviewReferences.length),
    decision220.sourceReviewReferences);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(current.evidence.valueCount, 221);
  assert.equal(pointer.currentId, latestCurrent.id);
  assert.equal(pointer.valueCount, 253);
  assert.equal(pointer.previousResolution.valueCount, 252);
  assert.equal(pointer.previousResolution.previousResolution.valueCount, 251);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 249);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 248);
  assert.equal(pointer.change, latestCurrent.justification);
  const retirement = integration.retirements.find(entry => entry.source.name === 'migrations');
  assert.equal(retirement.replacementId, latestCurrent.id);
  assert.equal(retirement.currentResolution.id, latestCurrent.id);
  assert.equal(retirement.currentResolution.valueCount, 253);
  assert.equal(retirement.previousResolution.currentResolution.valueCount, 252);
  assert.equal(retirement.previousResolution.previousResolution.currentResolution.valueCount, 251);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 250);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 249);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 248);
  assert.equal(retirement.reason, latestCurrent.justification);
  for (const decision of retiredParents) {
    assert.ok(pointer.previousCurrentIds.includes(decision.id));
    assert.ok(retirement.previousReplacementIds.includes(decision.id));
  }

  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-alert-successor-test-'));
  try {
    const repoDir = path.join(tempRoot, 'repo');
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, 'scripts/production-migrations.json', JSON.stringify({ migrations: rawManifest }));
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

test('dated-UX-parent: main-parent: event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
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

test('dated-UX-parent: main-parent: reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active, successors } = await readReconciliation();
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-reconciliation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');
  const decisionsPath = path.join(tempRoot, 'decisions.json');
  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    for (const file of new Set(ledger.retirements.map(entry => entry.file))) {
      await writeFile(repoDir, file, await mainSourceBytes(file));
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


test('dated-UX-parent: main-parent: main integration archives exact old decisions and retains reviewed successors or source-removal evidence', async () => {
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

test('dated-UX-parent: main-parent: catalog audit excludes ignored local source files from candidate fingerprints', async () => {
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

test('dated-UX-parent: main-parent: decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
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
      // The removed review is kept as retirement provenance, so any history it
      // references stays reachable in the content-addressed store.
      sourceRemovalRetirements: [{
        reason: 'Catalog refresh unit-test-review: these candidates are no longer in the static list inventory. Their full reviewed decisions are preserved here as history; no catalog values are affected.',
        decisions: [{ ...preserved, id: 'stale-id' }],
      }],
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('dated-UX-parent: main-parent: ticket main merge retains stale approvals only as provenance', async () => {
  const document = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const retired = document.reconciliation.ticketMainMergeRetirements;
  assert.equal(retired.sourceMain, 'b06a4d3926e03a0b72a23732e454a33c72eb9705');
  assert.equal(retired.decisions.length, 3);
  for (const entry of retired.decisions) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'), entry.originalDecisionSha256);
    assert.ok(!document.decisions.some(item => item.id === entry.originalDecision.id));
    if (entry.replacementId === 'fd606f7d00f82c78cb20') {
      // The dated 164-feature replacement is now nested in the explicitly
      // reviewed canonical 165-feature mirror. It cannot authorize that new
      // source: the generator/negative-control test below checks this directly.
      const successor = document.decisions.find(item => item.id === 'ea29c6e8a6cd165802a8');
      assert.equal(successor?.reviewed, true);
      const parent = (await reviewedJson(INTEGRATION_MAIN, catalogPath)).decisions
        .find(item => item.id === entry.replacementId);
      assert.deepEqual(successor.previousDecision, parent);
      assert.ok(!document.decisions.some(item => item.id === entry.replacementId));
    } else if (entry.replacementId) {
      assert.equal(document.decisions.find(item => item.id === entry.replacementId)?.reviewed, true);
    } else {
      assert.equal(entry.originalDecision.id, 'ec1a316784aa9992384a');
      assert.doesNotMatch(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'tdf-mobile/app/ddex/index.tsx', 'utf8'), /const STATUS_LABELS/);
    }
  }
});


test('dated-UX-parent: main-parent: storage/main integration preserves both exact parent reviews and manifest views', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const manifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references');
  const { active, successors } = await readReconciliation();
  const extension = active.find(entry => entry.id === successors.find(item => item.name === 'migrations').currentId);
  const current = historicalReview(extension, 244);
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item) ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(manifest.length, 244);
  assert.equal(canonicalHash(manifest.slice(0, 239)), ledger.protectedMainPrefixCanonicalSha256);
  assert.equal(canonicalHash(manifest.filter(entry => !ledger.mainOnlyMigrationIds.includes(entry.id))), ledger.storageViewCanonicalSha256);
  assert.equal(current.evidence.valueCount, 244);
  assert.equal(successors.find(entry => entry.name === 'migrations').currentId, extension.id);
  for (const parent of ledger.parentReviews) {
    const entry = [current.previousDecision, ...current.concurrentReviewHistory].find(item => item.id === parent.id);
    assert.equal(canonicalHash(entry), parent.canonicalSha256);
    assert.ok(!active.some(item => item.id === parent.id));
  }
  assert.equal(ledger.executionAllowed, false);
});

test('dated-UX-parent: main-parent: video campaign migration appends to the exact protected 244-entry manifest', async () => {
  const main = 'e5248e0b3ad30d832ef1ccbf5e595a7ee36b0324';
  const introduction = '4b84238abb4ca17c6aaa9902e3bec16a1cc3daa5';
  const old = JSON.parse((await execFileAsync('git', ['show', `${main}:scripts/production-migrations.json`], { cwd: workspaceRoot })).stdout).migrations;
  const current = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const priorDecisions = JSON.parse((await execFileAsync('git', ['show', `${main}:${decisionsPath}`], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout);
  const { active } = await readReconciliation(true);
  const latest = active.find(item => item.evidence?.file === manifestPath);
  const videoReview = historicalReview(latest, 245);
  assert.equal(videoReview.id, '969410f2f45854cc54fc');
  assert.deepEqual(videoReview.previousDecision,
    priorDecisions.decisions.find(item => item.id === 'be0b1503a04f9fa295ee'));
  assert.equal(old.length, 244);
  assert.equal(current.length, 246);
  assert.deepEqual(current.slice(0, 244), old);
  assert.deepEqual(current[244], { id: '2026-10-20_video_campaigns', path: 'tdf-hq/sql/2026-10-20_video_campaigns.sql', introducedBy: introduction });
  const originalSql = (await execFileAsync('git', ['show', `${introduction}:${current[244].path}`], { cwd: workspaceRoot })).stdout;
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[244].path), 'utf8'), originalSql);
  await execFileAsync('git', ['merge-base', '--is-ancestor', introduction, 'HEAD'], { cwd: workspaceRoot });
  const forward = '09fd7babb92e45c1da1b9e5017de53b6378b4f7c';
  assert.deepEqual(current[245], { id: '2026-11-06_video_campaign_identity_references', path: 'tdf-hq/sql/2026-11-06_video_campaign_identity_references.sql', introducedBy: forward });
  const priorManifest = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.deepEqual(current.slice(0, 245), priorManifest);
  const priorReview = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === videoReview.id);
  assert.deepEqual(videoReview, priorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[245].path), 'utf8'), (await execFileAsync('git', ['show', forward + ':' + current[245].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', forward, 'HEAD'], { cwd: workspaceRoot });
  // ADR 0134: marketplace offers were reviewed on top of the exact 246-entry
  // identity manifest (8ee6c7a) and now append after the two Apple entries that
  // protected main integrated; the manifest without those entries is still that
  // reviewed 247-entry manifest byte for byte.
  const marketplace = '8ee6c7a66e2504f425863ca6f5b83062723efd58';
  const marketplaceIntroduction = '5901003189dac2291380604c66fdd66087e4a587';
  const full = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(full.length, 249);
  // The agreements entry (ADR 0135) is the 251st, the last one before the
  // event administrator entry: its SQL is exactly what its introduction commit
  // carries and that commit is an ancestor.
  const agreementsEntry = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY).at(-1);
  assert.equal(agreementsEntry.id, MARKETPLACE_AGREEMENTS_ENTRY);
  assert.equal(agreementsEntry.path, 'tdf-hq/sql/2026-11-12_marketplace_agreements.sql');
  assert.equal(await fs.readFile(path.join(workspaceRoot, agreementsEntry.path), 'utf8'), (await execFileAsync('git', ['show', agreementsEntry.introducedBy + ':' + agreementsEntry.path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', agreementsEntry.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  assert.deepEqual(full[248], { id: MARKETPLACE_OFFERS_ENTRY, path: 'tdf-hq/sql/2026-11-08_marketplace_offers.sql', introducedBy: marketplaceIntroduction });
  const identityManifest = JSON.parse((await execFileAsync('git', ['show', marketplace + ':scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.equal(identityManifest.length, 247);
  // The marketplace entry's introduction moved to the review-fix commit (the
  // SQL gained the analytics allowlist and the filled -> published edge);
  // everything before it is still the reviewed identity manifest byte for byte.
  const withoutApple = full.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY);
  assert.deepEqual(withoutApple.slice(0, -1), identityManifest.slice(0, -1));
  assert.equal(withoutApple.at(-1).id, identityManifest.at(-1).id);
  const identityReview = historicalReview(latest, 246);
  assert.equal(identityReview.id, '806180f5957fc4a7520f');
  const identityPriorReview = JSON.parse((await execFileAsync('git', ['show', marketplace + ':' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === identityReview.id);
  assert.deepEqual(identityReview, identityPriorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, full[248].path), 'utf8'), (await execFileAsync('git', ['show', marketplaceIntroduction + ':' + full[248].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', marketplaceIntroduction, 'HEAD'], { cwd: workspaceRoot });
});

test('dated-UX-parent: main-parent: Apple policy preserves the exact main prefix and every manual review', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const fromGit = async file => (await execFileAsync('git', ['show', `${main}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout;
  const oldManifest = JSON.parse(await fromGit('scripts/production-migrations.json')).migrations;
  // The marketplace offers entry (ADR 0134) appends after the Apple entries;
  // the Apple review is verified over the manifest without it.
  const currentManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(oldManifest.length, 246);
  assert.equal(currentManifest.length, 248);
  assert.deepEqual(currentManifest.slice(0, 246), oldManifest);
  const issuer = currentManifest[247];
  assert.deepEqual(issuer, { id: APPLE_ISSUER_ENTRY, path: 'tdf-hq/sql/2026-11-07_apple_provider_identity_issuer.sql', introducedBy: '7c1525edf0f02fe53f985c24862940b91c11cdfa' });
  assert.equal(await fs.readFile(path.join(workspaceRoot, issuer.path), 'utf8'), (await execFileAsync('git', ['show', `${issuer.introducedBy}:${issuer.path}`], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', issuer.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  const addition = currentManifest[246];
  assert.equal(addition.id, APPLE_POLICY_ENTRY);
  assert.equal(addition.path, 'tdf-hq/sql/2026-10-13_apple_sign_in_policy.sql');
  const original = JSON.parse((await execFileAsync('git', ['show',
    'e7ec452787e15f09deda4c4328ec6a8c1cece727:scripts/production-migrations.json'],
  { cwd: workspaceRoot })).stdout).migrations.find(row => row.id === APPLE_POLICY_ENTRY);
  assert.deepEqual(addition, original);
  assert.equal(await fs.readFile(path.join(workspaceRoot, addition.path), 'utf8'),
    (await execFileAsync('git', ['show', `${addition.introducedBy}:${addition.path}`],
      { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', addition.introducedBy, 'HEAD'],
    { cwd: workspaceRoot });
  const old = JSON.parse(await fromGit('docs/catalog-persistence/catalog-list-decisions.json')).decisions;
  // Compare the Apple review with the records as protected main 8ec7d85c held
  // them: the event administrator successors are read back to the review,
  // pointer and retirement they replaced (each nests its predecessor
  // unchanged), and lists first reviewed after that main are left out.
  const { active: currentDecisions, retired, successors: currentSuccessors,
    integration: currentIntegration } = await readReconciliation(true);
  const mainIds = new Set((await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/catalog-list-decisions.json')).decisions.map(row => row.id));
  // Later successors (the agreement payments manifest review, ADR 0136) nest
  // the event administrator one, so step back until the record that main held.
  const active = currentDecisions.flatMap(row => {
    let reviewed = row;
    while (reviewed && !mainIds.has(reviewed.id)) reviewed = reviewed.previousDecision;
    return reviewed ? [reviewed] : [];
  });
  const heldByMain = (row, idOf) => {
    let held = row;
    while (held && !mainIds.has(idOf(held))) held = held.previousResolution;
    return held;
  };
  const successors = currentSuccessors.map(row => row.name === 'migrations' ? heldByMain(row, held => held.currentId) : row);
  const integration = { ...currentIntegration, retirements: currentIntegration.retirements
    .map(row => row.source.name === 'migrations' ? heldByMain(row, held => held.replacementId) : row) };
  const reviewedParent = async revision => JSON.parse((await execFileAsync('git',
    ['show', `${revision}:docs/catalog-persistence/catalog-list-decisions.json`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  const priorApple = await reviewedParent('bc16d557c7b246361c83d78191314087ca2dd3d9');
  const currentMain = await reviewedParent('321f4fe6c8e803ab1c32724a8ce5b7814418b79c');
  const offlineMain = await reviewedParent('5c69dc30318d110d9daba042571b8e54f5bf140d');
  const offlineFixture = offlineMain.find(row => row.id === 'b3eadb9d52a262f5371f');
  assert.ok(offlineFixture, 'the offline currency fixture needs its exact protected-main review');
  assert.deepEqual(active.find(row => row.id === offlineFixture.id), offlineFixture);
  // Protected main later reviewed two route-specification lists (#710); keep
  // their exact reviews and leave them out of the historical Apple view too.
  const routeMain = await reviewedParent('b1614afd18b02e8cd0d46f71d798b7f38764bcec');
  const routeReviews = ['afe675892f43a8a4de73', '6f876292d2fcb1dd7a86']
    .map(id => routeMain.find(row => row.id === id));
  for (const review of routeReviews) {
    assert.ok(review, 'the route-specification reviews need their exact protected-main review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The desktop shell (#717) adds nine reviewed playback and desktop lists;
  // keep their exact branch reviews outside the historical Apple view as well.
  const desktopBranch = await reviewedParent('3668640fa56a558bbd3bd95881526d5faf5be22c');
  const desktopReviews = ['9f5a58afbcc5a21f3a81', '162bb73e8a27ecdbd80d', '19b53762aa84c847059a',
    'd3fe8dedc63dbee01dcf', 'dd837be242ffaf6c73c5', '70eef2bc5a9e8ef6ab65', 'ee97985b0d9abc1b1ed3',
    '57fc580c92ca2352698d', '2cf4da8b8443b9be1b48'].map(id => desktopBranch.find(row => row.id === id));
  for (const review of desktopReviews) {
    assert.ok(review, 'the desktop shell reviews need their exact branch review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The marketplace offers branch (ADR 0134, #719) adds eighteen value lists
  // reviewed by the deterministic policy, extends the search page view-mode
  // attribute list (successor of main's exact review 7393e5dd10ebe68ae3e2) and
  // records a successor review of the 249-entry manifest whose exact
  // predecessor is main's 248-entry review; the historical Apple view keeps
  // the exact predecessors and leaves the new lists out.
  const marketplaceReviewIds = ['3d30e49db2fefa10a623', '2471af5d3bf550aade51', '0365580729887c8f5ca7',
    '0dc607f91bd630b1fb87', '4ed56a39eea6e4d4679d', 'aa6a65391d2c0611fdd8',
    'b79670b0b1a1fbc1ee05', 'df679c5b2cb5d91be79e', 'ef5bffc43b9c0ff3a361', '39edb2a76ecef8cd6c57',
    '2a350b9da29a2d080bd8', 'd776481beacf4f4a074f', '16393adee900667601af', '7207f087ef5f8e0b2158',
    '3a7f374bb3b52ef8d34d', 'a9c6757fbe6f2868f4f2', '853a5f768e09d0c32259',
    // Listing-conditions labels on the public classified page (review fix round four).
    '9dd6b72c9435bd64fe09'];
  for (const id of marketplaceReviewIds) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace offers review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  // The auth_rate_limit migration (owner decisions of 2026-11-10) succeeds the
  // marketplace manifest review as the 250-entry tip and adds two reviews; the
  // historical view sets those aside and steps the manifest back twice.
  // The marketplace agreements migration (ADR 0135) succeeds the auth_rate_limit
  // review as the 251-entry tip and adds six reviews (status filter, panel
  // tabs, agreement CHECK lists and the party type); the historical view sets
  // those aside too and steps the manifest back once more.
  const agreementsTip = active.find(row => row.id === 'bc5e5bd4f9d8795d2166');
  assert.equal(agreementsTip.evidence.valueCount, 251);
  const agreementReviews = ['3a5425de732f609db717', '75f5a77be26697f0a281', '06f1e7eef25d710385f4',
    '74460721a4c7f744e3ab', '33d907b3850fb5474551', 'c9a145a03e59580950b5'];
  for (const id of agreementReviews) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace agreements review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  const authRateLimitTip = agreementsTip.previousDecision;
  assert.equal(authRateLimitTip.id, '52d96d4b31e677ac5948');
  assert.equal(authRateLimitTip.evidence.valueCount, 250);
  const authRateLimitReviews = ['f37745d92e494e02fcba', 'a95cbb8b9256407889ea'];
  const marketplaceManifestReview = authRateLimitTip.previousDecision;
  assert.equal(marketplaceManifestReview.id, '4dc5a2f26154a0f125db');
  assert.equal(marketplaceManifestReview.evidence.valueCount, 249);
  assert.equal(marketplaceManifestReview.previousDecision.id, 'a20e04bc426cc6383584');
  const searchViewReview = active.find(row => row.id === '79bdbe2ae9b350e0d1da');
  assert.equal(searchViewReview.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(searchViewReview.previousDecision.id, '7393e5dd10ebe68ae3e2');
  const marketplaceSuccessors = new Map([
    [agreementsTip.id, marketplaceManifestReview.previousDecision],
    [searchViewReview.id, searchViewReview.previousDecision]]);
  const laterMainIds = new Set([offlineFixture.id, ...routeReviews.map(row => row.id),
    ...desktopReviews.map(row => row.id), ...marketplaceReviewIds, ...authRateLimitReviews, ...agreementReviews]);
  const historicalAppleView = active.filter(row => !laterMainIds.has(row.id))
    .map(row => marketplaceSuccessors.get(row.id) ?? row);
  assert.equal(old.length, 1405);
  assert.equal(priorApple.length, 1407);
  assert.equal(currentMain.length, 1409);
  const retiredIds = new Set(retired.map(row => row.id));
  assert.equal(historicalAppleView.length, 1411 - retired.length);
  const appleView = (await reviewedParent('15e2228d24080a520d4bed187731cc3db09d474a'))
    .filter(row => !retiredIds.has(row.id));
  assert.equal(historicalAppleView.length, appleView.length);
  for (const [index, row] of appleView.entries()) {
    assert.deepEqual(historicalAppleView[index], row, `Apple review order or content changed at ${row.id}`);
  }
  assert.equal(active.length,
    1423 + marketplaceReviewIds.length + authRateLimitReviews.length + agreementReviews.length - retired.length);
  const history = new Map(retired.map(row => [row.id, row]));
  const visit = row => {
    if (!row) return;
    if (!history.has(row.id)) history.set(row.id, row);
    visit(row.previousDecision);
    for (const branch of row.concurrentReviewHistory ?? []) visit(branch);
  };
  for (const row of active) visit(row);
  for (const row of [...old, ...priorApple, ...currentMain, ...offlineMain, ...routeMain]) {
    assert.deepEqual(history.get(row.id), row);
  }
  const pointer = successors.find(row => row.name === 'migrations');
  const agreementsCurrentTip = active.find(row => row.id === pointer.currentId);
  assert.equal(agreementsCurrentTip.evidence.valueCount, 251);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').replacementId,
    agreementsCurrentTip.id);
  assert.equal(pointer.previousResolution.currentId, agreementsCurrentTip.previousDecision.id);
  const currentTip = agreementsCurrentTip.previousDecision;
  assert.equal(currentTip.evidence.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.currentId, currentTip.previousDecision.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousReplacementId,
    currentTip.id);
  const marketplaceTip = currentTip.previousDecision;
  assert.equal(marketplaceTip.evidence.valueCount, 249);
  const tip = marketplaceTip.previousDecision;
  assert.equal(tip.evidence.valueCount, 248);
  assert.equal(historicalReview(tip, 247).previousDecision.id, '806180f5957fc4a7520f');
  assert.equal(tip.previousDecision.id, '4e282e4c258854caa655');
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.currentId, tip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousReplacementId,
    marketplaceTip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousResolution.previousReplacementId,
    tip.id);
  const security = active.find(row => row.evidence?.file === 'tdf-hq/src/TDF/Catalog/Security.hs'
    && row.evidence?.name === 'expectedSecurityRoleAssignmentPolicyBindings');
  assert.equal(security.previousDecision.id, 'ff4edc0f8d2debecf71c');
  const currentSecurity = await fs.readFile(path.join(workspaceRoot, security.evidence.file), 'utf8');
  const withoutApple = currentSecurity.replace(
    '  , ("account.apple.customer", "apple-account-create", "customer", True)\n', '');
  assert.notEqual(withoutApple, currentSecurity);
  assert.equal(withoutApple, await fromGit(security.evidence.file));
});

test('dated-UX-parent: main-parent: event administrator delegation appends one migration to the protected 251-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: EVENT_ADMIN_ENTRY, path: 'tdf-hq/sql/2026-11-11_event_admin_delegation.sql',
    introducedBy: EVENT_ADMIN_INTRODUCTION };
  assert.equal(main.migrations.length, 251);
  // The agreement payments entry (ADR 0136) appends after this one and has its own review.
  assert.deepEqual({ ...combined, migrations: combined.migrations.filter(entry => entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY) },
    { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${EVENT_ADMIN_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', EVENT_ADMIN_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, decisionsPath)).decisions;
  // The active manifest review is the agreement payments successor; this
  // review, its pointer and its retirement are nested one step back, unchanged.
  const tipPointer = successors.find(row => row.name === 'migrations');
  const pointer = tipPointer.previousResolution;
  const current = active.find(row => row.id === tipPointer.currentId).previousDecision;
  assert.equal(current.id, pointer.currentId);
  assert.equal(current.evidence.valueCount, 252);
  assert.equal(current.reviewed, true);
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 251);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations').previousResolution;
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // The typed client registry is the only other successor; it also nests its parent review.
  // EVT-TICKET-SALE-001 later added four methods: its 90-method successor keeps
  // this 86-method review intact as its previousDecision.
  const clientNow = active.find(row => row.id === successors.find(row => row.name === 'SocialEventsAPI').currentId);
  assert.equal(clientNow.evidence.valueCount, 90);
  const client = clientNow.previousDecision;
  assert.equal(client.id, '083bd1a66e9f23f02866');
  assert.equal(client.evidence.valueCount, 86);
  assert.deepEqual(client.previousDecision, priorMain.find(row => row.id === '2511cbba2fc7279bc28a'));
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const movedReviews = assertMovedListsNest(active, priorMain);
  // Every other review of protected main is retained exactly; three lists are new.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, client.previousDecision.id, ...movedReviews]);
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else assert.deepEqual(history.get(row.id), row);
  }
  assert.equal(priorMain.length, 1449);
  // 1452 after this review, plus the nine agreement payments reviews (ADR 0136),
  // less the reviews retired with their deleted source lists; the delivery
  // protocol review (SYS-VERIFY-002) is counted separately.
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});

test('dated-UX-parent: main-parent: marketplace agreement payments append one migration to the protected 252-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY,
    path: 'tdf-hq/sql/2026-11-12_marketplace_agreement_payments.sql',
    introducedBy: AGREEMENT_PAYMENTS_INTRODUCTION };
  assert.equal(main.migrations.length, 252);
  assert.deepEqual(combined, { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${AGREEMENT_PAYMENTS_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', AGREEMENT_PAYMENTS_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, decisionsPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const current = active.find(row => row.id === pointer.currentId);
  assert.equal(current.evidence.valueCount, 253);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 252);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.equal(pointer.change, current.justification);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, current.id);
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // Every other review of protected main is retained exactly; nine lists are
  // new (payment provider, fee payer, payout and payment CHECK lists and the
  // two Policy sum types) and reviewed by the deterministic policy.
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, ...assertMovedListsNest(active, priorMain)]);
  // The 86-method client review was since superseded by EVT-TICKET-SALE-001
  // and is retained exactly as the previousDecision of its successor.
  const clientSuccessor = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else if (row.id === '083bd1a66e9f23f02866') assert.deepEqual(clientSuccessor.previousDecision, row);
    else assert.deepEqual(history.get(row.id), row);
  }
  const payoutReviews = ['d580ad14a8f01218c4a9', 'fbe5e4e4d0289e1caaf4', 'cfa5a05214e95ed07ef0',
    '203395cfa370d78f86a1', '1f6946049f264576684d'];
  const chargeReviews = ['09d7dc6ebc55642a4bb2', '584b9609b82ad71578ed', 'e1a25b160e801cf866df',
    '32574b309f1d005ed165'];
  for (const id of [...payoutReviews, ...chargeReviews]) {
    assert.ok(!priorMain.some(row => row.id === id));
    assert.ok(history.get(id)?.reviewed, `the marketplace agreement payments review ${id} must be present and reviewed`);
    assert.match(history.get(id).reviewMethod, /^deterministic-policy-v\d+$/);
  }
  assert.equal(priorMain.length, 1452);
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});


test('dated-UX-parent: main-parent: player server preserves both reviewed migration histories and appends only reviewed SQL', async () => {
  const main = await reviewedJson(PLAYER_SERVER_MAIN_BASE, manifestPath);
  const player = await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, manifestPath);
  const current = (await mainManifestView());
  const extension = player.migrations.filter(row => playerMigrations.has(row.id));
  assert.deepEqual(extension.map(row => row.id), [...playerMigrations]);
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...extension] });
  for (const row of extension) {
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'],
      { cwd: workspaceRoot });
    const { stdout } = await execFileAsync('git', ['show', `${row.introducedBy}:${row.path}`],
      { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
  }

  const { active, successors, integration } = await readReconciliation();
  const pointer = successors.find(row => row.name === 'migrations');
  const actual = active.find(row => row.id === pointer.currentId);
  const findTip = doc => doc.decisions.find(row => row.evidence?.file === manifestPath);
  const mainReview = findTip(await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath));
  const playerReview = findTip(await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, catalogPath));
  assert.deepEqual(actual.previousDecision, mainReview);
  assert.deepEqual(actual.concurrentReviewHistory, [playerReview]);
  assert.equal(actual.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, actual.id);
  assert.equal(retirement.previousResolution.replacementId, mainReview.id);
});

test('dated-UX-parent: main-parent: loop rework retires the reviews of two deleted scripts as provenance and reviews one new list', async () => {
  const { active } = await readReconciliation();
  const rework = await readLoopRework();
  assert.equal(rework.decisions.length, 2);
  assert.ok(rework.reason.length > 80);
  for (const entry of rework.decisions) {
    assert.equal(entry.decision.reviewed, true);
    assert.ok(!active.some(row => row.id === entry.decision.id));
    await assert.rejects(fs.access(path.join(workspaceRoot, entry.source.file)), { code: 'ENOENT' });
  }
  assert.deepEqual(rework.addedDecisionIds, ['16e3988d48089cc77b7e']);
  for (const id of rework.addedDecisionIds) {
    assert.equal(active.find(row => row.id === id)?.reviewed, true);
  }
  // The earlier view re-inserts the retired reviews and drops the new one; the catalog
  // itself keeps growing with main, so only the difference is fixed.
  const historical = (await readHistoricalReconciliation()).active;
  assert.equal(historical.length, active.length + rework.decisions.length - rework.addedDecisionIds.length);
  for (const entry of rework.decisions) assert.deepEqual(historical[entry.position], entry.decision);
});

// One explicit review classifies the delivery protocol; it is new, unique and
// carries its requirement. The rest of the catalog follows main.
test('dated-UX-parent: main-parent: delivery evidence protocol adds exactly one reviewed list', async () => {
  const current = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', catalogPath, 'utf8'));
  const baseline = await reviewedJson('5084d8a8a4d7a7a3b8d30d1f55ae5d9c8c032b9b', catalogPath);
  assert.ok(!baseline.decisions.some(row => row.id === DELIVERY_PROTOCOL_REVIEW));
  const additions = current.decisions.filter(row => row.id === DELIVERY_PROTOCOL_REVIEW);
  assert.equal(additions.length, 1);
  assert.equal(additions[0].classification, 'genuine-technical-constant');
  assert.equal(additions[0].disposition, 'explicit-allowlist');
  assert.equal(additions[0].reviewed, true);
  assert.equal(additions[0].evidence.file, 'scripts/lib/delivery-matrix.mjs');
  assert.equal(additions[0].evidence.name, 'DELIVERY_CHECKS');
  assert.match(additions[0].justification, /SYS-VERIFY-002/);
  assert.equal(current.decisions.filter(row => row.evidence?.file === 'scripts/lib/delivery-matrix.mjs').length, 1);
});

// These parent tests describe a dated schema. Only the two/one explicitly
// appended migration rows are projected away; combined-source checks below
// validate the actual unprojected registry and complete parent histories.
const INTEGRATION_MAIN = '966fb55dca70a49c0a340b674f8cc133cc413a2d';
const INTEGRATION_SOURCE = 'c702734dd932b8445e2429462d6638e03261435a';
const INTEGRATION_EXTRA_IDS = new Set(["2026-11-02_domo_refund_lifecycle"]);
const INTEGRATION_MAIN_REVIEW = '912cda11c7b6eb622f76';
async function actualManifest() {
  return JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'scripts/production-migrations.json', 'utf8'));
}
async function mainManifestView() {
  const actual = await actualManifest();
  return { ...actual, migrations: actual.migrations.filter(row => !INTEGRATION_EXTRA_IDS.has(row.id)) };
}
async function sourceManifestView() {
  const original = (await reviewedJson(INTEGRATION_SOURCE, 'scripts/production-migrations.json')).migrations;
  const current = (await actualManifest()).migrations;
  return { migrations: original.map(row => {
    const actual = current.find(item => item.id === row.id);
    assert.deepEqual(actual, row, `Source manifest entry changed: ${row.id}`);
    return actual;
  }) };
}
const reviewIndexes = new WeakMap();
function wholeReview(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  let index = reviewIndexes.get(value);
  if (!index) {
    index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      if (node.id) index.set(node.id, [...(index.get(node.id) ?? []), node]);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); reviewIndexes.set(value, index);
  }
  return (index.get(expected.id) ?? []).find(row => canonicalReview(row) === canonicalReview(expected));
}

async function sourceReconciliation() {
  const register = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', catalogPath, 'utf8'));
  const original = await reviewedJson(INTEGRATION_SOURCE, catalogPath);
  const active = original.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Complete original source review missing: ${row.id}`);
    return found;
  });
  return { active, successors: (await reviewedJson(INTEGRATION_SOURCE,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors };
}

const DOMO_REFUND_ENTRY = '2026-11-02_domo_refund_lifecycle';

test('dated-UX-parent: main-parent: Domo refund247 preserves exact Main246, original SQL/rollback and both review graphs', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const source = 'c36edd9f2fd99112ec56622d89ea37e8b72b2879';
  const readAt = async (ref, file) => (await execFileAsync('git', ['show', `${ref}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 32 * 1024 * 1024 })).stdout;
  const file = 'scripts/production-migrations.json';
  const prior = JSON.parse(await readAt(main, file)).migrations;
  const original = JSON.parse(await readAt(source, file)).migrations;
  const current = (await sourceManifestView()).migrations;
  assert.equal(prior.length, 246); assert.equal(original.length, 236); assert.equal(current.length, 247);
  assert.deepEqual(current.slice(0, 246), prior);
  assert.deepEqual(current[246], original[235]);
  assert.equal(current[246].id, DOMO_REFUND_ENTRY);
  assert.deepEqual(prior.slice(0, 235), original.slice(0, 235));
  await execFileAsync('git', ['merge-base', '--is-ancestor', current[246].introducedBy, 'HEAD'], { cwd: workspaceRoot });
  for (const migration of current) {
    const ref = migration.id === DOMO_REFUND_ENTRY ? source : main;
    assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'),
      await readAt(ref, migration.path), `Immutable reviewed SQL changed: ${migration.id}`);
  }
  const rollback = 'tdf-hq/sql/2026-11-02_domo_refund_lifecycle_rollback.sql';
  assert.equal(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', rollback, 'utf8'), await readAt(source, rollback));
  const decisions = 'docs/catalog-persistence/catalog-list-decisions.json';
  const sourceReviews = JSON.parse(await readAt(source, decisions));
  const mainReviews = JSON.parse(await readAt(main, decisions));
  const { active, successors } = await sourceReconciliation();
  const tip = active.find(row => row.id === successors.find(row => row.name === 'migrations').currentId);
  assert.equal(tip.evidence.valueCount, 247);
  assert.deepEqual(tip.previousDecision, mainReviews.decisions.find(row => row.evidence?.file === file));
  assert.deepEqual(tip.concurrentReviewHistory, [sourceReviews.decisions.find(row => row.evidence?.file === file)]);
  for (const priorReview of mainReviews.decisions) {
    if (priorReview.evidence?.file === file) continue;
    let retained = active.find(row => row.id === priorReview.id);
    if (priorReview.id === '34210b76e81fd04cffae') {
      // This assertion refers to dated Main f615's 75-method transport, not
      // current Main's reviewed 78-method successor. Preserve the WHOLE old
      // review in that actual successor chain; it must not be active again.
      retained = active.find(row => row.id === '2511cbba2fc7279bc28a');
      while (retained && retained.id !== priorReview.id) retained = retained.previousDecision;
    }
    assert.deepEqual(retained, priorReview);
  }
  assert.deepEqual(JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/domo-source-review-history-20261107.json', 'utf8')), sourceReviews);
});

test('dated-UX-parent: main-parent: dated source transport review retains its 75-method predecessor and historical negative control', async () => {
  const main = '321f4fe6c8e803ab1c32724a8ce5b7814418b79c';
  const file = 'tdf-hq-ui/src/api/socialEvents.ts';
  const decisionsFile = 'docs/catalog-persistence/catalog-list-decisions.json';
  const readAt = async name => (await execFileAsync('git', ['show', `${main}:${name}`],
    { cwd: workspaceRoot, maxBuffer: 32 * 1024 * 1024 })).stdout;
  const mainReviews = JSON.parse(await readAt(decisionsFile));
  const currentReviews = { decisions: (await sourceReconciliation()).active };
  const current = currentReviews.decisions.find(row => row.id === '2511cbba2fc7279bc28a');
  assert.deepEqual(current, mainReviews.decisions.find(row => row.id === current.id));
  assert.equal(current.evidence.valueCount, 78);
  const implementation = await readAt(file); // dated fixture for the retained negative control
  assert.equal(implementation, await readAt(file), 'Current transport must preserve complete canonical Main source');
  let predecessor = current;
  while (predecessor && predecessor.id !== '34210b76e81fd04cffae') predecessor = predecessor.previousDecision;
  assert.ok(predecessor, 'Keep the exact dated review in the actual successor chain');
  assert.equal(predecessor.evidence.valueCount, 75);
  assert.ok(!currentReviews.decisions.some(row => row.id === predecessor.id), 'Historical approval must stay inactive');

  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-current-transport-'));
  const repo = path.join(temporary, 'repo');
  const decisions = path.join(temporary, 'decisions.json');
  const reportPath = path.join(temporary, 'report.json');
  try {
    await fs.mkdir(repo);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repo });
    await writeFile(repo, file, implementation);
    await execFileAsync('git', ['add', '--', file], { cwd: repo });
    const mainInventory = JSON.parse(await readAt('docs/catalog-persistence/reports/static-list-inventory.json'));
    const ids = new Set(mainInventory.candidates.filter(row => row.file === file).map(row => row.id));
    const selected = mainReviews.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size, 'Keep every canonical review for this complete source file');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [auditScriptPath, '--root', repo, '--decisions', decisions, '--output', reportPath];
    await execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repo });
    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    const candidate = report.candidates.find(row => row.id === current.id);
    assert.ok(candidate, 'Actual source fingerprint must identify the active review');
    assert.equal(candidate.file, file);
    assert.equal(candidate.values.length, 78);
    assert.equal(candidate.decision, current.classification);

    // An intact historical review is still insufficient for the changed source.
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row.id === current.id ? predecessor : row)));
    await assert.rejects(execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
    const rejected = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    assert.equal(rejected.candidates.find(row => row.id === current.id)?.decision, 'unreviewed');
    assert.ok(!rejected.candidates.some(row => row.id === predecessor.id));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});


test('dated-UX-parent: main-parent: combined registry retains the exact main prefix, original SQL and whole parent reviews', async () => {
  const current = await actualManifest();
  const main = await reviewedJson(INTEGRATION_MAIN, manifestPath);
  const original = await reviewedJson(INTEGRATION_SOURCE, manifestPath);
  const suffix = original.migrations.filter(row => INTEGRATION_EXTRA_IDS.has(row.id));
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...suffix] });
  assert.equal(new Set(current.migrations.map(row => row.id)).size, current.migrations.length);
  for (const row of suffix) {
    const { stdout } = await execFileAsync('git', ['show', `${INTEGRATION_SOURCE}:${row.path}`], { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  }
  const register = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', catalogPath, 'utf8'));
  const pointerFile = 'docs/catalog-persistence/event-catalog-current-successors.json';
  const retireFile = 'docs/catalog-persistence/event-integration-retirements.json';
  const pointers = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', pointerFile, 'utf8'));
  const retirements = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', retireFile, 'utf8'));
  const pointer = pointers.successors.find(row => row.name === 'migrations');
  const tip = register.decisions.find(row => row.id === pointer.currentId);
  const retirement = retirements.retirements.find(row => row.source.name === 'migrations');
  assert.equal(tip.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  assert.equal(retirement.replacementId, tip.id);
  assert.deepEqual(tip.previousDecision, (await reviewedJson(INTEGRATION_MAIN, catalogPath))
    .decisions.find(row => row.evidence?.file === manifestPath));
  const canonical = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
    ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
  for (const parent of [INTEGRATION_MAIN, INTEGRATION_SOURCE]) {
    const old = await reviewedJson(parent, catalogPath);
    for (const row of old.decisions) {
      const found = wholeReview(register, row);
      assert.ok(found, `Lost complete parent review ${parent}:${row.id}`);
      assert.equal(canonical(found), canonical(row));
    }
    for (const [file, actual, key] of [[pointerFile, pointers, 'successors'], [retireFile, retirements, 'retirements']]) {
      const rows = (await reviewedJson(parent, file))[key];
      for (const row of rows) assert.ok(wholeResolution(actual, row), `Lost complete parent resolution ${parent}:${file}`);
    }
  }
});
function wholeResolution(value, expected) {
  return Boolean(findWholeResolution(value, expected));
}

const canonicalReview = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findWholeResolution(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if ((Object.hasOwn(expected, 'currentId') && value.currentId === expected.currentId || Object.hasOwn(expected, 'replacementId') && value.replacementId === expected.replacementId) && canonicalReview(value) === canonicalReview(expected)) return value;
  for (const child of Object.values(value)) {
    const found = findWholeResolution(child, expected);
    if (found) return found;
  }
}
async function mainSourceBytes(file) {
  if (file === 'tdf-mobile/src/features/generatedFeatureRegistry.ts') {
    const { stdout } = await execFileAsync('git', ['show', '4d866387ac6938d7aeaad5d1fa2985f8e02932e7:src/features/generatedFeatureRegistry.ts'], { cwd: path.join(workspaceRoot, 'tdf-mobile'), maxBuffer: 16 * 1024 * 1024 });
    return stdout; // exact dated fixture; the actual mirror has its own successor check below
  }
  return file === manifestPath ? JSON.stringify(await mainManifestView())
    : readPreservedParentSource('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', file);
}

function findHistoricalReview(value, id) {
  if (!value) return undefined;
  if (value.id === id) return value;
  const previous = findHistoricalReview(value.previousDecision, id);
  if (previous) return previous;
  for (const branch of value.concurrentReviewHistory ?? []) {
    const found = findHistoricalReview(branch, id);
    if (found) return found;
  }
}


test('dated-UX-parent: main-parent: actual Mobile mirror matches its generator and stale predecessor cannot authorize the new list', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-mobile-mirror-'));
  const mobileFile = 'tdf-mobile/src/features/generatedFeatureRegistry.ts';
  const generatedFile = 'src/features/generatedFeatureRegistry.ts';
  const register = JSON.parse(await readPreservedParentFile('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', catalogPath, 'utf8'));
  const current = register.decisions.find(row => row.id === 'ea29c6e8a6cd165802a8');
  assert.equal(current?.evidence.valueCount, 165);
  assert.equal(current.previousDecision.id, 'fd606f7d00f82c78cb20');
  try {
    const output = path.join(temporary, 'generated');
    await fs.mkdir(path.join(output, 'src/features'), { recursive: true });
    await generatePreservedParentMirror('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', output);
    const source = await integrationSource('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', mobileFile);
    assert.equal(source, await fs.readFile(path.join(output, generatedFile), 'utf8'));
    const repo = path.join(temporary, 'repo');
    await fs.mkdir(repo);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repo });
    await writeFile(repo, mobileFile, source);
    await execFileAsync('git', ['add', '--', mobileFile], { cwd: repo });
    const report = JSON.parse(await integrationSource('2c6dd64b9bd28d3fec6ac8d961eddea338d448d5', 'docs/catalog-persistence/reports/static-list-inventory.json'));
    const ids = new Set(report.candidates.filter(row => row.file === mobileFile).map(row => row.id));
    const selected = register.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size);
    assert.ok(ids.has(current.id));
    const decisions = path.join(temporary, 'decisions.json');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [auditScriptPath, '--root', repo, '--decisions', decisions, '--fail-on-unreviewed'];
    await execFileAsync(process.execPath, args, { cwd: repo });
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row.id === current.id ? current.previousDecision : row)));
    await assert.rejects(execFileAsync(process.execPath, args, { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});
}

// Complete dated parent assertions, reconstructed only from retained whole reviews.
{

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

// 2026-11-02 registry refresh: the only field ever added inside an already
// reviewed manifest prefix is the compatible applied checksum of the
// regenerated 2026-09-18 identity registry. Historical prefix hashes are taken
// over the manifest as it was reviewed, i.e. without that field.
const REGISTRY_REFRESH_ENTRY = '2026-09-18_identity_review_dependencies';
// 2026-11-02 session-expiry integration: protected main's migration is
// interleaved at index 227, ahead of the seven audit migrations. The audit's
// earlier reviews are verified over the manifest without that entry.
const SESSION_EXPIRY_ENTRY = '2026-10-31_api_token_session_expiry';
const APPLE_POLICY_ENTRY = '2026-10-13_apple_sign_in_policy';
const APPLE_ISSUER_ENTRY = '2026-11-07_apple_provider_identity_issuer';
// 2026-11-08 marketplace offers (ADR 0134) append after the Apple issuer entry.
const MARKETPLACE_OFFERS_ENTRY = '2026-11-08_marketplace_offers';
// 2026-11-10 auth rate limit (owner decisions of the formal audit) appends last.
const AUTH_RATE_LIMIT_ENTRY = '2026-11-10_auth_rate_limit';
// 2026-11-12 marketplace agreements (ADR 0135) append after the auth_rate_limit entry.
const MARKETPLACE_AGREEMENTS_ENTRY = '2026-11-12_marketplace_agreements';
// 2026-11-11 delegated event administrators: one migration appended last to the
// 251-entry manifest of protected main 8ec7d85c. Earlier reviews are verified
// over the manifest without that entry.
const EVENT_ADMIN_ENTRY = '2026-11-11_event_admin_delegation';
const EVENT_ADMIN_PROTECTED_MAIN = '8ec7d85c672e844e28c38e49c087b102032582d8';
const EVENT_ADMIN_INTRODUCTION = '2703eb3a1ac1604829e902262fe2dbfbbf4f4ca8';
// The marketplace agreement payments migration (ADR 0136) appends one entry to
// the 252-entry manifest of protected main d4bb9603. Earlier reviews are
// verified with the records stepped back to the main they reviewed.
const MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY = '2026-11-12_marketplace_agreement_payments';
const AGREEMENT_PAYMENTS_PROTECTED_MAIN = 'd4bb96030b1182e165ff39b614315c6a2554aee7';
const AGREEMENT_PAYMENTS_INTRODUCTION = '26c83d9e6525d859b0da0bebf959ad6d1c806159';

async function reviewedJson(revision, file) {
  const { stdout } = await execFileAsync('git', ['show', `${revision}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 });
  return JSON.parse(stdout);
}
const historicalReview = (entry, count) => {
  while (entry && entry.evidence?.valueCount !== count) entry = entry.previousDecision;
  assert.ok(entry, `Missing exact reviewed ${count}-entry predecessor`);
  return entry;
};
const auditView = entries => entries.filter(entry => entry.id !== SESSION_EXPIRY_ENTRY);
const asReviewedBeforeRegistryRefresh = entries => entries.map(entry => {
  if (entry.id !== REGISTRY_REFRESH_ENTRY) return entry;
  const { compatibleAppliedChecksums, ...reviewed } = entry;
  assert.deepEqual(compatibleAppliedChecksums,
    ['5636028805b77a1705bb364c87669e8d46983c93d9fc177da9e2187b98972f4f']);
  return reviewed;
});

const DELIVERY_PROTOCOL_REVIEW = '755cf14ac92e4f590879';
const PLAYER_SERVER_MAIN_BASE = '2ebb5c38bef08f3205877dc9521026b40b68d4ef';
const PLAYER_SERVER_REVIEW_HEAD = '85255658262166ab4aeb0e7e167b3a93c4a33d61';
const manifestPath = 'scripts/production-migrations.json';
const catalogPath = 'docs/catalog-persistence/catalog-list-decisions.json';
const playerMigrations = new Set([
  '2026-10-30_artist_featured_track',
  '2026-11-06_music_media_health',
  '2026-11-06_music_legacy_source_compatibility',
  '2026-11-07_music_featured_source_health',
  '2026-11-07_artist_featured_identity_guard',
]);

async function manifestBeforePlayer() {
  const manifest = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', manifestPath, 'utf8'));
  return { ...manifest, migrations: manifest.migrations.filter(row => !playerMigrations.has(row.id)) };
}

// Lists whose source file moved without any change to their values (ADR 0133).
// A list id follows its file, so each move is recorded as a successor review
// that nests the exact earlier review; checks against an earlier main read the
// successor back to that review.
const MOVED_FILE_REVIEW_BATCHES = new Set([
  'paypal-webhook-domain-module-2026-11-10',
  'web-auth-state-layer-2026-11-17',
  'web-feature-registry-layer-2026-11-17',
]);

function movedFileSuccessors(active) {
  const moved = active.filter(row => MOVED_FILE_REVIEW_BATCHES.has(row.reviewBatch));
  for (const row of moved) {
    assert.equal(row.reviewMethod, 'successor-of-reviewed-decision');
    assert.ok(row.previousDecision?.reviewed, `moved list ${row.id} must nest a reviewed decision`);
    assert.equal(row.classification, row.previousDecision.classification);
    assert.equal(row.disposition, row.previousDecision.disposition);
  }
  return moved;
}

// Every moved list whose earlier review protected main held nests that exact review.
function assertMovedListsNest(active, priorMain) {
  const prior = new Map(priorMain.map(row => [row.id, row]));
  return movedFileSuccessors(active).filter(row => prior.has(row.previousDecision.id)).map(row => {
    assert.deepEqual(row.previousDecision, prior.get(row.previousDecision.id));
    return row.previousDecision.id;
  });
}

// The continuous improvement loop rework (pull request #750) deleted two automation
// scripts: their reviews are retired as provenance and one new list is reviewed.
const LOOP_REWORK_RECONCILIATION = 'continuousImprovementLoopRework20261113';

async function readLoopRework() {
  return JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8')).reconciliation[LOOP_REWORK_RECONCILIATION];
}

async function readHistoricalReconciliation() {
  const current = await readReconciliation();
  const rework = await readLoopRework();
  const added = new Set(rework.addedDecisionIds);
  const active = current.active.filter(row => !added.has(row.id));
  for (const entry of [...rework.decisions].sort((left, right) => left.position - right.position)) {
    active.splice(entry.position, 0, entry.decision);
  }
  return { ...current, active };
}

async function readReconciliation(beforePlayer = false) {
  const ledger = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/event-catalog-retirements.json', 'utf8'));
  const register = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const mainRegister = await reviewedJson(INTEGRATION_MAIN, catalogPath);
  const active = mainRegister.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Dated main review missing: ${row.id}`);
    return found;
  });
  // Reviews whose source list was deleted outright (no successor list). They
  // leave the active register and are kept, unchanged, as history.
  const retired = (register.reconciliation.sourceRemovalRetirements ?? []).flatMap(entry => entry.decisions);
  let successors = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/event-catalog-current-successors.json', 'utf8')).successors;
  let integration = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/event-integration-retirements.json', 'utf8'));
  const parentPointers = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-catalog-current-successors.json');
  successors = parentPointers.successors.map(row => {
    const found = findWholeResolution(successors, row);
    assert.ok(found, `Dated main resolution missing: ${row.name}`);
    return found;
  });
  const parentRetirements = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-integration-retirements.json');
  integration = { ...integration, retirements: parentRetirements.retirements.map(row => {
    const found = findWholeResolution(integration, row);
    assert.ok(found, `Dated main retirement missing: ${row.source.name}`);
    return found;
  }) };
  if (!beforePlayer) return { ledger, active, retired, successors, integration };

  // Earlier reviews retain their own manifest fingerprint and catalog size.
  const predecessor = (await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const tip = active.find(row => row.id === pointer.currentId);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  const byId = new Map(active.map(row => row === tip ? row.previousDecision : row)
    .map(row => [row.id, row]));
  // A moved list's successor stands in for the review it nests.
  for (const row of movedFileSuccessors(active)) byId.set(row.previousDecision.id, row);
  // EVT-TICKET-SALE-001 extended the typed client registry; its successor
  // review nests the 86-method review unchanged and stands in for it.
  const ticketSaleClient = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  byId.set(ticketSaleClient.previousDecision.id, ticketSaleClient);
  // The loop rework (pull request #750) retired two reviews of deleted scripts as
  // provenance; the earlier catalog still lists them, so they stand in for themselves.
  for (const entry of (await readLoopRework()).decisions) byId.set(entry.decision.id, entry.decision);
  return {
    ledger,
    retired,
    active: predecessor.map(row => byId.get(row.id)).filter(Boolean),
    successors: successors.map(row => row === pointer ? row.previousResolution : row),
    integration: { ...integration, retirements: integration.retirements
      .map(row => row === retirement ? row.previousResolution : row) },
  };
}

test('dated-UX-parent: UX-parent: music/main reconciliation retains both reviews and the protected migration prefix', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/music-main-reconciliation.json', 'utf8'));
  const { active, successors, integration } = await readReconciliation(true);
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
  const manifest = asReviewedBeforeRegistryRefresh(JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'scripts/production-migrations.json', 'utf8')).migrations);
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
    const extension = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/public-ux-main-reconciliation-20261021.json', 'utf8'));
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

test('dated-UX-parent: UX-parent: memory226 and ledger succession preserve213/214/220/221/222/224/225 reviews and reject stale approvals', async () => {
  const document = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const transition = document.reconciliation.directorySavedSearchParity;
  const financial = document.reconciliation.directoryFinancialMainIntegration;
  const { active, successors, integration } = await readReconciliation(true);
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
  const storageMerge = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const rawManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references' && !storageMerge.storageMigrationIds.includes(entry.id));
  const manifest = asReviewedBeforeRegistryRefresh(rawManifest);
  assert.equal(canonicalHash(manifest.slice(0, 213)), protectedPrefixHash);
  assert.equal(transition.protectedMainManifestCanonicalSha256, protectedPrefixHash);
  const protectedFinancialPrefixHash = 'c025cf3d2ab92f64bfb92d3656384d7fe0da6919457440d3c24301d7f6b8f232';
  assert.equal(canonicalHash(manifest.slice(0, 220)), protectedFinancialPrefixHash);
  assert.equal(financial.protectedMainManifestCanonicalSha256, protectedFinancialPrefixHash);
  assert.equal(financial.sourceMain, '5114fe95ac8e328b58162a8dcae5c9015a47ce09');
  assert.equal(manifest.length, 239);
  assert.deepEqual(manifest[227], {
    id: SESSION_EXPIRY_ENTRY,
    path: 'tdf-hq/sql/2026-10-31_api_token_session_expiry.sql',
    introducedBy: '8187065f81fad94677c8ba27760ab2cb74f58f79',
  });
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
  assert.equal(canonicalHash(manifest.slice(223, 227)),
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
  const latestCurrent = active.find(entry => entry.id === pointer.currentId);
  const finalCurrent = historicalReview(latestCurrent, 239);
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
  assert.equal(finalCurrent.evidence.valueCount, 239);
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
  const provider = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.providerDomainMainIntegration.document), 'utf8'));
  const aggregate = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.eventMemoryProviderMain20261026.document), 'utf8'));
  const memory226 = history.find(entry => entry.id === 'f48c34252ff8930b3d74');
  const provider223 = history.find(entry => entry.id === provider.currentDecisionId);
  assert.equal(finalCurrent.id, 'd7fc1df6cbade4a92d25');
  // Contract store (2026-11-04, CONTRACT-STORE-001): one migration appended after
  // the 238-entry review, which stays retired and hash-preserved.
  const reviewed238 = history.find(entry => entry.id === '050f309300a1688162fa');
  assert.equal(canonicalHash(reviewed238),
    '8c6aa26845f40678a9625f41a2d67b5dc367d290f4b0432639fd8472b85f4b8b');
  assert.equal(reviewed238.evidence.valueCount, 238);
  assert.equal(reviewed238.reviewed, true);
  assert.equal(finalCurrent.previousDecision.id, reviewed238.id);
  const contractStore = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3ContractStore20261104.document), 'utf8'));
  assert.equal(contractStore.currentDecisionId, finalCurrent.id);
  assert.deepEqual(contractStore.previousManifestDecision, reviewed238);
  assert.equal(contractStore.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 238)));
  assert.equal(contractStore.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(238)));
  assert.deepEqual(contractStore.appendedMigrationIds, ['2026-11-04_contract_store']);
  // Session invoice issuance (2026-11-04, PAY-INVOICE-ISSUE-001): one migration
  // appended after the 237-entry review, which stays retired and hash-preserved.
  const reviewed237 = history.find(entry => entry.id === 'ef3b8933af3854450a02');
  assert.equal(canonicalHash(reviewed237),
    'b3e101545dbce2e26e9603fbc81d735463b22f0cb486b4ae9342f724131fa145');
  assert.equal(reviewed237.evidence.valueCount, 237);
  assert.equal(reviewed237.reviewed, true);
  assert.equal(reviewed238.previousDecision.id, reviewed237.id);
  const invoiceIssue = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3SessionInvoiceIssue20261104.document), 'utf8'));
  assert.equal(invoiceIssue.currentDecisionId, reviewed238.id);
  assert.deepEqual(invoiceIssue.previousManifestDecision, reviewed237);
  assert.equal(invoiceIssue.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 237)));
  assert.equal(invoiceIssue.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(237, 238)));
  assert.deepEqual(invoiceIssue.appendedMigrationIds, ['2026-11-04_session_invoice_issue']);
  // Directory review states (2026-11-04, F-09): one migration appended after the
  // 236-entry review merged to main, which stays retired and hash-preserved.
  const merged236 = history.find(entry => entry.id === '438668da134480da0a9e');
  assert.equal(canonicalHash(merged236),
    'fae5221769f1e07714cd6160b57b8ae09146c3840d305f109d81baa176396032');
  assert.equal(merged236.evidence.valueCount, 236);
  assert.equal(merged236.reviewed, true);
  assert.equal(reviewed237.previousDecision.id, merged236.id);
  const reviewStates = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3DirectoryReviewStates20261104.document), 'utf8'));
  assert.equal(reviewStates.currentDecisionId, reviewed237.id);
  assert.deepEqual(reviewStates.previousManifestDecision, merged236);
  assert.equal(reviewStates.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 236)));
  assert.deepEqual(reviewStates.appendedMigrationIds, ['2026-11-04_directory_review_status_without_pending']);
  // Domo deposit validator (2026-11-04): one migration appended after the
  // reconciled 235-entry review, which stays retired and hash-preserved.
  const reconciled235 = history.find(entry => entry.id === '27def070486792cad395');
  assert.equal(canonicalHash(reconciled235),
    '085659be48eed02b09860767b22855e80f1fe17bddca460289a6ab13504fca03');
  assert.equal(reconciled235.evidence.valueCount, 235);
  assert.equal(reconciled235.reviewed, true);
  assert.equal(merged236.previousDecision.id, reconciled235.id);
  const depositValidator = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2DomoDepositValidator20261104.document), 'utf8'));
  assert.equal(depositValidator.currentDecisionId, merged236.id);
  assert.equal(depositValidator.previousDecisionId, reconciled235.id);
  assert.deepEqual(depositValidator.previousManifestDecision, reconciled235);
  assert.equal(depositValidator.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 235)));
  assert.equal(depositValidator.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(235, 236)));
  assert.deepEqual(depositValidator.appendedMigrationIds, ['2026-11-04_domo_quote_deposit_policy_validator']);
  // Session-expiry integration (2026-11-02): two concurrent successors of the
  // 227-entry review are reconciled; both stay retired and hash-preserved.
  const audit234 = history.find(entry => entry.id === 'd9468b767dadec9cf3d8');
  const sessionExpiry228 = history.find(entry => entry.id === '040c32c5b44ceadb2dde');
  assert.equal(canonicalHash(audit234),
    '61cbf1e27157b704dcad31fb8800df2d3498c2b57efb3dbc519f63f8b38e1c18');
  assert.equal(canonicalHash(sessionExpiry228),
    'e863458f044f4994d17134c7cd04ae00e8d48c95c2f59f23f076c6d27cfcf85e');
  assert.equal(audit234.evidence.valueCount, 234);
  assert.equal(sessionExpiry228.evidence.valueCount, 228);
  assert.equal(audit234.reviewed, true);
  assert.equal(sessionExpiry228.reviewed, true);
  assert.equal(reconciled235.previousDecision.id, audit234.id);
  assert.deepEqual(reconciled235.concurrentReviewHistory.map(entry => entry.id), [sessionExpiry228.id]);
  assert.equal(sessionExpiry228.previousDecision.id, 'a2bb0e828d5b94c1cb31');
  for (const parent of [audit234, sessionExpiry228]) {
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const sessionExpiry = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2SessionExpiryMainIntegration20261102.document), 'utf8'));
  assert.equal(sessionExpiry.currentDecisionId, reconciled235.id);
  assert.equal(sessionExpiry.previousDecisionId, audit234.id);
  assert.equal(sessionExpiry.concurrentDecisionId, sessionExpiry228.id);
  assert.deepEqual(sessionExpiry.mainManifestDecision, sessionExpiry228);
  assert.deepEqual(sessionExpiry.auditManifestDecision, audit234);
  assert.equal(sessionExpiry.interleavedIndex, 227);
  assert.equal(rawManifest.length, 239);
  // Protected main's 228-entry review is the exact prefix of the merged manifest.
  assert.equal(sessionExpiry.protectedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 228)));
  // The audit's 234-entry review is the merged manifest without main's entry.
  // Earlier reviews were taken over the 235-entry manifest before the deposit validator.
  const reconciledRaw = rawManifest.slice(0, 235);
  assert.equal(sessionExpiry.auditViewCanonicalSha256, canonicalHash(auditView(reconciledRaw)));
  assert.equal(auditView(reconciledRaw).length, 234);
  assert.equal(sessionExpiry.auditSuffixCanonicalSha256, canonicalHash(rawManifest.slice(228, 235)));
  const auditRaw = auditView(reconciledRaw);
  const auditReviewed = auditView(manifest.slice(0, 235));
  // Registry refresh (2026-11-02): one migration appended after the 233-entry
  // formal-audit review, which stays retired and preserved by canonical hash.
  const main233 = history.find(entry => entry.id === '903e61463c0222f30d9b');
  assert.equal(canonicalHash(main233),
    '9adad9f469eb233e58e2cbfac22651f797d7aba5e438d1f10ce880c436e0566c');
  assert.equal(main233.evidence.valueCount, 233);
  assert.equal(main233.reviewed, true);
  assert.equal(audit234.previousDecision.id, main233.id);
  const refresh = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2RegistryRefresh20261102.document), 'utf8'));
  assert.equal(refresh.currentDecisionId, audit234.id);
  assert.equal(refresh.previousDecisionId, main233.id);
  assert.equal(refresh.mainDecisionCanonicalSha256, canonicalHash(main233));
  assert.deepEqual(refresh.mainManifestDecision, main233);
  assert.equal(refresh.preservedPrefixCanonicalSha256, canonicalHash(auditRaw.slice(0, 233)));
  assert.equal(refresh.preservedPrefixCanonicalSha256BeforeCompatibleChecksum, canonicalHash(auditReviewed.slice(0, 233)));
  assert.equal(refresh.appendedSuffixCanonicalSha256, canonicalHash(auditRaw.slice(233)));
  assert.deepEqual(refresh.appendedMigrationIds, ['2026-11-02_identity_party_reference_refresh']);
  assert.equal(refresh.reviewedPrefixCanonicalSha256WithoutCompatibleChecksum['227'],
    '3fb0233724d5f6080b6d8933e46f0cd79c34b73fd3070e3e17b742ce1ff5113e');
  // The raw manifest differs from the reviewed one in exactly that one field.
  assert.equal(rawManifest[refresh.inPrefixChange.index].id, REGISTRY_REFRESH_ENTRY);
  assert.deepEqual(rawManifest[refresh.inPrefixChange.index].compatibleAppliedChecksums, refresh.inPrefixChange.added);
  for (const [index, entry] of rawManifest.entries()) {
    if (index === refresh.inPrefixChange.index) {
      const { compatibleAppliedChecksums: _added, ...rest } = entry;
      assert.deepEqual(rest, manifest[index]);
    } else assert.deepEqual(entry, manifest[index]);
  }
  // Formal audit phase 2 (2026-11-02): six audit migrations appended after the
  // exact 227-entry memory/provider review, which stays retired and preserved.
  const main227 = history.find(entry => entry.id === 'a2bb0e828d5b94c1cb31');
  assert.equal(canonicalHash(main227),
    '2a868127eeb27cf8ffcaf4d162dd6123bf0ae8ad9ce54901c0cabb631cf43e6e');
  assert.equal(main227.evidence.valueCount, 227);
  assert.equal(main227.reviewed, true);
  assert.equal(main233.previousDecision.id, main227.id);
  assert.equal(canonicalHash(manifest.slice(0, 227)),
    JSON.parse(await fs.readFile(path.join(workspaceRoot,
      document.reconciliation.formalAuditPhase2MainIntegration20261102.document), 'utf8')).preservedPrefixCanonicalSha256);
  assert.equal(canonicalHash(memory226),
    'd08e3ff9409b47ab035fc67269d79010fd3738333fdc971c305ded3ddfeadf9b');
  assert.equal(canonicalHash(provider223),
    '6bcc3d19c2aea1adb0ce6ede68d59ec60b025b953a083bc5be6090f4be90ed1a');
  assert.deepEqual(aggregate.memoryManifestDecision, memory226);
  assert.deepEqual(aggregate.mainManifestDecision, provider223);
  assert.equal(aggregate.memoryDecisionCanonicalSha256, canonicalHash(memory226));
  assert.equal(aggregate.mainDecisionCanonicalSha256, canonicalHash(provider223));
  assert.equal(canonicalHash(manifest.slice(0, 223)),
    '3123446d4ae490b41d13beb6ca62f17da39fbc90539791c9b5373709c05b1703');
  assert.equal(aggregate.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 223)));
  assert.equal(aggregate.memorySuffixCanonicalSha256, canonicalHash(manifest.slice(223, 227)));
  assert.deepEqual(manifest[222], {
    id: '2026-10-16_provider_domain_payment_binding',
    path: 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql',
    introducedBy: '456be073c944dd3d76eb9e3c2169fe7fbb4041ef',
  });
  assert.equal(provider.mainManifestDecision.id, header222.id);
  assert.equal(provider.mainManifestDecision.reviewed, true);
  assert.equal(provider.mainManifestDecision.evidence.valueCount, 222);
  assert.equal(canonicalHash(provider.mainManifestDecision),
    '866eef0dde46336e216336f13bdf9f83570314d2c17b9cf93f354de0f8ad9404');
  assert.equal(provider.mainDecisionCanonicalSha256, canonicalHash(provider.mainManifestDecision));
  assert.equal(canonicalHash(provider.providerManifestDecision),
    '018a1dc08aafe97b3af2257fe2ad24bac9a8f47f724975df0eec07e729ba8058');
  assert.equal(provider.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 222)));
  for (const parent of [memory226, provider223]) {
    assert.equal(parent.reviewed, true);
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const retiredParents = [...historical, current, memoryParent, memory225, header222,
    combined.headerManifestDecision, memory226, provider223, provider.providerManifestDecision, main227, main233,
    audit234, sessionExpiry228, reconciled235, merged236, reviewed237, reviewed238];
  assert.equal(current.classification, decision220.classification);
  assert.equal(current.disposition, decision220.disposition);
  assert.deepEqual(current.sourceReviewReferences.slice(0, decision220.sourceReviewReferences.length),
    decision220.sourceReviewReferences);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(current.evidence.valueCount, 221);
  assert.equal(pointer.currentId, latestCurrent.id);
  assert.equal(pointer.valueCount, 253);
  assert.equal(pointer.previousResolution.valueCount, 252);
  assert.equal(pointer.previousResolution.previousResolution.valueCount, 251);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 249);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 248);
  assert.equal(pointer.change, latestCurrent.justification);
  const retirement = integration.retirements.find(entry => entry.source.name === 'migrations');
  assert.equal(retirement.replacementId, latestCurrent.id);
  assert.equal(retirement.currentResolution.id, latestCurrent.id);
  assert.equal(retirement.currentResolution.valueCount, 253);
  assert.equal(retirement.previousResolution.currentResolution.valueCount, 252);
  assert.equal(retirement.previousResolution.previousResolution.currentResolution.valueCount, 251);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 250);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 249);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 248);
  assert.equal(retirement.reason, latestCurrent.justification);
  for (const decision of retiredParents) {
    assert.ok(pointer.previousCurrentIds.includes(decision.id));
    assert.ok(retirement.previousReplacementIds.includes(decision.id));
  }

  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-alert-successor-test-'));
  try {
    const repoDir = path.join(tempRoot, 'repo');
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, 'scripts/production-migrations.json', JSON.stringify({ migrations: rawManifest }));
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

test('dated-UX-parent: UX-parent: event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
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

test('dated-UX-parent: UX-parent: reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active, successors } = await readReconciliation();
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-reconciliation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');
  const decisionsPath = path.join(tempRoot, 'decisions.json');
  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    for (const file of new Set(ledger.retirements.map(entry => entry.file))) {
      await writeFile(repoDir, file, await readPreservedParentSource('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', file));
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


test('dated-UX-parent: UX-parent: main integration archives exact old decisions and retains reviewed successors or source-removal evidence', async () => {
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

test('dated-UX-parent: UX-parent: catalog audit excludes ignored local source files from candidate fingerprints', async () => {
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

test('dated-UX-parent: UX-parent: decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
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
      // The removed review is kept as retirement provenance, so any history it
      // references stays reachable in the content-addressed store.
      sourceRemovalRetirements: [{
        reason: 'Catalog refresh unit-test-review: these candidates are no longer in the static list inventory. Their full reviewed decisions are preserved here as history; no catalog values are affected.',
        decisions: [{ ...preserved, id: 'stale-id' }],
      }],
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('dated-UX-parent: UX-parent: ticket main merge retains stale approvals only as provenance', async () => {
  const document = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
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
      assert.doesNotMatch(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-mobile/app/ddex/index.tsx', 'utf8'), /const STATUS_LABELS/);
    }
  }
});


test('dated-UX-parent: UX-parent: storage/main integration preserves both exact parent reviews and manifest views', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const manifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references');
  const { active, successors } = await readReconciliation();
  const extension = active.find(entry => entry.id === successors.find(item => item.name === 'migrations').currentId);
  const current = historicalReview(extension, 244);
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item) ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(manifest.length, 244);
  assert.equal(canonicalHash(manifest.slice(0, 239)), ledger.protectedMainPrefixCanonicalSha256);
  assert.equal(canonicalHash(manifest.filter(entry => !ledger.mainOnlyMigrationIds.includes(entry.id))), ledger.storageViewCanonicalSha256);
  assert.equal(current.evidence.valueCount, 244);
  assert.equal(successors.find(entry => entry.name === 'migrations').currentId, extension.id);
  for (const parent of ledger.parentReviews) {
    const entry = [current.previousDecision, ...current.concurrentReviewHistory].find(item => item.id === parent.id);
    assert.equal(canonicalHash(entry), parent.canonicalSha256);
    assert.ok(!active.some(item => item.id === parent.id));
  }
  assert.equal(ledger.executionAllowed, false);
});

test('dated-UX-parent: UX-parent: video campaign migration appends to the exact protected 244-entry manifest', async () => {
  const main = 'e5248e0b3ad30d832ef1ccbf5e595a7ee36b0324';
  const introduction = '4b84238abb4ca17c6aaa9902e3bec16a1cc3daa5';
  const old = JSON.parse((await execFileAsync('git', ['show', `${main}:scripts/production-migrations.json`], { cwd: workspaceRoot })).stdout).migrations;
  const current = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const priorDecisions = JSON.parse((await execFileAsync('git', ['show', `${main}:${decisionsPath}`], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout);
  const { active } = await readReconciliation(true);
  const latest = active.find(item => item.evidence?.file === manifestPath);
  const videoReview = historicalReview(latest, 245);
  assert.equal(videoReview.id, '969410f2f45854cc54fc');
  assert.deepEqual(videoReview.previousDecision,
    priorDecisions.decisions.find(item => item.id === 'be0b1503a04f9fa295ee'));
  assert.equal(old.length, 244);
  assert.equal(current.length, 246);
  assert.deepEqual(current.slice(0, 244), old);
  assert.deepEqual(current[244], { id: '2026-10-20_video_campaigns', path: 'tdf-hq/sql/2026-10-20_video_campaigns.sql', introducedBy: introduction });
  const originalSql = (await execFileAsync('git', ['show', `${introduction}:${current[244].path}`], { cwd: workspaceRoot })).stdout;
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[244].path), 'utf8'), originalSql);
  await execFileAsync('git', ['merge-base', '--is-ancestor', introduction, 'HEAD'], { cwd: workspaceRoot });
  const forward = '09fd7babb92e45c1da1b9e5017de53b6378b4f7c';
  assert.deepEqual(current[245], { id: '2026-11-06_video_campaign_identity_references', path: 'tdf-hq/sql/2026-11-06_video_campaign_identity_references.sql', introducedBy: forward });
  const priorManifest = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.deepEqual(current.slice(0, 245), priorManifest);
  const priorReview = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === videoReview.id);
  assert.deepEqual(videoReview, priorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[245].path), 'utf8'), (await execFileAsync('git', ['show', forward + ':' + current[245].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', forward, 'HEAD'], { cwd: workspaceRoot });
  // ADR 0134: marketplace offers were reviewed on top of the exact 246-entry
  // identity manifest (8ee6c7a) and now append after the two Apple entries that
  // protected main integrated; the manifest without those entries is still that
  // reviewed 247-entry manifest byte for byte.
  const marketplace = '8ee6c7a66e2504f425863ca6f5b83062723efd58';
  const marketplaceIntroduction = '5901003189dac2291380604c66fdd66087e4a587';
  const full = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(full.length, 249);
  // The agreements entry (ADR 0135) is the 251st, the last one before the
  // event administrator entry: its SQL is exactly what its introduction commit
  // carries and that commit is an ancestor.
  const agreementsEntry = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY).at(-1);
  assert.equal(agreementsEntry.id, MARKETPLACE_AGREEMENTS_ENTRY);
  assert.equal(agreementsEntry.path, 'tdf-hq/sql/2026-11-12_marketplace_agreements.sql');
  assert.equal(await fs.readFile(path.join(workspaceRoot, agreementsEntry.path), 'utf8'), (await execFileAsync('git', ['show', agreementsEntry.introducedBy + ':' + agreementsEntry.path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', agreementsEntry.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  assert.deepEqual(full[248], { id: MARKETPLACE_OFFERS_ENTRY, path: 'tdf-hq/sql/2026-11-08_marketplace_offers.sql', introducedBy: marketplaceIntroduction });
  const identityManifest = JSON.parse((await execFileAsync('git', ['show', marketplace + ':scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.equal(identityManifest.length, 247);
  // The marketplace entry's introduction moved to the review-fix commit (the
  // SQL gained the analytics allowlist and the filled -> published edge);
  // everything before it is still the reviewed identity manifest byte for byte.
  const withoutApple = full.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY);
  assert.deepEqual(withoutApple.slice(0, -1), identityManifest.slice(0, -1));
  assert.equal(withoutApple.at(-1).id, identityManifest.at(-1).id);
  const identityReview = historicalReview(latest, 246);
  assert.equal(identityReview.id, '806180f5957fc4a7520f');
  const identityPriorReview = JSON.parse((await execFileAsync('git', ['show', marketplace + ':' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === identityReview.id);
  assert.deepEqual(identityReview, identityPriorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, full[248].path), 'utf8'), (await execFileAsync('git', ['show', marketplaceIntroduction + ':' + full[248].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', marketplaceIntroduction, 'HEAD'], { cwd: workspaceRoot });
});

test('dated-UX-parent: UX-parent: Apple policy preserves the exact main prefix and every manual review', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const fromGit = async file => (await execFileAsync('git', ['show', `${main}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout;
  const oldManifest = JSON.parse(await fromGit('scripts/production-migrations.json')).migrations;
  // The marketplace offers entry (ADR 0134) appends after the Apple entries;
  // the Apple review is verified over the manifest without it.
  const currentManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(oldManifest.length, 246);
  assert.equal(currentManifest.length, 248);
  assert.deepEqual(currentManifest.slice(0, 246), oldManifest);
  const issuer = currentManifest[247];
  assert.deepEqual(issuer, { id: APPLE_ISSUER_ENTRY, path: 'tdf-hq/sql/2026-11-07_apple_provider_identity_issuer.sql', introducedBy: '7c1525edf0f02fe53f985c24862940b91c11cdfa' });
  assert.equal(await fs.readFile(path.join(workspaceRoot, issuer.path), 'utf8'), (await execFileAsync('git', ['show', `${issuer.introducedBy}:${issuer.path}`], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', issuer.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  const addition = currentManifest[246];
  assert.equal(addition.id, APPLE_POLICY_ENTRY);
  assert.equal(addition.path, 'tdf-hq/sql/2026-10-13_apple_sign_in_policy.sql');
  const original = JSON.parse((await execFileAsync('git', ['show',
    'e7ec452787e15f09deda4c4328ec6a8c1cece727:scripts/production-migrations.json'],
  { cwd: workspaceRoot })).stdout).migrations.find(row => row.id === APPLE_POLICY_ENTRY);
  assert.deepEqual(addition, original);
  assert.equal(await fs.readFile(path.join(workspaceRoot, addition.path), 'utf8'),
    (await execFileAsync('git', ['show', `${addition.introducedBy}:${addition.path}`],
      { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', addition.introducedBy, 'HEAD'],
    { cwd: workspaceRoot });
  const old = JSON.parse(await fromGit('docs/catalog-persistence/catalog-list-decisions.json')).decisions;
  // Compare the Apple review with the records as protected main 8ec7d85c held
  // them: the event administrator successors are read back to the review,
  // pointer and retirement they replaced (each nests its predecessor
  // unchanged), and lists first reviewed after that main are left out.
  const { active: currentDecisions, retired, successors: currentSuccessors,
    integration: currentIntegration } = await readReconciliation(true);
  const mainIds = new Set((await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/catalog-list-decisions.json')).decisions.map(row => row.id));
  // Later successors (the agreement payments manifest review, ADR 0136) nest
  // the event administrator one, so step back until the record that main held.
  const active = currentDecisions.flatMap(row => {
    let reviewed = row;
    while (reviewed && !mainIds.has(reviewed.id)) reviewed = reviewed.previousDecision;
    return reviewed ? [reviewed] : [];
  });
  const heldByMain = (row, idOf) => {
    let held = row;
    while (held && !mainIds.has(idOf(held))) held = held.previousResolution;
    return held;
  };
  const successors = currentSuccessors.map(row => row.name === 'migrations' ? heldByMain(row, held => held.currentId) : row);
  const integration = { ...currentIntegration, retirements: currentIntegration.retirements
    .map(row => row.source.name === 'migrations' ? heldByMain(row, held => held.replacementId) : row) };
  const reviewedParent = async revision => JSON.parse((await execFileAsync('git',
    ['show', `${revision}:docs/catalog-persistence/catalog-list-decisions.json`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  const priorApple = await reviewedParent('bc16d557c7b246361c83d78191314087ca2dd3d9');
  const currentMain = await reviewedParent('321f4fe6c8e803ab1c32724a8ce5b7814418b79c');
  const offlineMain = await reviewedParent('5c69dc30318d110d9daba042571b8e54f5bf140d');
  const offlineFixture = offlineMain.find(row => row.id === 'b3eadb9d52a262f5371f');
  assert.ok(offlineFixture, 'the offline currency fixture needs its exact protected-main review');
  assert.deepEqual(active.find(row => row.id === offlineFixture.id), offlineFixture);
  // Protected main later reviewed two route-specification lists (#710); keep
  // their exact reviews and leave them out of the historical Apple view too.
  const routeMain = await reviewedParent('b1614afd18b02e8cd0d46f71d798b7f38764bcec');
  const routeReviews = ['afe675892f43a8a4de73', '6f876292d2fcb1dd7a86']
    .map(id => routeMain.find(row => row.id === id));
  for (const review of routeReviews) {
    assert.ok(review, 'the route-specification reviews need their exact protected-main review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The desktop shell (#717) adds nine reviewed playback and desktop lists;
  // keep their exact branch reviews outside the historical Apple view as well.
  const desktopBranch = await reviewedParent('3668640fa56a558bbd3bd95881526d5faf5be22c');
  const desktopReviews = ['9f5a58afbcc5a21f3a81', '162bb73e8a27ecdbd80d', '19b53762aa84c847059a',
    'd3fe8dedc63dbee01dcf', 'dd837be242ffaf6c73c5', '70eef2bc5a9e8ef6ab65', 'ee97985b0d9abc1b1ed3',
    '57fc580c92ca2352698d', '2cf4da8b8443b9be1b48'].map(id => desktopBranch.find(row => row.id === id));
  for (const review of desktopReviews) {
    assert.ok(review, 'the desktop shell reviews need their exact branch review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The marketplace offers branch (ADR 0134, #719) adds eighteen value lists
  // reviewed by the deterministic policy, extends the search page view-mode
  // attribute list (successor of main's exact review 7393e5dd10ebe68ae3e2) and
  // records a successor review of the 249-entry manifest whose exact
  // predecessor is main's 248-entry review; the historical Apple view keeps
  // the exact predecessors and leaves the new lists out.
  const marketplaceReviewIds = ['3d30e49db2fefa10a623', '2471af5d3bf550aade51', '0365580729887c8f5ca7',
    '0dc607f91bd630b1fb87', '4ed56a39eea6e4d4679d', 'aa6a65391d2c0611fdd8',
    'b79670b0b1a1fbc1ee05', 'df679c5b2cb5d91be79e', 'ef5bffc43b9c0ff3a361', '39edb2a76ecef8cd6c57',
    '2a350b9da29a2d080bd8', 'd776481beacf4f4a074f', '16393adee900667601af', '7207f087ef5f8e0b2158',
    '3a7f374bb3b52ef8d34d', 'a9c6757fbe6f2868f4f2', '853a5f768e09d0c32259',
    // Listing-conditions labels on the public classified page (review fix round four).
    '9dd6b72c9435bd64fe09'];
  for (const id of marketplaceReviewIds) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace offers review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  // The auth_rate_limit migration (owner decisions of 2026-11-10) succeeds the
  // marketplace manifest review as the 250-entry tip and adds two reviews; the
  // historical view sets those aside and steps the manifest back twice.
  // The marketplace agreements migration (ADR 0135) succeeds the auth_rate_limit
  // review as the 251-entry tip and adds six reviews (status filter, panel
  // tabs, agreement CHECK lists and the party type); the historical view sets
  // those aside too and steps the manifest back once more.
  const agreementsTip = active.find(row => row.id === 'bc5e5bd4f9d8795d2166');
  assert.equal(agreementsTip.evidence.valueCount, 251);
  const agreementReviews = ['3a5425de732f609db717', '75f5a77be26697f0a281', '06f1e7eef25d710385f4',
    '74460721a4c7f744e3ab', '33d907b3850fb5474551', 'c9a145a03e59580950b5'];
  for (const id of agreementReviews) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace agreements review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  const authRateLimitTip = agreementsTip.previousDecision;
  assert.equal(authRateLimitTip.id, '52d96d4b31e677ac5948');
  assert.equal(authRateLimitTip.evidence.valueCount, 250);
  const authRateLimitReviews = ['f37745d92e494e02fcba', 'a95cbb8b9256407889ea'];
  const marketplaceManifestReview = authRateLimitTip.previousDecision;
  assert.equal(marketplaceManifestReview.id, '4dc5a2f26154a0f125db');
  assert.equal(marketplaceManifestReview.evidence.valueCount, 249);
  assert.equal(marketplaceManifestReview.previousDecision.id, 'a20e04bc426cc6383584');
  const searchViewReview = active.find(row => row.id === '79bdbe2ae9b350e0d1da');
  assert.equal(searchViewReview.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(searchViewReview.previousDecision.id, '7393e5dd10ebe68ae3e2');
  const marketplaceSuccessors = new Map([
    [agreementsTip.id, marketplaceManifestReview.previousDecision],
    [searchViewReview.id, searchViewReview.previousDecision]]);
  const laterMainIds = new Set([offlineFixture.id, ...routeReviews.map(row => row.id),
    ...desktopReviews.map(row => row.id), ...marketplaceReviewIds, ...authRateLimitReviews, ...agreementReviews]);
  const historicalAppleView = active.filter(row => !laterMainIds.has(row.id))
    .map(row => marketplaceSuccessors.get(row.id) ?? row);
  assert.equal(old.length, 1405);
  assert.equal(priorApple.length, 1407);
  assert.equal(currentMain.length, 1409);
  const retiredIds = new Set(retired.map(row => row.id));
  assert.equal(historicalAppleView.length, 1411 - retired.length);
  const appleView = (await reviewedParent('15e2228d24080a520d4bed187731cc3db09d474a'))
    .filter(row => !retiredIds.has(row.id));
  assert.equal(historicalAppleView.length, appleView.length);
  for (const [index, row] of appleView.entries()) {
    assert.deepEqual(historicalAppleView[index], row, `Apple review order or content changed at ${row.id}`);
  }
  assert.equal(active.length,
    1423 + marketplaceReviewIds.length + authRateLimitReviews.length + agreementReviews.length - retired.length);
  const history = new Map(retired.map(row => [row.id, row]));
  const visit = row => {
    if (!row) return;
    if (!history.has(row.id)) history.set(row.id, row);
    visit(row.previousDecision);
    for (const branch of row.concurrentReviewHistory ?? []) visit(branch);
  };
  for (const row of active) visit(row);
  for (const row of [...old, ...priorApple, ...currentMain, ...offlineMain, ...routeMain]) {
    assert.deepEqual(history.get(row.id), row);
  }
  const pointer = successors.find(row => row.name === 'migrations');
  const agreementsCurrentTip = active.find(row => row.id === pointer.currentId);
  assert.equal(agreementsCurrentTip.evidence.valueCount, 251);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').replacementId,
    agreementsCurrentTip.id);
  assert.equal(pointer.previousResolution.currentId, agreementsCurrentTip.previousDecision.id);
  const currentTip = agreementsCurrentTip.previousDecision;
  assert.equal(currentTip.evidence.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.currentId, currentTip.previousDecision.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousReplacementId,
    currentTip.id);
  const marketplaceTip = currentTip.previousDecision;
  assert.equal(marketplaceTip.evidence.valueCount, 249);
  const tip = marketplaceTip.previousDecision;
  assert.equal(tip.evidence.valueCount, 248);
  assert.equal(historicalReview(tip, 247).previousDecision.id, '806180f5957fc4a7520f');
  assert.equal(tip.previousDecision.id, '4e282e4c258854caa655');
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.currentId, tip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousReplacementId,
    marketplaceTip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousResolution.previousReplacementId,
    tip.id);
  const security = active.find(row => row.evidence?.file === 'tdf-hq/src/TDF/Catalog/Security.hs'
    && row.evidence?.name === 'expectedSecurityRoleAssignmentPolicyBindings');
  assert.equal(security.previousDecision.id, 'ff4edc0f8d2debecf71c');
  const currentSecurity = await fs.readFile(path.join(workspaceRoot, security.evidence.file), 'utf8');
  const withoutApple = currentSecurity.replace(
    '  , ("account.apple.customer", "apple-account-create", "customer", True)\n', '');
  assert.notEqual(withoutApple, currentSecurity);
  assert.equal(withoutApple, await fromGit(security.evidence.file));
});

test('dated-UX-parent: UX-parent: event administrator delegation appends one migration to the protected 251-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: EVENT_ADMIN_ENTRY, path: 'tdf-hq/sql/2026-11-11_event_admin_delegation.sql',
    introducedBy: EVENT_ADMIN_INTRODUCTION };
  assert.equal(main.migrations.length, 251);
  // The agreement payments entry (ADR 0136) appends after this one and has its own review.
  assert.deepEqual({ ...combined, migrations: combined.migrations.filter(entry => entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY) },
    { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${EVENT_ADMIN_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', EVENT_ADMIN_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, decisionsPath)).decisions;
  // The active manifest review is the agreement payments successor; this
  // review, its pointer and its retirement are nested one step back, unchanged.
  const tipPointer = successors.find(row => row.name === 'migrations');
  const pointer = tipPointer.previousResolution;
  const current = active.find(row => row.id === tipPointer.currentId).previousDecision;
  assert.equal(current.id, pointer.currentId);
  assert.equal(current.evidence.valueCount, 252);
  assert.equal(current.reviewed, true);
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 251);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations').previousResolution;
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // The typed client registry is the only other successor; it also nests its parent review.
  // EVT-TICKET-SALE-001 later added four methods: its 90-method successor keeps
  // this 86-method review intact as its previousDecision.
  const clientNow = active.find(row => row.id === successors.find(row => row.name === 'SocialEventsAPI').currentId);
  assert.equal(clientNow.evidence.valueCount, 90);
  const client = clientNow.previousDecision;
  assert.equal(client.id, '083bd1a66e9f23f02866');
  assert.equal(client.evidence.valueCount, 86);
  assert.deepEqual(client.previousDecision, priorMain.find(row => row.id === '2511cbba2fc7279bc28a'));
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const movedReviews = assertMovedListsNest(active, priorMain);
  // Every other review of protected main is retained exactly; three lists are new.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, client.previousDecision.id, ...movedReviews]);
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else assert.deepEqual(history.get(row.id), row);
  }
  assert.equal(priorMain.length, 1449);
  // 1452 after this review, plus the nine agreement payments reviews (ADR 0136),
  // less the reviews retired with their deleted source lists; the delivery
  // protocol review (SYS-VERIFY-002) is counted separately.
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});

test('dated-UX-parent: UX-parent: marketplace agreement payments append one migration to the protected 252-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY,
    path: 'tdf-hq/sql/2026-11-12_marketplace_agreement_payments.sql',
    introducedBy: AGREEMENT_PAYMENTS_INTRODUCTION };
  assert.equal(main.migrations.length, 252);
  assert.deepEqual(combined, { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${AGREEMENT_PAYMENTS_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', AGREEMENT_PAYMENTS_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, decisionsPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const current = active.find(row => row.id === pointer.currentId);
  assert.equal(current.evidence.valueCount, 253);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 252);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.equal(pointer.change, current.justification);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, current.id);
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // Every other review of protected main is retained exactly; nine lists are
  // new (payment provider, fee payer, payout and payment CHECK lists and the
  // two Policy sum types) and reviewed by the deterministic policy.
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, ...assertMovedListsNest(active, priorMain)]);
  // The 86-method client review was since superseded by EVT-TICKET-SALE-001
  // and is retained exactly as the previousDecision of its successor.
  const clientSuccessor = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else if (row.id === '083bd1a66e9f23f02866') assert.deepEqual(clientSuccessor.previousDecision, row);
    else assert.deepEqual(history.get(row.id), row);
  }
  const payoutReviews = ['d580ad14a8f01218c4a9', 'fbe5e4e4d0289e1caaf4', 'cfa5a05214e95ed07ef0',
    '203395cfa370d78f86a1', '1f6946049f264576684d'];
  const chargeReviews = ['09d7dc6ebc55642a4bb2', '584b9609b82ad71578ed', 'e1a25b160e801cf866df',
    '32574b309f1d005ed165'];
  for (const id of [...payoutReviews, ...chargeReviews]) {
    assert.ok(!priorMain.some(row => row.id === id));
    assert.ok(history.get(id)?.reviewed, `the marketplace agreement payments review ${id} must be present and reviewed`);
    assert.match(history.get(id).reviewMethod, /^deterministic-policy-v\d+$/);
  }
  assert.equal(priorMain.length, 1452);
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});


test('dated-UX-parent: UX-parent: player server preserves both reviewed migration histories and appends only reviewed SQL', async () => {
  const main = await reviewedJson(PLAYER_SERVER_MAIN_BASE, manifestPath);
  const player = await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, manifestPath);
  const current = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', manifestPath, 'utf8'));
  const extension = player.migrations.filter(row => playerMigrations.has(row.id));
  assert.deepEqual(extension.map(row => row.id), [...playerMigrations]);
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...extension] });
  for (const row of extension) {
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'],
      { cwd: workspaceRoot });
    const { stdout } = await execFileAsync('git', ['show', `${row.introducedBy}:${row.path}`],
      { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
  }

  const { active, successors, integration } = await readReconciliation();
  const pointer = successors.find(row => row.name === 'migrations');
  const actual = active.find(row => row.id === pointer.currentId);
  const findTip = doc => doc.decisions.find(row => row.evidence?.file === manifestPath);
  const mainReview = findTip(await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath));
  const playerReview = findTip(await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, catalogPath));
  assert.deepEqual(actual.previousDecision, mainReview);
  assert.deepEqual(actual.concurrentReviewHistory, [playerReview]);
  assert.equal(actual.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, actual.id);
  assert.equal(retirement.previousResolution.replacementId, mainReview.id);
});

test('dated-UX-parent: UX-parent: loop rework retires the reviews of two deleted scripts as provenance and reviews one new list', async () => {
  const { active } = await readReconciliation();
  const rework = await readLoopRework();
  assert.equal(rework.decisions.length, 2);
  assert.ok(rework.reason.length > 80);
  for (const entry of rework.decisions) {
    assert.equal(entry.decision.reviewed, true);
    assert.ok(!active.some(row => row.id === entry.decision.id));
    await assert.rejects(fs.access(path.join(workspaceRoot, entry.source.file)), { code: 'ENOENT' });
  }
  assert.deepEqual(rework.addedDecisionIds, ['16e3988d48089cc77b7e']);
  for (const id of rework.addedDecisionIds) {
    assert.equal(active.find(row => row.id === id)?.reviewed, true);
  }
  // The earlier view re-inserts the retired reviews and drops the new one; the catalog
  // itself keeps growing with main, so only the difference is fixed.
  const historical = (await readHistoricalReconciliation()).active;
  assert.equal(historical.length, active.length + rework.decisions.length - rework.addedDecisionIds.length);
  for (const entry of rework.decisions) assert.deepEqual(historical[entry.position], entry.decision);
});

// One explicit review classifies the delivery protocol; it is new, unique and
// carries its requirement. The rest of the catalog follows main.
test('dated-UX-parent: UX-parent: delivery evidence protocol adds exactly one reviewed list', async () => {
  const current = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', catalogPath, 'utf8'));
  const baseline = await reviewedJson('5084d8a8a4d7a7a3b8d30d1f55ae5d9c8c032b9b', catalogPath);
  assert.ok(!baseline.decisions.some(row => row.id === DELIVERY_PROTOCOL_REVIEW));
  const additions = current.decisions.filter(row => row.id === DELIVERY_PROTOCOL_REVIEW);
  assert.equal(additions.length, 1);
  assert.equal(additions[0].classification, 'genuine-technical-constant');
  assert.equal(additions[0].disposition, 'explicit-allowlist');
  assert.equal(additions[0].reviewed, true);
  assert.equal(additions[0].evidence.file, 'scripts/lib/delivery-matrix.mjs');
  assert.equal(additions[0].evidence.name, 'DELIVERY_CHECKS');
  assert.match(additions[0].justification, /SYS-VERIFY-002/);
  assert.equal(current.decisions.filter(row => row.evidence?.file === 'scripts/lib/delivery-matrix.mjs').length, 1);
});

const reviewIndexes = new WeakMap();
function wholeReview(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  let index = reviewIndexes.get(value);
  if (!index) {
    index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      if (node.id) index.set(node.id, [...(index.get(node.id) ?? []), node]);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); reviewIndexes.set(value, index);
  }
  return (index.get(expected.id) ?? []).find(row => canonicalReview(row) === canonicalReview(expected));
}

const canonicalReview = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findWholeResolution(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if ((Object.hasOwn(expected, 'currentId') && value.currentId === expected.currentId || Object.hasOwn(expected, 'replacementId') && value.replacementId === expected.replacementId) && canonicalReview(value) === canonicalReview(expected)) return value;
  for (const child of Object.values(value)) {
    const found = findWholeResolution(child, expected);
    if (found) return found;
  }
}
const INTEGRATION_MAIN = '966fb55dca70a49c0a340b674f8cc133cc413a2d';
const UX_PARENT = 'e4ae39edb7924e9646aad4535036aeb39fb5dd7c';

async function uxParentReconciliation() {
  const register = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', catalogPath, 'utf8'));
  const parent = await reviewedJson(UX_PARENT, catalogPath);
  const currentActive = parent.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Complete UX parent review missing: ${row.id}`);
    return found;
  });
  // Each presentation follow-up has its own complete-parent preservation test.
  // Historical reconciliation tests replay their exact earlier reviewed view.
  const settlementDisplayView = currentActive.flatMap(row => {
    if (row.id === '734ed3533d72f55f1170' || row.id === '95da3806a32123c6e1a5' || row.id === '354146cb038857137555') return [];
    return [row.id === '295c31c74b0bad95c5c7' ? row.previousDecision : row];
  });
  const labelExtractionView = settlementDisplayView.filter(row =>
    row.id !== 'f42409396fc972bf4d31' && row.id !== '399cc2b7d2c27f5143c1');
  const active = labelExtractionView.flatMap(row => {
    if (row.id === 'e4eadc75e199657bbd8a') return [];
    if (row.id === '91c81c283f19a583aa11' || row.id === 'edd06ad23b93dfcc632a') return [row.previousDecision];
    return [row];
  });
  return { active, currentActive, settlementDisplayView, labelExtractionView };
}

test('dated-UX-parent: UX-parent: shared evidence/request display retains the full parent view and existing SQL/API vocabulary', async () => {
  const { currentActive, settlementDisplayView } = await uxParentReconciliation();
  const parent = JSON.parse((await execFileAsync('git', ['show',
    'a4edbe4b107e2f37b1091e02bac0d75fac472554:docs/catalog-persistence/catalog-list-decisions.json'],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  assert.deepEqual(settlementDisplayView, parent, 'all1432 complete parent reviews remain reconstructible unchanged');
  assert.equal(currentActive.length, parent.length + 3);
  const status = currentActive.find(row => row.id === '295c31c74b0bad95c5c7');
  assert.deepEqual(status.previousDecision, parent.find(row => row.id === '31fdec804cc113595b2f'));
  assert.equal(status.classification, status.previousDecision.classification);
  assert.equal(status.disposition, status.previousDecision.disposition);
  const labels = await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq-ui/src/utils/marketplaceStatusLabel.ts', 'utf8');
  const oldPage = (await execFileAsync('git', ['show',
    'a4edbe4b107e2f37b1091e02bac0d75fac472554:tdf-hq-ui/src/pages/MarketplaceOrderTrackingPage.tsx'],
    { cwd: workspaceRoot })).stdout;
  const body = (text, name) => text.match(new RegExp(String.raw`const ${name}: [^=]+ = \{([\s\S]*?)\n\};`))?.[1];
  assert.ok(body(labels, 'CUSTOMER_REQUEST_TYPE_LABELS'));
  assert.ok(body(labels, 'CUSTOMER_REQUEST_STATUS_LABELS'));
  assert.equal(body(labels, 'CUSTOMER_REQUEST_TYPE_LABELS'), body(oldPage, 'requestLabels'));
  assert.equal(body(labels, 'CUSTOMER_REQUEST_STATUS_LABELS'), body(oldPage, 'requestStatusLabels'));
  const requestSchema = (await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement.sql', 'utf8'))
    .split('CREATE TABLE IF NOT EXISTS marketplace_customer_request (')[1];
  const manualSchema = (await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq/sql/2026-08-13_unified_checkout_core.sql', 'utf8'))
    .split('CREATE TABLE IF NOT EXISTS commerce_manual_payment_evidence (')[1];
  const types = await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq-ui/src/api/types.ts', 'utf8');
  const rows = currentActive.filter(row => row.reviewBatch === 'cognitive-load-evidence-request-display-20261119');
  assert.equal(rows.length, 4);
  for (const row of rows) {
    let values;
    if (row.evidence.name === 'CUSTOMER_REQUEST_TYPE_LABELS') values = requestSchema.match(/request_type TEXT NOT NULL CHECK \(request_type IN \(([\s\S]*?)\)\)/)[1];
    else if (row.evidence.name === 'CUSTOMER_REQUEST_STATUS_LABELS') values = requestSchema.match(/status TEXT NOT NULL DEFAULT 'submitted' CHECK \(status IN \(([\s\S]*?)\)\)/)[1];
    else if (row.evidence.name === 'MANUAL_EVIDENCE_STATUS_LABELS') values = manualSchema.match(/status TEXT NOT NULL CHECK \(status IN \(([\s\S]*?)\)\)/)[1];
    else values = types.match(/mmePaymentMethod:([^;]+);/)[1];
    assert.deepEqual(row.evidence.values, [...values.matchAll(/'([^']+)'/g)].map(match => match[1]));
    assert.equal(row.classification, 'dynamic-business-catalog');
  }
});


test('dated-UX-parent: UX-parent: staff settlement display preserves every parent review and only labels existing SQL codes', async () => {
  const { settlementDisplayView: currentActive, labelExtractionView } = await uxParentReconciliation();
  const parent = JSON.parse((await execFileAsync('git', ['show',
    'f11327af1ad809147f23b50e988dcc631d416836:docs/catalog-persistence/catalog-list-decisions.json'],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  assert.deepEqual(labelExtractionView, parent, 'all1430 complete parent reviews must remain byte-for-byte equivalent');
  assert.equal(currentActive.length, parent.length + 2);
  const schema = (await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement.sql', 'utf8'))
    .split('CREATE TABLE IF NOT EXISTS marketplace_rental_deposit_settlement (')[1];
  assert.ok(schema, 'the canonical settlement schema must exist');
  const rows = currentActive.filter(row => row.id === 'f42409396fc972bf4d31' || row.id === '399cc2b7d2c27f5143c1');
  assert.equal(rows.length, 2);
  for (const row of rows) {
    assert.equal(row.classification, 'dynamic-business-catalog');
    assert.equal(row.reviewMethod, 'individual-source-review');
    assert.equal(row.evidence.file, 'tdf-hq-ui/src/utils/marketplaceStatusLabel.ts');
    const pattern = row.evidence.name === 'DEPOSIT_SETTLEMENT_STATUS_LABELS'
      ? /status TEXT NOT NULL DEFAULT 'submitted' CHECK \(status IN \(([\s\S]*?)\)\)/
      : /settlement_method TEXT NOT NULL CHECK \(settlement_method IN \(([\s\S]*?)\)\)/;
    const allowed = [...schema.match(pattern)[1].matchAll(/'([^']+)'/g)].map(match => match[1]);
    assert.deepEqual(row.evidence.values, allowed);
    assert.equal(row.evidence.valueCount, 4);
  }
});


test('dated-UX-parent: UX-parent: marketplace label extraction preserves all complete reviews and the backend deposit vocabulary', async () => {
  const { active, labelExtractionView: currentActive } = await uxParentReconciliation();
  const parent = JSON.parse((await execFileAsync('git',
    ['show', '4669d12deaf891f31a78f4d49140d59d260e50da:docs/catalog-persistence/catalog-list-decisions.json'],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  assert.deepEqual(active, parent, 'every prior complete decision survives unchanged in the current review history');
  assert.equal(currentActive.length, parent.length + 1);
  const labels = await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq-ui/src/utils/marketplaceStatusLabel.ts', 'utf8');
  const oldPage = (await execFileAsync('git', ['show',
    '4669d12deaf891f31a78f4d49140d59d260e50da:tdf-hq-ui/src/pages/MarketplaceOrdersPage.tsx'],
    { cwd: workspaceRoot })).stdout;
  for (const row of currentActive.filter(row => row.id === '91c81c283f19a583aa11' || row.id === 'edd06ad23b93dfcc632a')) {
    assert.equal(row.reviewMethod, 'successor-of-reviewed-decision');
    assert.equal(row.classification, row.previousDecision.classification);
    assert.equal(row.disposition, row.previousDecision.disposition);
    assert.equal(row.evidence.file, 'tdf-hq-ui/src/utils/marketplaceStatusLabel.ts');
    const pattern = new RegExp(String.raw`const ${row.evidence.name}: Record<string, string> = \{([\s\S]*?)\n\};`);
    assert.equal(labels.match(pattern)?.[1], oldPage.match(pattern)?.[1]);
    assert.ok(labels.match(pattern), 'the complete copied map must exist in the shared source');
  }
  const deposit = currentActive.find(row => row.id === 'e4eadc75e199657bbd8a');
  assert.equal(deposit.reviewMethod, 'individual-source-review');
  assert.equal(deposit.classification, 'dynamic-business-catalog');
  const schema = await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', 'tdf-hq/sql/2026-08-15_marketplace_rental_checkout_runtime.sql', 'utf8');
  const allowed = [...schema.match(/deposit_status TEXT NOT NULL CHECK \(deposit_status IN \(([\s\S]*?)\)\)/)[1].matchAll(/'([^']+)'/g)].map(match => match[1]);
  assert.deepEqual(deposit.evidence.values, allowed, 'display covers only the already-persisted deposit states');
});


test('dated-UX-parent: UX-parent: dated UX parent preserves its explicit form, label, category and booking reviews', async () => {
  const { active } = await uxParentReconciliation();
  const reviewedParent = async revision => (await reviewedJson(revision, catalogPath)).decisions;
  const offlineFixture = active.find(row => row.id === 'b3eadb9d52a262f5371f');
  const routeReviews = ['afe675892f43a8a4de73', '6f876292d2fcb1dd7a86'].map(id => active.find(row => row.id === id));
  const desktopReviews = (await reviewedParent('3668640fa56a558bbd3bd95881526d5faf5be22c')).filter(row => ['9f5a58afbcc5a21f3a81','162bb73e8a27ecdbd80d','19b53762aa84c847059a','d3fe8dedc63dbee01dcf','dd837be242ffaf6c73c5','70eef2bc5a9e8ef6ab65','ee97985b0d9abc1b1ed3','57fc580c92ca2352698d','2cf4da8b8443b9be1b48'].includes(row.id));
  const priceFixture = active.find(row => row.id === '4255c54da850af675134');
  assert.ok(priceFixture, 'the service-price form fixture needs its explicit structural review');
  assert.equal(priceFixture.classification, 'genuine-technical-constant');
  assert.equal(priceFixture.evidence.file, 'tdf-hq-ui/src/pages/ServiceTypesPage.form.test.tsx');
  const eventPriceFixture = active.find(row => row.id === '7eedc6e7af967d6f5e56');
  assert.ok(eventPriceFixture, 'the event-money fixture needs its structural source review');
  assert.equal(eventPriceFixture.classification, 'genuine-technical-constant');
  assert.equal(eventPriceFixture.evidence.file, 'tdf-hq-ui/src/pages/SocialEventsPage.money-input.test.tsx');
  assert.equal(eventPriceFixture.evidence.valueCount, 6);
  const merchLabelReviews = ['32d2939cc4dcd34d3c1e', '38c31c0f6ac4d4fafd4b', 'ddb74ea1ebe3db43e723']
    .map(id => active.find(row => row.id === id));
  for (const review of merchLabelReviews) {
    assert.ok(review, 'each localized financial label pair needs an individual source review');
    assert.equal(review.classification, 'security-system-registry');
    assert.equal(review.reviewMethod, 'individual-source-review');
    assert.equal(review.evidence.file, 'tdf-hq-ui/src/utils/merch.ts');
    assert.equal(review.evidence.valueCount, 2);
  }
  // The category-label consumer adds one separately reviewed presentation map.
  // Preserve the complete pre-existing decision objects, then project it out of
  // the historical financial-label and Apple-policy views.
  const categoryId = '7d5d1e64b79af7288982';
  const categoryParent = await reviewedParent('7aff2e236d3a73d3d00a22dc57a533dcc4efa0c2');
  assert.deepEqual(active.find(row => row.id === categoryId), categoryParent.find(row => row.id === categoryId));
  assert.deepEqual(active, categoryParent);
  const merchLabelIds = new Set([...merchLabelReviews.map(row => row.id), categoryId]);
  assert.deepEqual(active.filter(row => !merchLabelIds.has(row.id)),
    await reviewedParent('cfbd8d66447585ef4b178c1ea76004b188cdad46'));
  const laterMainIds = new Set([offlineFixture.id, ...routeReviews.map(row => row.id),
    ...desktopReviews.map(row => row.id), priceFixture.id, eventPriceFixture.id, ...merchLabelIds]);
  // The cognitive-load continuation combines two booking presentation steps.
  // Its successor must preserve the exact earlier classification and review,
  // including in this historical Apple-policy projection.
  const bookingReview = active.find(row => row.id === '6a832158d7d69c8fcc7e');
  const bookingParent = await reviewedParent('147d83c9c12ccceb65bac7f2353522bbdebb33ca');
  assert.ok(bookingReview, 'the two-step booking list needs its successor review');
  assert.deepEqual(bookingReview.previousDecision,
    bookingParent.find(row => row.id === '528ebad9929ca2769837'));
  const historicalAppleView = active.filter(row => !laterMainIds.has(row.id))
    .map(row => row.id === bookingReview.id ? bookingReview.previousDecision : row);
  assert.equal(historicalAppleView.length, 1411);
  assert.deepEqual(historicalAppleView,
    await reviewedParent('15e2228d24080a520d4bed187731cc3db09d474a'));
  assert.equal(active.length, 1429);
});

test('dated-UX-parent: UX-parent: current integration preserves every complete main and UX parent review', async () => {
  const register = JSON.parse(await readPreservedParentFile('2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485', catalogPath, 'utf8'));
  for (const revision of [INTEGRATION_MAIN, UX_PARENT]) {
    const parent = await reviewedJson(revision, catalogPath);
    for (const row of parent.decisions) assert.ok(wholeReview(register, row), `${revision}: ${row.id}`);
  }
});
}
const composedRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const composedExec = promisify(execFile);
const composedCache = new Map();
const preservedIndexes = new WeakMap();
const composedCurrentFiles = new Map();
const preservedFiles = new Map([
  ['docs/catalog-persistence/catalog-list-decisions.json', 'decisions'],
  ['docs/catalog-persistence/event-catalog-current-successors.json', 'successors'],
  ['docs/catalog-persistence/event-integration-retirements.json', 'retirements'],
]);
const canonicalComposed = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findPreserved(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if (!preservedIndexes.has(value)) {
    const index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      index.set(canonicalComposed(node), node);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); preservedIndexes.set(value, index);
  }
  return preservedIndexes.get(value).get(canonicalComposed(expected));
}
async function composedParent(ref, file) {
  const key = `${ref}:${file}`;
  if (!composedCache.has(key)) composedCache.set(key, composedExec('git', ['show', key],
    { cwd: composedRoot, maxBuffer: 64 * 1024 * 1024 }).then(({ stdout }) => JSON.parse(stdout)));
  return composedCache.get(key);
}
async function readPreservedParentFile(ref, file, encoding) {
  const actualBytes = await integrationRead('b99ce783caa8446920c5950f5111865e7f05cfa3', file, encoding);
  if (file === 'scripts/production-migrations.json' || preservedFiles.has(file)) {
    if (!composedCurrentFiles.has(file)) composedCurrentFiles.set(file, { bytes: actualBytes, value: JSON.parse(actualBytes) });
    assert.equal(composedCurrentFiles.get(file).bytes, actualBytes, `Composed source changed during parent assertions: ${file}`);
  }
  if (file === 'scripts/production-migrations.json') {
    const actual = composedCurrentFiles.get(file).value; const parent = await composedParent(ref, file);
    return JSON.stringify({ ...parent, migrations: parent.migrations.map(row => {
      const found = actual.migrations.find(item => item.id === row.id);
      assert.deepEqual(found, row, `Dated migration changed: ${ref}:${row.id}`); return found;
    }) });
  }
  const key = preservedFiles.get(file);
  if (!key) return actualBytes;
  const actual = composedCurrentFiles.get(file).value; const parent = await composedParent(ref, file);
  return JSON.stringify({ ...parent, [key]: parent[key].map(row => {
    const found = findPreserved(actual, row);
    assert.ok(found, `Lost whole parent review/resolution: ${ref}:${file}:${row.id ?? row.name}`);
    return found;
  }) });
}

async function readPreservedParentSource(ref, file) {
  // Historical negative controls are tied to their original source revision.
  // This function cannot supply approvals to the current-source audit.
  if (file.startsWith('tdf-mobile/')) {
    const { stdout } = await composedExec('git', ['ls-tree', ref, 'tdf-mobile'], { cwd: composedRoot });
    const pin = stdout.match(/^160000 commit ([0-9a-f]{40})\t/); assert.ok(pin);
    return (await composedExec('git', ['show', `${pin[1]}:${file.slice('tdf-mobile/'.length)}`],
      { cwd: path.join(composedRoot, 'tdf-mobile'), maxBuffer: 64 * 1024 * 1024 })).stdout;
  }
  return (await composedExec('git', ['show', `${ref}:${file}`],
    { cwd: composedRoot, maxBuffer: 64 * 1024 * 1024 })).stdout;
}

const COMPOSED_PARENTS = ["2c6dd64b9bd28d3fec6ac8d961eddea338d448d5", "2e7c5dd67cd0cca63a66a4cd2bc02ccd2a716485"];

test('dated-UX-parent: actual UX/main integration preserves the complete main schema, immutable SQL and every whole parent review', async () => {
  const file = 'scripts/production-migrations.json';
  const actual = JSON.parse(await integrationRead('b99ce783caa8446920c5950f5111865e7f05cfa3', file, 'utf8'));
  const [main, ux] = await Promise.all(COMPOSED_PARENTS.map(ref => composedParent(ref, file)));
  assert.deepEqual(actual, main);
  assert.deepEqual(actual.migrations.slice(0, ux.migrations.length), ux.migrations);
  assert.equal(actual.migrations.length, 259);
  assert.equal(new Set(actual.migrations.map(row => row.id)).size, 259);
  for (const row of actual.migrations) {
    const { stdout } = await composedExec('git', ['show', `${COMPOSED_PARENTS[0]}:${row.path}`],
      { cwd: composedRoot, maxBuffer: 32 * 1024 * 1024 });
    assert.equal(await fs.readFile(path.join(composedRoot, row.path), 'utf8'), stdout, `Immutable SQL changed: ${row.id}`);
    await composedExec('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: composedRoot });
  }
  for (const [reviewFile, key] of preservedFiles) {
    const current = JSON.parse(await integrationRead('b99ce783caa8446920c5950f5111865e7f05cfa3', reviewFile, 'utf8'));
    for (const ref of COMPOSED_PARENTS) {
      const prior = await composedParent(ref, reviewFile);
      for (const row of prior[key]) assert.ok(findPreserved(current, row), `Lost whole parent object: ${ref}:${reviewFile}`);
      for (const [field, value] of Object.entries(prior)) {
        if (field === key) continue;
        if (value && typeof value === 'object') assert.ok(findPreserved(current, value), `Lost whole parent metadata: ${ref}:${reviewFile}:${field}`);
        else assert.ok(current[field] === value || current.sourceContextHistory?.some(entry => entry.metadata?.[field] === value),
          `Lost parent metadata: ${ref}:${reviewFile}:${field}`);
      }
    }
  }
});
}

// Complete retained parent assertions; dated inputs are explicitly projected.
{

// Retained complete assertions from d65017d10f16e8089c43b82bdbcacf3177d206f5; dated inputs are explicitly projected.
{

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

// 2026-11-02 registry refresh: the only field ever added inside an already
// reviewed manifest prefix is the compatible applied checksum of the
// regenerated 2026-09-18 identity registry. Historical prefix hashes are taken
// over the manifest as it was reviewed, i.e. without that field.
const REGISTRY_REFRESH_ENTRY = '2026-09-18_identity_review_dependencies';
// 2026-11-02 session-expiry integration: protected main's migration is
// interleaved at index 227, ahead of the seven audit migrations. The audit's
// earlier reviews are verified over the manifest without that entry.
const SESSION_EXPIRY_ENTRY = '2026-10-31_api_token_session_expiry';
const APPLE_POLICY_ENTRY = '2026-10-13_apple_sign_in_policy';
const APPLE_ISSUER_ENTRY = '2026-11-07_apple_provider_identity_issuer';
// 2026-11-08 marketplace offers (ADR 0134) append after the Apple issuer entry.
const MARKETPLACE_OFFERS_ENTRY = '2026-11-08_marketplace_offers';
// 2026-11-10 auth rate limit (owner decisions of the formal audit) appends last.
const AUTH_RATE_LIMIT_ENTRY = '2026-11-10_auth_rate_limit';
// 2026-11-12 marketplace agreements (ADR 0135) append after the auth_rate_limit entry.
const MARKETPLACE_AGREEMENTS_ENTRY = '2026-11-12_marketplace_agreements';
// 2026-11-11 delegated event administrators: one migration appended last to the
// 251-entry manifest of protected main 8ec7d85c. Earlier reviews are verified
// over the manifest without that entry.
const EVENT_ADMIN_ENTRY = '2026-11-11_event_admin_delegation';
const EVENT_ADMIN_PROTECTED_MAIN = '8ec7d85c672e844e28c38e49c087b102032582d8';
const EVENT_ADMIN_INTRODUCTION = '2703eb3a1ac1604829e902262fe2dbfbbf4f4ca8';
// The marketplace agreement payments migration (ADR 0136) appends one entry to
// the 252-entry manifest of protected main d4bb9603. Earlier reviews are
// verified with the records stepped back to the main they reviewed.
const MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY = '2026-11-12_marketplace_agreement_payments';
const AGREEMENT_PAYMENTS_PROTECTED_MAIN = 'd4bb96030b1182e165ff39b614315c6a2554aee7';
const AGREEMENT_PAYMENTS_INTRODUCTION = '26c83d9e6525d859b0da0bebf959ad6d1c806159';

async function reviewedJson(revision, file) {
  const { stdout } = await execFileAsync('git', ['show', `${revision}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 });
  return JSON.parse(stdout);
}
const historicalReview = (entry, count) => {
  while (entry && entry.evidence?.valueCount !== count) entry = entry.previousDecision;
  assert.ok(entry, `Missing exact reviewed ${count}-entry predecessor`);
  return entry;
};
const auditView = entries => entries.filter(entry => entry.id !== SESSION_EXPIRY_ENTRY);
const asReviewedBeforeRegistryRefresh = entries => entries.map(entry => {
  if (entry.id !== REGISTRY_REFRESH_ENTRY) return entry;
  const { compatibleAppliedChecksums, ...reviewed } = entry;
  assert.deepEqual(compatibleAppliedChecksums,
    ['5636028805b77a1705bb364c87669e8d46983c93d9fc177da9e2187b98972f4f']);
  return reviewed;
});

const DELIVERY_PROTOCOL_REVIEW = '755cf14ac92e4f590879';
const PLAYER_SERVER_MAIN_BASE = '2ebb5c38bef08f3205877dc9521026b40b68d4ef';
const PLAYER_SERVER_REVIEW_HEAD = '85255658262166ab4aeb0e7e167b3a93c4a33d61';
const manifestPath = 'scripts/production-migrations.json';
const catalogPath = 'docs/catalog-persistence/catalog-list-decisions.json';
const playerMigrations = new Set([
  '2026-10-30_artist_featured_track',
  '2026-11-06_music_media_health',
  '2026-11-06_music_legacy_source_compatibility',
  '2026-11-07_music_featured_source_health',
  '2026-11-07_artist_featured_identity_guard',
]);

async function manifestBeforePlayer() {
  const manifest = (await mainManifestView());
  return { ...manifest, migrations: manifest.migrations.filter(row => !playerMigrations.has(row.id)) };
}

// Lists whose source file moved without any change to their values (ADR 0133).
// A list id follows its file, so each move is recorded as a successor review
// that nests the exact earlier review; checks against an earlier main read the
// successor back to that review.
const MOVED_FILE_REVIEW_BATCHES = new Set([
  'paypal-webhook-domain-module-2026-11-10',
  'web-auth-state-layer-2026-11-17',
  'web-feature-registry-layer-2026-11-17',
  'web-campaign-cart-layer-2026-11-19',
]);

function movedFileSuccessors(active) {
  const moved = active.filter(row => MOVED_FILE_REVIEW_BATCHES.has(row.reviewBatch));
  for (const row of moved) {
    assert.equal(row.reviewMethod, 'successor-of-reviewed-decision');
    assert.ok(row.previousDecision?.reviewed, `moved list ${row.id} must nest a reviewed decision`);
    assert.equal(row.classification, row.previousDecision.classification);
    assert.equal(row.disposition, row.previousDecision.disposition);
  }
  return moved;
}

// Every moved list whose earlier review protected main held nests that exact review.
function assertMovedListsNest(active, priorMain) {
  const prior = new Map(priorMain.map(row => [row.id, row]));
  return movedFileSuccessors(active).filter(row => prior.has(row.previousDecision.id)).map(row => {
    assert.deepEqual(row.previousDecision, prior.get(row.previousDecision.id));
    return row.previousDecision.id;
  });
}

// The continuous improvement loop rework (pull request #750) deleted two automation
// scripts: their reviews are retired as provenance and one new list is reviewed.
const LOOP_REWORK_RECONCILIATION = 'continuousImprovementLoopRework20261113';

async function readLoopRework() {
  return JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8')).reconciliation[LOOP_REWORK_RECONCILIATION];
}

async function readHistoricalReconciliation() {
  const current = await readReconciliation();
  const rework = await readLoopRework();
  const added = new Set(rework.addedDecisionIds);
  const active = current.active.filter(row => !added.has(row.id));
  for (const entry of [...rework.decisions].sort((left, right) => left.position - right.position)) {
    active.splice(entry.position, 0, entry.decision);
  }
  return { ...current, active };
}

async function readReconciliation(beforePlayer = false) {
  const ledger = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/event-catalog-retirements.json', 'utf8'));
  const register = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const mainRegister = await reviewedJson(INTEGRATION_MAIN, catalogPath);
  const active = mainRegister.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Dated main review missing: ${row.id}`);
    return found;
  });
  // Reviews whose source list was deleted outright (no successor list). They
  // leave the active register and are kept, unchanged, as history.
  const retired = (register.reconciliation.sourceRemovalRetirements ?? []).flatMap(entry => entry.decisions);
  let successors = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/event-catalog-current-successors.json', 'utf8')).successors;
  let integration = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/event-integration-retirements.json', 'utf8'));
  const parentPointers = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-catalog-current-successors.json');
  successors = parentPointers.successors.map(row => {
    const found = findWholeResolution(successors, row);
    assert.ok(found, `Dated main resolution missing: ${row.name}`);
    return found;
  });
  const parentRetirements = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-integration-retirements.json');
  integration = { ...integration, retirements: parentRetirements.retirements.map(row => {
    const found = findWholeResolution(integration, row);
    assert.ok(found, `Dated main retirement missing: ${row.source.name}`);
    return found;
  }) };
  if (!beforePlayer) return { ledger, active, retired, successors, integration };

  // Earlier reviews retain their own manifest fingerprint and catalog size.
  const predecessor = (await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const tip = active.find(row => row.id === pointer.currentId);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  const byId = new Map(active.map(row => row === tip ? row.previousDecision : row)
    .map(row => [row.id, row]));
  // A moved list's successor stands in for the review it nests.
  for (const row of movedFileSuccessors(active)) byId.set(row.previousDecision.id, row);
  // EVT-TICKET-SALE-001 extended the typed client registry; its successor
  // review nests the 86-method review unchanged and stands in for it.
  const ticketSaleClient = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  byId.set(ticketSaleClient.previousDecision.id, ticketSaleClient);
  // The loop rework (pull request #750) retired two reviews of deleted scripts as
  // provenance; the earlier catalog still lists them, so they stand in for themselves.
  for (const entry of (await readLoopRework()).decisions) byId.set(entry.decision.id, entry.decision);
  return {
    ledger,
    retired,
    active: predecessor.map(row => byId.get(row.id)).filter(Boolean),
    successors: successors.map(row => row === pointer ? row.previousResolution : row),
    integration: { ...integration, retirements: integration.retirements
      .map(row => row === retirement ? row.previousResolution : row) },
  };
}

test('dated-storage-parent: refund-parent: music/main reconciliation retains both reviews and the protected migration prefix', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/music-main-reconciliation.json', 'utf8'));
  const { active, successors, integration } = await readReconciliation(true);
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
  const manifest = asReviewedBeforeRegistryRefresh((await mainManifestView()).migrations);
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
    const extension = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/public-ux-main-reconciliation-20261021.json', 'utf8'));
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

test('dated-storage-parent: refund-parent: memory226 and ledger succession preserve213/214/220/221/222/224/225 reviews and reject stale approvals', async () => {
  const document = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const transition = document.reconciliation.directorySavedSearchParity;
  const financial = document.reconciliation.directoryFinancialMainIntegration;
  const { active, successors, integration } = await readReconciliation(true);
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
  const storageMerge = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const rawManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references' && !storageMerge.storageMigrationIds.includes(entry.id));
  const manifest = asReviewedBeforeRegistryRefresh(rawManifest);
  assert.equal(canonicalHash(manifest.slice(0, 213)), protectedPrefixHash);
  assert.equal(transition.protectedMainManifestCanonicalSha256, protectedPrefixHash);
  const protectedFinancialPrefixHash = 'c025cf3d2ab92f64bfb92d3656384d7fe0da6919457440d3c24301d7f6b8f232';
  assert.equal(canonicalHash(manifest.slice(0, 220)), protectedFinancialPrefixHash);
  assert.equal(financial.protectedMainManifestCanonicalSha256, protectedFinancialPrefixHash);
  assert.equal(financial.sourceMain, '5114fe95ac8e328b58162a8dcae5c9015a47ce09');
  assert.equal(manifest.length, 239);
  assert.deepEqual(manifest[227], {
    id: SESSION_EXPIRY_ENTRY,
    path: 'tdf-hq/sql/2026-10-31_api_token_session_expiry.sql',
    introducedBy: '8187065f81fad94677c8ba27760ab2cb74f58f79',
  });
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
  assert.equal(canonicalHash(manifest.slice(223, 227)),
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
  const latestCurrent = active.find(entry => entry.id === pointer.currentId);
  const finalCurrent = historicalReview(latestCurrent, 239);
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
  assert.equal(finalCurrent.evidence.valueCount, 239);
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
  const provider = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.providerDomainMainIntegration.document), 'utf8'));
  const aggregate = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.eventMemoryProviderMain20261026.document), 'utf8'));
  const memory226 = history.find(entry => entry.id === 'f48c34252ff8930b3d74');
  const provider223 = history.find(entry => entry.id === provider.currentDecisionId);
  assert.equal(finalCurrent.id, 'd7fc1df6cbade4a92d25');
  // Contract store (2026-11-04, CONTRACT-STORE-001): one migration appended after
  // the 238-entry review, which stays retired and hash-preserved.
  const reviewed238 = history.find(entry => entry.id === '050f309300a1688162fa');
  assert.equal(canonicalHash(reviewed238),
    '8c6aa26845f40678a9625f41a2d67b5dc367d290f4b0432639fd8472b85f4b8b');
  assert.equal(reviewed238.evidence.valueCount, 238);
  assert.equal(reviewed238.reviewed, true);
  assert.equal(finalCurrent.previousDecision.id, reviewed238.id);
  const contractStore = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3ContractStore20261104.document), 'utf8'));
  assert.equal(contractStore.currentDecisionId, finalCurrent.id);
  assert.deepEqual(contractStore.previousManifestDecision, reviewed238);
  assert.equal(contractStore.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 238)));
  assert.equal(contractStore.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(238)));
  assert.deepEqual(contractStore.appendedMigrationIds, ['2026-11-04_contract_store']);
  // Session invoice issuance (2026-11-04, PAY-INVOICE-ISSUE-001): one migration
  // appended after the 237-entry review, which stays retired and hash-preserved.
  const reviewed237 = history.find(entry => entry.id === 'ef3b8933af3854450a02');
  assert.equal(canonicalHash(reviewed237),
    'b3e101545dbce2e26e9603fbc81d735463b22f0cb486b4ae9342f724131fa145');
  assert.equal(reviewed237.evidence.valueCount, 237);
  assert.equal(reviewed237.reviewed, true);
  assert.equal(reviewed238.previousDecision.id, reviewed237.id);
  const invoiceIssue = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3SessionInvoiceIssue20261104.document), 'utf8'));
  assert.equal(invoiceIssue.currentDecisionId, reviewed238.id);
  assert.deepEqual(invoiceIssue.previousManifestDecision, reviewed237);
  assert.equal(invoiceIssue.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 237)));
  assert.equal(invoiceIssue.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(237, 238)));
  assert.deepEqual(invoiceIssue.appendedMigrationIds, ['2026-11-04_session_invoice_issue']);
  // Directory review states (2026-11-04, F-09): one migration appended after the
  // 236-entry review merged to main, which stays retired and hash-preserved.
  const merged236 = history.find(entry => entry.id === '438668da134480da0a9e');
  assert.equal(canonicalHash(merged236),
    'fae5221769f1e07714cd6160b57b8ae09146c3840d305f109d81baa176396032');
  assert.equal(merged236.evidence.valueCount, 236);
  assert.equal(merged236.reviewed, true);
  assert.equal(reviewed237.previousDecision.id, merged236.id);
  const reviewStates = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3DirectoryReviewStates20261104.document), 'utf8'));
  assert.equal(reviewStates.currentDecisionId, reviewed237.id);
  assert.deepEqual(reviewStates.previousManifestDecision, merged236);
  assert.equal(reviewStates.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 236)));
  assert.deepEqual(reviewStates.appendedMigrationIds, ['2026-11-04_directory_review_status_without_pending']);
  // Domo deposit validator (2026-11-04): one migration appended after the
  // reconciled 235-entry review, which stays retired and hash-preserved.
  const reconciled235 = history.find(entry => entry.id === '27def070486792cad395');
  assert.equal(canonicalHash(reconciled235),
    '085659be48eed02b09860767b22855e80f1fe17bddca460289a6ab13504fca03');
  assert.equal(reconciled235.evidence.valueCount, 235);
  assert.equal(reconciled235.reviewed, true);
  assert.equal(merged236.previousDecision.id, reconciled235.id);
  const depositValidator = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2DomoDepositValidator20261104.document), 'utf8'));
  assert.equal(depositValidator.currentDecisionId, merged236.id);
  assert.equal(depositValidator.previousDecisionId, reconciled235.id);
  assert.deepEqual(depositValidator.previousManifestDecision, reconciled235);
  assert.equal(depositValidator.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 235)));
  assert.equal(depositValidator.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(235, 236)));
  assert.deepEqual(depositValidator.appendedMigrationIds, ['2026-11-04_domo_quote_deposit_policy_validator']);
  // Session-expiry integration (2026-11-02): two concurrent successors of the
  // 227-entry review are reconciled; both stay retired and hash-preserved.
  const audit234 = history.find(entry => entry.id === 'd9468b767dadec9cf3d8');
  const sessionExpiry228 = history.find(entry => entry.id === '040c32c5b44ceadb2dde');
  assert.equal(canonicalHash(audit234),
    '61cbf1e27157b704dcad31fb8800df2d3498c2b57efb3dbc519f63f8b38e1c18');
  assert.equal(canonicalHash(sessionExpiry228),
    'e863458f044f4994d17134c7cd04ae00e8d48c95c2f59f23f076c6d27cfcf85e');
  assert.equal(audit234.evidence.valueCount, 234);
  assert.equal(sessionExpiry228.evidence.valueCount, 228);
  assert.equal(audit234.reviewed, true);
  assert.equal(sessionExpiry228.reviewed, true);
  assert.equal(reconciled235.previousDecision.id, audit234.id);
  assert.deepEqual(reconciled235.concurrentReviewHistory.map(entry => entry.id), [sessionExpiry228.id]);
  assert.equal(sessionExpiry228.previousDecision.id, 'a2bb0e828d5b94c1cb31');
  for (const parent of [audit234, sessionExpiry228]) {
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const sessionExpiry = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2SessionExpiryMainIntegration20261102.document), 'utf8'));
  assert.equal(sessionExpiry.currentDecisionId, reconciled235.id);
  assert.equal(sessionExpiry.previousDecisionId, audit234.id);
  assert.equal(sessionExpiry.concurrentDecisionId, sessionExpiry228.id);
  assert.deepEqual(sessionExpiry.mainManifestDecision, sessionExpiry228);
  assert.deepEqual(sessionExpiry.auditManifestDecision, audit234);
  assert.equal(sessionExpiry.interleavedIndex, 227);
  assert.equal(rawManifest.length, 239);
  // Protected main's 228-entry review is the exact prefix of the merged manifest.
  assert.equal(sessionExpiry.protectedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 228)));
  // The audit's 234-entry review is the merged manifest without main's entry.
  // Earlier reviews were taken over the 235-entry manifest before the deposit validator.
  const reconciledRaw = rawManifest.slice(0, 235);
  assert.equal(sessionExpiry.auditViewCanonicalSha256, canonicalHash(auditView(reconciledRaw)));
  assert.equal(auditView(reconciledRaw).length, 234);
  assert.equal(sessionExpiry.auditSuffixCanonicalSha256, canonicalHash(rawManifest.slice(228, 235)));
  const auditRaw = auditView(reconciledRaw);
  const auditReviewed = auditView(manifest.slice(0, 235));
  // Registry refresh (2026-11-02): one migration appended after the 233-entry
  // formal-audit review, which stays retired and preserved by canonical hash.
  const main233 = history.find(entry => entry.id === '903e61463c0222f30d9b');
  assert.equal(canonicalHash(main233),
    '9adad9f469eb233e58e2cbfac22651f797d7aba5e438d1f10ce880c436e0566c');
  assert.equal(main233.evidence.valueCount, 233);
  assert.equal(main233.reviewed, true);
  assert.equal(audit234.previousDecision.id, main233.id);
  const refresh = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2RegistryRefresh20261102.document), 'utf8'));
  assert.equal(refresh.currentDecisionId, audit234.id);
  assert.equal(refresh.previousDecisionId, main233.id);
  assert.equal(refresh.mainDecisionCanonicalSha256, canonicalHash(main233));
  assert.deepEqual(refresh.mainManifestDecision, main233);
  assert.equal(refresh.preservedPrefixCanonicalSha256, canonicalHash(auditRaw.slice(0, 233)));
  assert.equal(refresh.preservedPrefixCanonicalSha256BeforeCompatibleChecksum, canonicalHash(auditReviewed.slice(0, 233)));
  assert.equal(refresh.appendedSuffixCanonicalSha256, canonicalHash(auditRaw.slice(233)));
  assert.deepEqual(refresh.appendedMigrationIds, ['2026-11-02_identity_party_reference_refresh']);
  assert.equal(refresh.reviewedPrefixCanonicalSha256WithoutCompatibleChecksum['227'],
    '3fb0233724d5f6080b6d8933e46f0cd79c34b73fd3070e3e17b742ce1ff5113e');
  // The raw manifest differs from the reviewed one in exactly that one field.
  assert.equal(rawManifest[refresh.inPrefixChange.index].id, REGISTRY_REFRESH_ENTRY);
  assert.deepEqual(rawManifest[refresh.inPrefixChange.index].compatibleAppliedChecksums, refresh.inPrefixChange.added);
  for (const [index, entry] of rawManifest.entries()) {
    if (index === refresh.inPrefixChange.index) {
      const { compatibleAppliedChecksums: _added, ...rest } = entry;
      assert.deepEqual(rest, manifest[index]);
    } else assert.deepEqual(entry, manifest[index]);
  }
  // Formal audit phase 2 (2026-11-02): six audit migrations appended after the
  // exact 227-entry memory/provider review, which stays retired and preserved.
  const main227 = history.find(entry => entry.id === 'a2bb0e828d5b94c1cb31');
  assert.equal(canonicalHash(main227),
    '2a868127eeb27cf8ffcaf4d162dd6123bf0ae8ad9ce54901c0cabb631cf43e6e');
  assert.equal(main227.evidence.valueCount, 227);
  assert.equal(main227.reviewed, true);
  assert.equal(main233.previousDecision.id, main227.id);
  assert.equal(canonicalHash(manifest.slice(0, 227)),
    JSON.parse(await fs.readFile(path.join(workspaceRoot,
      document.reconciliation.formalAuditPhase2MainIntegration20261102.document), 'utf8')).preservedPrefixCanonicalSha256);
  assert.equal(canonicalHash(memory226),
    'd08e3ff9409b47ab035fc67269d79010fd3738333fdc971c305ded3ddfeadf9b');
  assert.equal(canonicalHash(provider223),
    '6bcc3d19c2aea1adb0ce6ede68d59ec60b025b953a083bc5be6090f4be90ed1a');
  assert.deepEqual(aggregate.memoryManifestDecision, memory226);
  assert.deepEqual(aggregate.mainManifestDecision, provider223);
  assert.equal(aggregate.memoryDecisionCanonicalSha256, canonicalHash(memory226));
  assert.equal(aggregate.mainDecisionCanonicalSha256, canonicalHash(provider223));
  assert.equal(canonicalHash(manifest.slice(0, 223)),
    '3123446d4ae490b41d13beb6ca62f17da39fbc90539791c9b5373709c05b1703');
  assert.equal(aggregate.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 223)));
  assert.equal(aggregate.memorySuffixCanonicalSha256, canonicalHash(manifest.slice(223, 227)));
  assert.deepEqual(manifest[222], {
    id: '2026-10-16_provider_domain_payment_binding',
    path: 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql',
    introducedBy: '456be073c944dd3d76eb9e3c2169fe7fbb4041ef',
  });
  assert.equal(provider.mainManifestDecision.id, header222.id);
  assert.equal(provider.mainManifestDecision.reviewed, true);
  assert.equal(provider.mainManifestDecision.evidence.valueCount, 222);
  assert.equal(canonicalHash(provider.mainManifestDecision),
    '866eef0dde46336e216336f13bdf9f83570314d2c17b9cf93f354de0f8ad9404');
  assert.equal(provider.mainDecisionCanonicalSha256, canonicalHash(provider.mainManifestDecision));
  assert.equal(canonicalHash(provider.providerManifestDecision),
    '018a1dc08aafe97b3af2257fe2ad24bac9a8f47f724975df0eec07e729ba8058');
  assert.equal(provider.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 222)));
  for (const parent of [memory226, provider223]) {
    assert.equal(parent.reviewed, true);
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const retiredParents = [...historical, current, memoryParent, memory225, header222,
    combined.headerManifestDecision, memory226, provider223, provider.providerManifestDecision, main227, main233,
    audit234, sessionExpiry228, reconciled235, merged236, reviewed237, reviewed238];
  assert.equal(current.classification, decision220.classification);
  assert.equal(current.disposition, decision220.disposition);
  assert.deepEqual(current.sourceReviewReferences.slice(0, decision220.sourceReviewReferences.length),
    decision220.sourceReviewReferences);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(current.evidence.valueCount, 221);
  assert.equal(pointer.currentId, latestCurrent.id);
  assert.equal(pointer.valueCount, 253);
  assert.equal(pointer.previousResolution.valueCount, 252);
  assert.equal(pointer.previousResolution.previousResolution.valueCount, 251);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 249);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 248);
  assert.equal(pointer.change, latestCurrent.justification);
  const retirement = integration.retirements.find(entry => entry.source.name === 'migrations');
  assert.equal(retirement.replacementId, latestCurrent.id);
  assert.equal(retirement.currentResolution.id, latestCurrent.id);
  assert.equal(retirement.currentResolution.valueCount, 253);
  assert.equal(retirement.previousResolution.currentResolution.valueCount, 252);
  assert.equal(retirement.previousResolution.previousResolution.currentResolution.valueCount, 251);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 250);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 249);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 248);
  assert.equal(retirement.reason, latestCurrent.justification);
  for (const decision of retiredParents) {
    assert.ok(pointer.previousCurrentIds.includes(decision.id));
    assert.ok(retirement.previousReplacementIds.includes(decision.id));
  }

  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-alert-successor-test-'));
  try {
    const repoDir = path.join(tempRoot, 'repo');
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, 'scripts/production-migrations.json', JSON.stringify({ migrations: rawManifest }));
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

test('dated-storage-parent: refund-parent: event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
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

test('dated-storage-parent: refund-parent: reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active, successors } = await readReconciliation();
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-reconciliation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');
  const decisionsPath = path.join(tempRoot, 'decisions.json');
  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    for (const file of new Set(ledger.retirements.map(entry => entry.file))) {
      await writeFile(repoDir, file, await mainSourceBytes(file));
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


test('dated-storage-parent: refund-parent: main integration archives exact old decisions and retains reviewed successors or source-removal evidence', async () => {
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

test('dated-storage-parent: refund-parent: catalog audit excludes ignored local source files from candidate fingerprints', async () => {
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

test('dated-storage-parent: refund-parent: decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
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
      // The removed review is kept as retirement provenance, so any history it
      // references stays reachable in the content-addressed store.
      sourceRemovalRetirements: [{
        reason: 'Catalog refresh unit-test-review: these candidates are no longer in the static list inventory. Their full reviewed decisions are preserved here as history; no catalog values are affected.',
        decisions: [{ ...preserved, id: 'stale-id' }],
      }],
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('dated-storage-parent: refund-parent: ticket main merge retains stale approvals only as provenance', async () => {
  const document = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const retired = document.reconciliation.ticketMainMergeRetirements;
  assert.equal(retired.sourceMain, 'b06a4d3926e03a0b72a23732e454a33c72eb9705');
  assert.equal(retired.decisions.length, 3);
  for (const entry of retired.decisions) {
    assert.equal(createHash('sha256').update(JSON.stringify(entry.originalDecision)).digest('hex'), entry.originalDecisionSha256);
    assert.ok(!document.decisions.some(item => item.id === entry.originalDecision.id));
    if (entry.replacementId === 'fd606f7d00f82c78cb20') {
      // The dated 164-feature replacement is now nested in the explicitly
      // reviewed canonical 165-feature mirror. It cannot authorize that new
      // source: the generator/negative-control test below checks this directly.
      const successor = document.decisions.find(item => item.id === 'ea29c6e8a6cd165802a8');
      assert.equal(successor?.reviewed, true);
      const parent = (await reviewedJson(INTEGRATION_MAIN, catalogPath)).decisions
        .find(item => item.id === entry.replacementId);
      assert.deepEqual(successor.previousDecision, parent);
      assert.ok(!document.decisions.some(item => item.id === entry.replacementId));
    } else if (entry.replacementId) {
      assert.equal(document.decisions.find(item => item.id === entry.replacementId)?.reviewed, true);
    } else {
      assert.equal(entry.originalDecision.id, 'ec1a316784aa9992384a');
      assert.doesNotMatch(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'tdf-mobile/app/ddex/index.tsx', 'utf8'), /const STATUS_LABELS/);
    }
  }
});


test('dated-storage-parent: refund-parent: storage/main integration preserves both exact parent reviews and manifest views', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const manifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references');
  const { active, successors } = await readReconciliation();
  const extension = active.find(entry => entry.id === successors.find(item => item.name === 'migrations').currentId);
  const current = historicalReview(extension, 244);
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item) ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(manifest.length, 244);
  assert.equal(canonicalHash(manifest.slice(0, 239)), ledger.protectedMainPrefixCanonicalSha256);
  assert.equal(canonicalHash(manifest.filter(entry => !ledger.mainOnlyMigrationIds.includes(entry.id))), ledger.storageViewCanonicalSha256);
  assert.equal(current.evidence.valueCount, 244);
  assert.equal(successors.find(entry => entry.name === 'migrations').currentId, extension.id);
  for (const parent of ledger.parentReviews) {
    const entry = [current.previousDecision, ...current.concurrentReviewHistory].find(item => item.id === parent.id);
    assert.equal(canonicalHash(entry), parent.canonicalSha256);
    assert.ok(!active.some(item => item.id === parent.id));
  }
  assert.equal(ledger.executionAllowed, false);
});

test('dated-storage-parent: refund-parent: video campaign migration appends to the exact protected 244-entry manifest', async () => {
  const main = 'e5248e0b3ad30d832ef1ccbf5e595a7ee36b0324';
  const introduction = '4b84238abb4ca17c6aaa9902e3bec16a1cc3daa5';
  const old = JSON.parse((await execFileAsync('git', ['show', `${main}:scripts/production-migrations.json`], { cwd: workspaceRoot })).stdout).migrations;
  const current = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const priorDecisions = JSON.parse((await execFileAsync('git', ['show', `${main}:${decisionsPath}`], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout);
  const { active } = await readReconciliation(true);
  const latest = active.find(item => item.evidence?.file === manifestPath);
  const videoReview = historicalReview(latest, 245);
  assert.equal(videoReview.id, '969410f2f45854cc54fc');
  assert.deepEqual(videoReview.previousDecision,
    priorDecisions.decisions.find(item => item.id === 'be0b1503a04f9fa295ee'));
  assert.equal(old.length, 244);
  assert.equal(current.length, 246);
  assert.deepEqual(current.slice(0, 244), old);
  assert.deepEqual(current[244], { id: '2026-10-20_video_campaigns', path: 'tdf-hq/sql/2026-10-20_video_campaigns.sql', introducedBy: introduction });
  const originalSql = (await execFileAsync('git', ['show', `${introduction}:${current[244].path}`], { cwd: workspaceRoot })).stdout;
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[244].path), 'utf8'), originalSql);
  await execFileAsync('git', ['merge-base', '--is-ancestor', introduction, 'HEAD'], { cwd: workspaceRoot });
  const forward = '09fd7babb92e45c1da1b9e5017de53b6378b4f7c';
  assert.deepEqual(current[245], { id: '2026-11-06_video_campaign_identity_references', path: 'tdf-hq/sql/2026-11-06_video_campaign_identity_references.sql', introducedBy: forward });
  const priorManifest = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.deepEqual(current.slice(0, 245), priorManifest);
  const priorReview = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === videoReview.id);
  assert.deepEqual(videoReview, priorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[245].path), 'utf8'), (await execFileAsync('git', ['show', forward + ':' + current[245].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', forward, 'HEAD'], { cwd: workspaceRoot });
  // ADR 0134: marketplace offers were reviewed on top of the exact 246-entry
  // identity manifest (8ee6c7a) and now append after the two Apple entries that
  // protected main integrated; the manifest without those entries is still that
  // reviewed 247-entry manifest byte for byte.
  const marketplace = '8ee6c7a66e2504f425863ca6f5b83062723efd58';
  const marketplaceIntroduction = '5901003189dac2291380604c66fdd66087e4a587';
  const full = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(full.length, 249);
  // The agreements entry (ADR 0135) is the 251st, the last one before the
  // event administrator entry: its SQL is exactly what its introduction commit
  // carries and that commit is an ancestor.
  const agreementsEntry = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY).at(-1);
  assert.equal(agreementsEntry.id, MARKETPLACE_AGREEMENTS_ENTRY);
  assert.equal(agreementsEntry.path, 'tdf-hq/sql/2026-11-12_marketplace_agreements.sql');
  assert.equal(await fs.readFile(path.join(workspaceRoot, agreementsEntry.path), 'utf8'), (await execFileAsync('git', ['show', agreementsEntry.introducedBy + ':' + agreementsEntry.path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', agreementsEntry.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  assert.deepEqual(full[248], { id: MARKETPLACE_OFFERS_ENTRY, path: 'tdf-hq/sql/2026-11-08_marketplace_offers.sql', introducedBy: marketplaceIntroduction });
  const identityManifest = JSON.parse((await execFileAsync('git', ['show', marketplace + ':scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.equal(identityManifest.length, 247);
  // The marketplace entry's introduction moved to the review-fix commit (the
  // SQL gained the analytics allowlist and the filled -> published edge);
  // everything before it is still the reviewed identity manifest byte for byte.
  const withoutApple = full.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY);
  assert.deepEqual(withoutApple.slice(0, -1), identityManifest.slice(0, -1));
  assert.equal(withoutApple.at(-1).id, identityManifest.at(-1).id);
  const identityReview = historicalReview(latest, 246);
  assert.equal(identityReview.id, '806180f5957fc4a7520f');
  const identityPriorReview = JSON.parse((await execFileAsync('git', ['show', marketplace + ':' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === identityReview.id);
  assert.deepEqual(identityReview, identityPriorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, full[248].path), 'utf8'), (await execFileAsync('git', ['show', marketplaceIntroduction + ':' + full[248].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', marketplaceIntroduction, 'HEAD'], { cwd: workspaceRoot });
});

test('dated-storage-parent: refund-parent: Apple policy preserves the exact main prefix and every manual review', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const fromGit = async file => (await execFileAsync('git', ['show', `${main}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout;
  const oldManifest = JSON.parse(await fromGit('scripts/production-migrations.json')).migrations;
  // The marketplace offers entry (ADR 0134) appends after the Apple entries;
  // the Apple review is verified over the manifest without it.
  const currentManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(oldManifest.length, 246);
  assert.equal(currentManifest.length, 248);
  assert.deepEqual(currentManifest.slice(0, 246), oldManifest);
  const issuer = currentManifest[247];
  assert.deepEqual(issuer, { id: APPLE_ISSUER_ENTRY, path: 'tdf-hq/sql/2026-11-07_apple_provider_identity_issuer.sql', introducedBy: '7c1525edf0f02fe53f985c24862940b91c11cdfa' });
  assert.equal(await fs.readFile(path.join(workspaceRoot, issuer.path), 'utf8'), (await execFileAsync('git', ['show', `${issuer.introducedBy}:${issuer.path}`], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', issuer.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  const addition = currentManifest[246];
  assert.equal(addition.id, APPLE_POLICY_ENTRY);
  assert.equal(addition.path, 'tdf-hq/sql/2026-10-13_apple_sign_in_policy.sql');
  const original = JSON.parse((await execFileAsync('git', ['show',
    'e7ec452787e15f09deda4c4328ec6a8c1cece727:scripts/production-migrations.json'],
  { cwd: workspaceRoot })).stdout).migrations.find(row => row.id === APPLE_POLICY_ENTRY);
  assert.deepEqual(addition, original);
  assert.equal(await fs.readFile(path.join(workspaceRoot, addition.path), 'utf8'),
    (await execFileAsync('git', ['show', `${addition.introducedBy}:${addition.path}`],
      { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', addition.introducedBy, 'HEAD'],
    { cwd: workspaceRoot });
  const old = JSON.parse(await fromGit('docs/catalog-persistence/catalog-list-decisions.json')).decisions;
  // Compare the Apple review with the records as protected main 8ec7d85c held
  // them: the event administrator successors are read back to the review,
  // pointer and retirement they replaced (each nests its predecessor
  // unchanged), and lists first reviewed after that main are left out.
  const { active: currentDecisions, retired, successors: currentSuccessors,
    integration: currentIntegration } = await readReconciliation(true);
  const mainIds = new Set((await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/catalog-list-decisions.json')).decisions.map(row => row.id));
  // Later successors (the agreement payments manifest review, ADR 0136) nest
  // the event administrator one, so step back until the record that main held.
  const active = currentDecisions.flatMap(row => {
    let reviewed = row;
    while (reviewed && !mainIds.has(reviewed.id)) reviewed = reviewed.previousDecision;
    return reviewed ? [reviewed] : [];
  });
  const heldByMain = (row, idOf) => {
    let held = row;
    while (held && !mainIds.has(idOf(held))) held = held.previousResolution;
    return held;
  };
  const successors = currentSuccessors.map(row => row.name === 'migrations' ? heldByMain(row, held => held.currentId) : row);
  const integration = { ...currentIntegration, retirements: currentIntegration.retirements
    .map(row => row.source.name === 'migrations' ? heldByMain(row, held => held.replacementId) : row) };
  const reviewedParent = async revision => JSON.parse((await execFileAsync('git',
    ['show', `${revision}:docs/catalog-persistence/catalog-list-decisions.json`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  const priorApple = await reviewedParent('bc16d557c7b246361c83d78191314087ca2dd3d9');
  const currentMain = await reviewedParent('321f4fe6c8e803ab1c32724a8ce5b7814418b79c');
  const offlineMain = await reviewedParent('5c69dc30318d110d9daba042571b8e54f5bf140d');
  const offlineFixture = offlineMain.find(row => row.id === 'b3eadb9d52a262f5371f');
  assert.ok(offlineFixture, 'the offline currency fixture needs its exact protected-main review');
  assert.deepEqual(active.find(row => row.id === offlineFixture.id), offlineFixture);
  // Protected main later reviewed two route-specification lists (#710); keep
  // their exact reviews and leave them out of the historical Apple view too.
  const routeMain = await reviewedParent('b1614afd18b02e8cd0d46f71d798b7f38764bcec');
  const routeReviews = ['afe675892f43a8a4de73', '6f876292d2fcb1dd7a86']
    .map(id => routeMain.find(row => row.id === id));
  for (const review of routeReviews) {
    assert.ok(review, 'the route-specification reviews need their exact protected-main review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The desktop shell (#717) adds nine reviewed playback and desktop lists;
  // keep their exact branch reviews outside the historical Apple view as well.
  const desktopBranch = await reviewedParent('3668640fa56a558bbd3bd95881526d5faf5be22c');
  const desktopReviews = ['9f5a58afbcc5a21f3a81', '162bb73e8a27ecdbd80d', '19b53762aa84c847059a',
    'd3fe8dedc63dbee01dcf', 'dd837be242ffaf6c73c5', '70eef2bc5a9e8ef6ab65', 'ee97985b0d9abc1b1ed3',
    '57fc580c92ca2352698d', '2cf4da8b8443b9be1b48'].map(id => desktopBranch.find(row => row.id === id));
  for (const review of desktopReviews) {
    assert.ok(review, 'the desktop shell reviews need their exact branch review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The marketplace offers branch (ADR 0134, #719) adds eighteen value lists
  // reviewed by the deterministic policy, extends the search page view-mode
  // attribute list (successor of main's exact review 7393e5dd10ebe68ae3e2) and
  // records a successor review of the 249-entry manifest whose exact
  // predecessor is main's 248-entry review; the historical Apple view keeps
  // the exact predecessors and leaves the new lists out.
  const marketplaceReviewIds = ['3d30e49db2fefa10a623', '2471af5d3bf550aade51', '0365580729887c8f5ca7',
    '0dc607f91bd630b1fb87', '4ed56a39eea6e4d4679d', 'aa6a65391d2c0611fdd8',
    'b79670b0b1a1fbc1ee05', 'df679c5b2cb5d91be79e', 'ef5bffc43b9c0ff3a361', '39edb2a76ecef8cd6c57',
    '2a350b9da29a2d080bd8', 'd776481beacf4f4a074f', '16393adee900667601af', '7207f087ef5f8e0b2158',
    '3a7f374bb3b52ef8d34d', 'a9c6757fbe6f2868f4f2', '853a5f768e09d0c32259',
    // Listing-conditions labels on the public classified page (review fix round four).
    '9dd6b72c9435bd64fe09'];
  for (const id of marketplaceReviewIds) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace offers review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  // The auth_rate_limit migration (owner decisions of 2026-11-10) succeeds the
  // marketplace manifest review as the 250-entry tip and adds two reviews; the
  // historical view sets those aside and steps the manifest back twice.
  // The marketplace agreements migration (ADR 0135) succeeds the auth_rate_limit
  // review as the 251-entry tip and adds six reviews (status filter, panel
  // tabs, agreement CHECK lists and the party type); the historical view sets
  // those aside too and steps the manifest back once more.
  const agreementsTip = active.find(row => row.id === 'bc5e5bd4f9d8795d2166');
  assert.equal(agreementsTip.evidence.valueCount, 251);
  const agreementReviews = ['3a5425de732f609db717', '75f5a77be26697f0a281', '06f1e7eef25d710385f4',
    '74460721a4c7f744e3ab', '33d907b3850fb5474551', 'c9a145a03e59580950b5'];
  for (const id of agreementReviews) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace agreements review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  const authRateLimitTip = agreementsTip.previousDecision;
  assert.equal(authRateLimitTip.id, '52d96d4b31e677ac5948');
  assert.equal(authRateLimitTip.evidence.valueCount, 250);
  const authRateLimitReviews = ['f37745d92e494e02fcba', 'a95cbb8b9256407889ea'];
  const marketplaceManifestReview = authRateLimitTip.previousDecision;
  assert.equal(marketplaceManifestReview.id, '4dc5a2f26154a0f125db');
  assert.equal(marketplaceManifestReview.evidence.valueCount, 249);
  assert.equal(marketplaceManifestReview.previousDecision.id, 'a20e04bc426cc6383584');
  const searchViewReview = active.find(row => row.id === '79bdbe2ae9b350e0d1da');
  assert.equal(searchViewReview.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(searchViewReview.previousDecision.id, '7393e5dd10ebe68ae3e2');
  const marketplaceSuccessors = new Map([
    [agreementsTip.id, marketplaceManifestReview.previousDecision],
    [searchViewReview.id, searchViewReview.previousDecision]]);
  const laterMainIds = new Set([offlineFixture.id, ...routeReviews.map(row => row.id),
    ...desktopReviews.map(row => row.id), ...marketplaceReviewIds, ...authRateLimitReviews, ...agreementReviews]);
  const historicalAppleView = active.filter(row => !laterMainIds.has(row.id))
    .map(row => marketplaceSuccessors.get(row.id) ?? row);
  assert.equal(old.length, 1405);
  assert.equal(priorApple.length, 1407);
  assert.equal(currentMain.length, 1409);
  const retiredIds = new Set(retired.map(row => row.id));
  assert.equal(historicalAppleView.length, 1411 - retired.length);
  const appleView = (await reviewedParent('15e2228d24080a520d4bed187731cc3db09d474a'))
    .filter(row => !retiredIds.has(row.id));
  assert.equal(historicalAppleView.length, appleView.length);
  for (const [index, row] of appleView.entries()) {
    assert.deepEqual(historicalAppleView[index], row, `Apple review order or content changed at ${row.id}`);
  }
  assert.equal(active.length,
    1423 + marketplaceReviewIds.length + authRateLimitReviews.length + agreementReviews.length - retired.length);
  const history = new Map(retired.map(row => [row.id, row]));
  const visit = row => {
    if (!row) return;
    if (!history.has(row.id)) history.set(row.id, row);
    visit(row.previousDecision);
    for (const branch of row.concurrentReviewHistory ?? []) visit(branch);
  };
  for (const row of active) visit(row);
  for (const row of [...old, ...priorApple, ...currentMain, ...offlineMain, ...routeMain]) {
    assert.deepEqual(history.get(row.id), row);
  }
  const pointer = successors.find(row => row.name === 'migrations');
  const agreementsCurrentTip = active.find(row => row.id === pointer.currentId);
  assert.equal(agreementsCurrentTip.evidence.valueCount, 251);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').replacementId,
    agreementsCurrentTip.id);
  assert.equal(pointer.previousResolution.currentId, agreementsCurrentTip.previousDecision.id);
  const currentTip = agreementsCurrentTip.previousDecision;
  assert.equal(currentTip.evidence.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.currentId, currentTip.previousDecision.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousReplacementId,
    currentTip.id);
  const marketplaceTip = currentTip.previousDecision;
  assert.equal(marketplaceTip.evidence.valueCount, 249);
  const tip = marketplaceTip.previousDecision;
  assert.equal(tip.evidence.valueCount, 248);
  assert.equal(historicalReview(tip, 247).previousDecision.id, '806180f5957fc4a7520f');
  assert.equal(tip.previousDecision.id, '4e282e4c258854caa655');
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.currentId, tip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousReplacementId,
    marketplaceTip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousResolution.previousReplacementId,
    tip.id);
  const security = active.find(row => row.evidence?.file === 'tdf-hq/src/TDF/Catalog/Security.hs'
    && row.evidence?.name === 'expectedSecurityRoleAssignmentPolicyBindings');
  assert.equal(security.previousDecision.id, 'ff4edc0f8d2debecf71c');
  const currentSecurity = await fs.readFile(path.join(workspaceRoot, security.evidence.file), 'utf8');
  const withoutApple = currentSecurity.replace(
    '  , ("account.apple.customer", "apple-account-create", "customer", True)\n', '');
  assert.notEqual(withoutApple, currentSecurity);
  assert.equal(withoutApple, await fromGit(security.evidence.file));
});

test('dated-storage-parent: refund-parent: event administrator delegation appends one migration to the protected 251-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: EVENT_ADMIN_ENTRY, path: 'tdf-hq/sql/2026-11-11_event_admin_delegation.sql',
    introducedBy: EVENT_ADMIN_INTRODUCTION };
  assert.equal(main.migrations.length, 251);
  // The agreement payments entry (ADR 0136) appends after this one and has its own review.
  assert.deepEqual({ ...combined, migrations: combined.migrations.filter(entry => entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY) },
    { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${EVENT_ADMIN_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', EVENT_ADMIN_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, decisionsPath)).decisions;
  // The active manifest review is the agreement payments successor; this
  // review, its pointer and its retirement are nested one step back, unchanged.
  const tipPointer = successors.find(row => row.name === 'migrations');
  const pointer = tipPointer.previousResolution;
  const current = active.find(row => row.id === tipPointer.currentId).previousDecision;
  assert.equal(current.id, pointer.currentId);
  assert.equal(current.evidence.valueCount, 252);
  assert.equal(current.reviewed, true);
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 251);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations').previousResolution;
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // The typed client registry is the only other successor; it also nests its parent review.
  // EVT-TICKET-SALE-001 later added four methods: its 90-method successor keeps
  // this 86-method review intact as its previousDecision.
  const clientNow = active.find(row => row.id === successors.find(row => row.name === 'SocialEventsAPI').currentId);
  assert.equal(clientNow.evidence.valueCount, 90);
  const client = clientNow.previousDecision;
  assert.equal(client.id, '083bd1a66e9f23f02866');
  assert.equal(client.evidence.valueCount, 86);
  assert.deepEqual(client.previousDecision, priorMain.find(row => row.id === '2511cbba2fc7279bc28a'));
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const movedReviews = assertMovedListsNest(active, priorMain);
  // Every other review of protected main is retained exactly; three lists are new.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, client.previousDecision.id, ...movedReviews]);
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else assert.deepEqual(history.get(row.id), row);
  }
  assert.equal(priorMain.length, 1449);
  // 1452 after this review, plus the nine agreement payments reviews (ADR 0136),
  // less the reviews retired with their deleted source lists; the delivery
  // protocol review (SYS-VERIFY-002) is counted separately.
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});

test('dated-storage-parent: refund-parent: marketplace agreement payments append one migration to the protected 252-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY,
    path: 'tdf-hq/sql/2026-11-12_marketplace_agreement_payments.sql',
    introducedBy: AGREEMENT_PAYMENTS_INTRODUCTION };
  assert.equal(main.migrations.length, 252);
  assert.deepEqual(combined, { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${AGREEMENT_PAYMENTS_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', AGREEMENT_PAYMENTS_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, decisionsPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const current = active.find(row => row.id === pointer.currentId);
  assert.equal(current.evidence.valueCount, 253);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 252);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.equal(pointer.change, current.justification);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, current.id);
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // Every other review of protected main is retained exactly; nine lists are
  // new (payment provider, fee payer, payout and payment CHECK lists and the
  // two Policy sum types) and reviewed by the deterministic policy.
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, ...assertMovedListsNest(active, priorMain)]);
  // The 86-method client review was since superseded by EVT-TICKET-SALE-001
  // and is retained exactly as the previousDecision of its successor.
  const clientSuccessor = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else if (row.id === '083bd1a66e9f23f02866') assert.deepEqual(clientSuccessor.previousDecision, row);
    else assert.deepEqual(history.get(row.id), row);
  }
  const payoutReviews = ['d580ad14a8f01218c4a9', 'fbe5e4e4d0289e1caaf4', 'cfa5a05214e95ed07ef0',
    '203395cfa370d78f86a1', '1f6946049f264576684d'];
  const chargeReviews = ['09d7dc6ebc55642a4bb2', '584b9609b82ad71578ed', 'e1a25b160e801cf866df',
    '32574b309f1d005ed165'];
  for (const id of [...payoutReviews, ...chargeReviews]) {
    assert.ok(!priorMain.some(row => row.id === id));
    assert.ok(history.get(id)?.reviewed, `the marketplace agreement payments review ${id} must be present and reviewed`);
    assert.match(history.get(id).reviewMethod, /^deterministic-policy-v\d+$/);
  }
  assert.equal(priorMain.length, 1452);
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});


test('dated-storage-parent: refund-parent: player server preserves both reviewed migration histories and appends only reviewed SQL', async () => {
  const main = await reviewedJson(PLAYER_SERVER_MAIN_BASE, manifestPath);
  const player = await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, manifestPath);
  const current = (await mainManifestView());
  const extension = player.migrations.filter(row => playerMigrations.has(row.id));
  assert.deepEqual(extension.map(row => row.id), [...playerMigrations]);
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...extension] });
  for (const row of extension) {
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'],
      { cwd: workspaceRoot });
    const { stdout } = await execFileAsync('git', ['show', `${row.introducedBy}:${row.path}`],
      { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
  }

  const { active, successors, integration } = await readReconciliation();
  const pointer = successors.find(row => row.name === 'migrations');
  const actual = active.find(row => row.id === pointer.currentId);
  const findTip = doc => doc.decisions.find(row => row.evidence?.file === manifestPath);
  const mainReview = findTip(await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath));
  const playerReview = findTip(await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, catalogPath));
  assert.deepEqual(actual.previousDecision, mainReview);
  assert.deepEqual(actual.concurrentReviewHistory, [playerReview]);
  assert.equal(actual.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, actual.id);
  assert.equal(retirement.previousResolution.replacementId, mainReview.id);
});

test('dated-storage-parent: refund-parent: loop rework retires the reviews of two deleted scripts as provenance and reviews one new list', async () => {
  const { active } = await readReconciliation();
  const rework = await readLoopRework();
  assert.equal(rework.decisions.length, 2);
  assert.ok(rework.reason.length > 80);
  for (const entry of rework.decisions) {
    assert.equal(entry.decision.reviewed, true);
    assert.ok(!active.some(row => row.id === entry.decision.id));
    await assert.rejects(fs.access(path.join(workspaceRoot, entry.source.file)), { code: 'ENOENT' });
  }
  assert.deepEqual(rework.addedDecisionIds, ['16e3988d48089cc77b7e']);
  for (const id of rework.addedDecisionIds) {
    assert.equal(active.find(row => row.id === id)?.reviewed, true);
  }
  // The earlier view re-inserts the retired reviews and drops the new one; the catalog
  // itself keeps growing with main, so only the difference is fixed.
  const historical = (await readHistoricalReconciliation()).active;
  assert.equal(historical.length, active.length + rework.decisions.length - rework.addedDecisionIds.length);
  for (const entry of rework.decisions) assert.deepEqual(historical[entry.position], entry.decision);
});

// One explicit review classifies the delivery protocol; it is new, unique and
// carries its requirement. The rest of the catalog follows main.
test('dated-storage-parent: refund-parent: delivery evidence protocol adds exactly one reviewed list', async () => {
  const current = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', catalogPath, 'utf8'));
  const baseline = await reviewedJson('5084d8a8a4d7a7a3b8d30d1f55ae5d9c8c032b9b', catalogPath);
  assert.ok(!baseline.decisions.some(row => row.id === DELIVERY_PROTOCOL_REVIEW));
  const additions = current.decisions.filter(row => row.id === DELIVERY_PROTOCOL_REVIEW);
  assert.equal(additions.length, 1);
  assert.equal(additions[0].classification, 'genuine-technical-constant');
  assert.equal(additions[0].disposition, 'explicit-allowlist');
  assert.equal(additions[0].reviewed, true);
  assert.equal(additions[0].evidence.file, 'scripts/lib/delivery-matrix.mjs');
  assert.equal(additions[0].evidence.name, 'DELIVERY_CHECKS');
  assert.match(additions[0].justification, /SYS-VERIFY-002/);
  assert.equal(current.decisions.filter(row => row.evidence?.file === 'scripts/lib/delivery-matrix.mjs').length, 1);
});

// These parent tests describe a dated schema. Only the two/one explicitly
// appended migration rows are projected away; combined-source checks below
// validate the actual unprojected registry and complete parent histories.
const INTEGRATION_MAIN = '966fb55dca70a49c0a340b674f8cc133cc413a2d';
const INTEGRATION_SOURCE = 'c702734dd932b8445e2429462d6638e03261435a';
const INTEGRATION_EXTRA_IDS = new Set(["2026-11-02_domo_refund_lifecycle"]);
const INTEGRATION_MAIN_REVIEW = '912cda11c7b6eb622f76';
async function actualManifest() {
  return JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'scripts/production-migrations.json', 'utf8'));
}
async function mainManifestView() {
  const actual = await actualManifest();
  return { ...actual, migrations: actual.migrations.filter(row => !INTEGRATION_EXTRA_IDS.has(row.id)) };
}
async function sourceManifestView() {
  const original = (await reviewedJson(INTEGRATION_SOURCE, 'scripts/production-migrations.json')).migrations;
  const current = (await actualManifest()).migrations;
  return { migrations: original.map(row => {
    const actual = current.find(item => item.id === row.id);
    assert.deepEqual(actual, row, `Source manifest entry changed: ${row.id}`);
    return actual;
  }) };
}
const reviewIndexes = new WeakMap();
function wholeReview(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  let index = reviewIndexes.get(value);
  if (!index) {
    index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      if (node.id) index.set(node.id, [...(index.get(node.id) ?? []), node]);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); reviewIndexes.set(value, index);
  }
  return (index.get(expected.id) ?? []).find(row => canonicalReview(row) === canonicalReview(expected));
}

async function sourceReconciliation() {
  const register = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', catalogPath, 'utf8'));
  const original = await reviewedJson(INTEGRATION_SOURCE, catalogPath);
  const active = original.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Complete original source review missing: ${row.id}`);
    return found;
  });
  return { active, successors: (await reviewedJson(INTEGRATION_SOURCE,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors };
}

const DOMO_REFUND_ENTRY = '2026-11-02_domo_refund_lifecycle';

test('dated-storage-parent: refund-parent: Domo refund247 preserves exact Main246, original SQL/rollback and both review graphs', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const source = 'c36edd9f2fd99112ec56622d89ea37e8b72b2879';
  const readAt = async (ref, file) => (await execFileAsync('git', ['show', `${ref}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 32 * 1024 * 1024 })).stdout;
  const file = 'scripts/production-migrations.json';
  const prior = JSON.parse(await readAt(main, file)).migrations;
  const original = JSON.parse(await readAt(source, file)).migrations;
  const current = (await sourceManifestView()).migrations;
  assert.equal(prior.length, 246); assert.equal(original.length, 236); assert.equal(current.length, 247);
  assert.deepEqual(current.slice(0, 246), prior);
  assert.deepEqual(current[246], original[235]);
  assert.equal(current[246].id, DOMO_REFUND_ENTRY);
  assert.deepEqual(prior.slice(0, 235), original.slice(0, 235));
  await execFileAsync('git', ['merge-base', '--is-ancestor', current[246].introducedBy, 'HEAD'], { cwd: workspaceRoot });
  for (const migration of current) {
    const ref = migration.id === DOMO_REFUND_ENTRY ? source : main;
    assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'),
      await readAt(ref, migration.path), `Immutable reviewed SQL changed: ${migration.id}`);
  }
  const rollback = 'tdf-hq/sql/2026-11-02_domo_refund_lifecycle_rollback.sql';
  assert.equal(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', rollback, 'utf8'), await readAt(source, rollback));
  const decisions = 'docs/catalog-persistence/catalog-list-decisions.json';
  const sourceReviews = JSON.parse(await readAt(source, decisions));
  const mainReviews = JSON.parse(await readAt(main, decisions));
  const { active, successors } = await sourceReconciliation();
  const tip = active.find(row => row.id === successors.find(row => row.name === 'migrations').currentId);
  assert.equal(tip.evidence.valueCount, 247);
  assert.deepEqual(tip.previousDecision, mainReviews.decisions.find(row => row.evidence?.file === file));
  assert.deepEqual(tip.concurrentReviewHistory, [sourceReviews.decisions.find(row => row.evidence?.file === file)]);
  for (const priorReview of mainReviews.decisions) {
    if (priorReview.evidence?.file === file) continue;
    let retained = active.find(row => row.id === priorReview.id);
    if (priorReview.id === '34210b76e81fd04cffae') {
      // This assertion refers to dated Main f615's 75-method transport, not
      // current Main's reviewed 78-method successor. Preserve the WHOLE old
      // review in that actual successor chain; it must not be active again.
      retained = active.find(row => row.id === '2511cbba2fc7279bc28a');
      while (retained && retained.id !== priorReview.id) retained = retained.previousDecision;
    }
    assert.deepEqual(retained, priorReview);
  }
  assert.deepEqual(JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/domo-source-review-history-20261107.json', 'utf8')), sourceReviews);
});

test('dated-storage-parent: refund-parent: dated source transport review retains its 75-method predecessor and historical negative control', async () => {
  const main = '321f4fe6c8e803ab1c32724a8ce5b7814418b79c';
  const file = 'tdf-hq-ui/src/api/socialEvents.ts';
  const decisionsFile = 'docs/catalog-persistence/catalog-list-decisions.json';
  const readAt = async name => (await execFileAsync('git', ['show', `${main}:${name}`],
    { cwd: workspaceRoot, maxBuffer: 32 * 1024 * 1024 })).stdout;
  const mainReviews = JSON.parse(await readAt(decisionsFile));
  const currentReviews = { decisions: (await sourceReconciliation()).active };
  const current = currentReviews.decisions.find(row => row.id === '2511cbba2fc7279bc28a');
  assert.deepEqual(current, mainReviews.decisions.find(row => row.id === current.id));
  assert.equal(current.evidence.valueCount, 78);
  const implementation = await readAt(file); // dated fixture for the retained negative control
  assert.equal(implementation, await readAt(file), 'Current transport must preserve complete canonical Main source');
  let predecessor = current;
  while (predecessor && predecessor.id !== '34210b76e81fd04cffae') predecessor = predecessor.previousDecision;
  assert.ok(predecessor, 'Keep the exact dated review in the actual successor chain');
  assert.equal(predecessor.evidence.valueCount, 75);
  assert.ok(!currentReviews.decisions.some(row => row.id === predecessor.id), 'Historical approval must stay inactive');

  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-current-transport-'));
  const repo = path.join(temporary, 'repo');
  const decisions = path.join(temporary, 'decisions.json');
  const reportPath = path.join(temporary, 'report.json');
  try {
    await fs.mkdir(repo);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repo });
    await writeFile(repo, file, implementation);
    await execFileAsync('git', ['add', '--', file], { cwd: repo });
    const mainInventory = JSON.parse(await readAt('docs/catalog-persistence/reports/static-list-inventory.json'));
    const ids = new Set(mainInventory.candidates.filter(row => row.file === file).map(row => row.id));
    const selected = mainReviews.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size, 'Keep every canonical review for this complete source file');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [auditScriptPath, '--root', repo, '--decisions', decisions, '--output', reportPath];
    await execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repo });
    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    const candidate = report.candidates.find(row => row.id === current.id);
    assert.ok(candidate, 'Actual source fingerprint must identify the active review');
    assert.equal(candidate.file, file);
    assert.equal(candidate.values.length, 78);
    assert.equal(candidate.decision, current.classification);

    // An intact historical review is still insufficient for the changed source.
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row.id === current.id ? predecessor : row)));
    await assert.rejects(execFileAsync(process.execPath, [...args, '--fail-on-unreviewed'], { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
    const rejected = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    assert.equal(rejected.candidates.find(row => row.id === current.id)?.decision, 'unreviewed');
    assert.ok(!rejected.candidates.some(row => row.id === predecessor.id));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});


test('dated-storage-parent: refund-parent: combined registry retains the exact main prefix, original SQL and whole parent reviews', async () => {
  const current = await actualManifest();
  const main = await reviewedJson(INTEGRATION_MAIN, manifestPath);
  const original = await reviewedJson(INTEGRATION_SOURCE, manifestPath);
  const suffix = original.migrations.filter(row => INTEGRATION_EXTRA_IDS.has(row.id));
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...suffix] });
  assert.equal(new Set(current.migrations.map(row => row.id)).size, current.migrations.length);
  for (const row of suffix) {
    const { stdout } = await execFileAsync('git', ['show', `${INTEGRATION_SOURCE}:${row.path}`], { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  }
  const register = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', catalogPath, 'utf8'));
  const pointerFile = 'docs/catalog-persistence/event-catalog-current-successors.json';
  const retireFile = 'docs/catalog-persistence/event-integration-retirements.json';
  const pointers = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', pointerFile, 'utf8'));
  const retirements = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', retireFile, 'utf8'));
  const pointer = pointers.successors.find(row => row.name === 'migrations');
  const tip = register.decisions.find(row => row.id === pointer.currentId);
  const retirement = retirements.retirements.find(row => row.source.name === 'migrations');
  assert.equal(tip.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  assert.equal(retirement.replacementId, tip.id);
  assert.deepEqual(tip.previousDecision, (await reviewedJson(INTEGRATION_MAIN, catalogPath))
    .decisions.find(row => row.evidence?.file === manifestPath));
  const canonical = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
    ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
  for (const parent of [INTEGRATION_MAIN, INTEGRATION_SOURCE]) {
    const old = await reviewedJson(parent, catalogPath);
    for (const row of old.decisions) {
      const found = wholeReview(register, row);
      assert.ok(found, `Lost complete parent review ${parent}:${row.id}`);
      assert.equal(canonical(found), canonical(row));
    }
    for (const [file, actual, key] of [[pointerFile, pointers, 'successors'], [retireFile, retirements, 'retirements']]) {
      const rows = (await reviewedJson(parent, file))[key];
      for (const row of rows) assert.ok(wholeResolution(actual, row), `Lost complete parent resolution ${parent}:${file}`);
    }
  }
});
function wholeResolution(value, expected) {
  return Boolean(findWholeResolution(value, expected));
}

const canonicalReview = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findWholeResolution(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if ((Object.hasOwn(expected, 'currentId') && value.currentId === expected.currentId || Object.hasOwn(expected, 'replacementId') && value.replacementId === expected.replacementId) && canonicalReview(value) === canonicalReview(expected)) return value;
  for (const child of Object.values(value)) {
    const found = findWholeResolution(child, expected);
    if (found) return found;
  }
}
async function mainSourceBytes(file) {
  if (file === 'tdf-mobile/src/features/generatedFeatureRegistry.ts') {
    const { stdout } = await execFileAsync('git', ['show', '4d866387ac6938d7aeaad5d1fa2985f8e02932e7:src/features/generatedFeatureRegistry.ts'], { cwd: path.join(workspaceRoot, 'tdf-mobile'), maxBuffer: 16 * 1024 * 1024 });
    return stdout; // exact dated fixture; the actual mirror has its own successor check below
  }
  return file === manifestPath ? JSON.stringify(await mainManifestView())
    : integrationSource('d65017d10f16e8089c43b82bdbcacf3177d206f5', file);
}

function findHistoricalReview(value, id) {
  if (!value) return undefined;
  if (value.id === id) return value;
  const previous = findHistoricalReview(value.previousDecision, id);
  if (previous) return previous;
  for (const branch of value.concurrentReviewHistory ?? []) {
    const found = findHistoricalReview(branch, id);
    if (found) return found;
  }
}


test('dated-storage-parent: refund-parent: actual Mobile mirror matches its generator and stale predecessor cannot authorize the new list', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-mobile-mirror-'));
  const mobileFile = 'tdf-mobile/src/features/generatedFeatureRegistry.ts';
  const generatedFile = 'src/features/generatedFeatureRegistry.ts';
  const register = JSON.parse(await readPreservedParentFile('d65017d10f16e8089c43b82bdbcacf3177d206f5', catalogPath, 'utf8'));
  const current = register.decisions.find(row => row.id === 'ea29c6e8a6cd165802a8');
  assert.equal(current?.evidence.valueCount, 165);
  assert.equal(current.previousDecision.id, 'fd606f7d00f82c78cb20');
  try {
    const output = path.join(temporary, 'generated');
    await fs.mkdir(path.join(output, 'src/features'), { recursive: true });
    await generatePreservedParentMirror('d65017d10f16e8089c43b82bdbcacf3177d206f5', output);
    const source = await integrationSource('d65017d10f16e8089c43b82bdbcacf3177d206f5', mobileFile);
    assert.equal(source, await fs.readFile(path.join(output, generatedFile), 'utf8'));
    const repo = path.join(temporary, 'repo');
    await fs.mkdir(repo);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repo });
    await writeFile(repo, mobileFile, source);
    await execFileAsync('git', ['add', '--', mobileFile], { cwd: repo });
    const report = JSON.parse(await integrationSource('d65017d10f16e8089c43b82bdbcacf3177d206f5', 'docs/catalog-persistence/reports/static-list-inventory.json'));
    const ids = new Set(report.candidates.filter(row => row.file === mobileFile).map(row => row.id));
    const selected = register.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size);
    assert.ok(ids.has(current.id));
    const decisions = path.join(temporary, 'decisions.json');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [auditScriptPath, '--root', repo, '--decisions', decisions, '--fail-on-unreviewed'];
    await execFileAsync(process.execPath, args, { cwd: repo });
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row.id === current.id ? current.previousDecision : row)));
    await assert.rejects(execFileAsync(process.execPath, args, { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});
}

// Retained complete assertions from cdf57961e4058f403ee4dd903bacfced2a5ee258; dated inputs are explicitly projected.
{

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');
const workspaceRoot = path.resolve(testDir, '../..');

// 2026-11-02 registry refresh: the only field ever added inside an already
// reviewed manifest prefix is the compatible applied checksum of the
// regenerated 2026-09-18 identity registry. Historical prefix hashes are taken
// over the manifest as it was reviewed, i.e. without that field.
const REGISTRY_REFRESH_ENTRY = '2026-09-18_identity_review_dependencies';
// 2026-11-02 session-expiry integration: protected main's migration is
// interleaved at index 227, ahead of the seven audit migrations. The audit's
// earlier reviews are verified over the manifest without that entry.
const SESSION_EXPIRY_ENTRY = '2026-10-31_api_token_session_expiry';
const APPLE_POLICY_ENTRY = '2026-10-13_apple_sign_in_policy';
const APPLE_ISSUER_ENTRY = '2026-11-07_apple_provider_identity_issuer';
// 2026-11-08 marketplace offers (ADR 0134) append after the Apple issuer entry.
const MARKETPLACE_OFFERS_ENTRY = '2026-11-08_marketplace_offers';
// 2026-11-10 auth rate limit (owner decisions of the formal audit) appends last.
const AUTH_RATE_LIMIT_ENTRY = '2026-11-10_auth_rate_limit';
// 2026-11-12 marketplace agreements (ADR 0135) append after the auth_rate_limit entry.
const MARKETPLACE_AGREEMENTS_ENTRY = '2026-11-12_marketplace_agreements';
// 2026-11-11 delegated event administrators: one migration appended last to the
// 251-entry manifest of protected main 8ec7d85c. Earlier reviews are verified
// over the manifest without that entry.
const EVENT_ADMIN_ENTRY = '2026-11-11_event_admin_delegation';
const EVENT_ADMIN_PROTECTED_MAIN = '8ec7d85c672e844e28c38e49c087b102032582d8';
const EVENT_ADMIN_INTRODUCTION = '2703eb3a1ac1604829e902262fe2dbfbbf4f4ca8';
// The marketplace agreement payments migration (ADR 0136) appends one entry to
// the 252-entry manifest of protected main d4bb9603. Earlier reviews are
// verified with the records stepped back to the main they reviewed.
const MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY = '2026-11-12_marketplace_agreement_payments';
const AGREEMENT_PAYMENTS_PROTECTED_MAIN = 'd4bb96030b1182e165ff39b614315c6a2554aee7';
const AGREEMENT_PAYMENTS_INTRODUCTION = '26c83d9e6525d859b0da0bebf959ad6d1c806159';

async function reviewedJson(revision, file) {
  const { stdout } = await execFileAsync('git', ['show', `${revision}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 });
  return JSON.parse(stdout);
}
const historicalReview = (entry, count) => {
  while (entry && entry.evidence?.valueCount !== count) entry = entry.previousDecision;
  assert.ok(entry, `Missing exact reviewed ${count}-entry predecessor`);
  return entry;
};
const auditView = entries => entries.filter(entry => entry.id !== SESSION_EXPIRY_ENTRY);
const asReviewedBeforeRegistryRefresh = entries => entries.map(entry => {
  if (entry.id !== REGISTRY_REFRESH_ENTRY) return entry;
  const { compatibleAppliedChecksums, ...reviewed } = entry;
  assert.deepEqual(compatibleAppliedChecksums,
    ['5636028805b77a1705bb364c87669e8d46983c93d9fc177da9e2187b98972f4f']);
  return reviewed;
});

const DELIVERY_PROTOCOL_REVIEW = '755cf14ac92e4f590879';
const PLAYER_SERVER_MAIN_BASE = '2ebb5c38bef08f3205877dc9521026b40b68d4ef';
const PLAYER_SERVER_REVIEW_HEAD = '85255658262166ab4aeb0e7e167b3a93c4a33d61';
const manifestPath = 'scripts/production-migrations.json';
const catalogPath = 'docs/catalog-persistence/catalog-list-decisions.json';
const playerMigrations = new Set([
  '2026-10-30_artist_featured_track',
  '2026-11-06_music_media_health',
  '2026-11-06_music_legacy_source_compatibility',
  '2026-11-07_music_featured_source_health',
  '2026-11-07_artist_featured_identity_guard',
]);

async function manifestBeforePlayer() {
  const manifest = (await mainManifestView());
  return { ...manifest, migrations: manifest.migrations.filter(row => !playerMigrations.has(row.id)) };
}

// Lists whose source file moved without any change to their values (ADR 0133).
// A list id follows its file, so each move is recorded as a successor review
// that nests the exact earlier review; checks against an earlier main read the
// successor back to that review.
const MOVED_FILE_REVIEW_BATCHES = new Set([
  'paypal-webhook-domain-module-2026-11-10',
  'web-auth-state-layer-2026-11-17',
  'web-feature-registry-layer-2026-11-17',
]);

function movedFileSuccessors(active) {
  const moved = active.filter(row => MOVED_FILE_REVIEW_BATCHES.has(row.reviewBatch));
  for (const row of moved) {
    assert.equal(row.reviewMethod, 'successor-of-reviewed-decision');
    assert.ok(row.previousDecision?.reviewed, `moved list ${row.id} must nest a reviewed decision`);
    assert.equal(row.classification, row.previousDecision.classification);
    assert.equal(row.disposition, row.previousDecision.disposition);
  }
  return moved;
}

// Every moved list whose earlier review protected main held nests that exact review.
function assertMovedListsNest(active, priorMain) {
  const prior = new Map(priorMain.map(row => [row.id, row]));
  return movedFileSuccessors(active).filter(row => prior.has(row.previousDecision.id)).map(row => {
    assert.deepEqual(row.previousDecision, prior.get(row.previousDecision.id));
    return row.previousDecision.id;
  });
}

// The continuous improvement loop rework (pull request #750) deleted two automation
// scripts: their reviews are retired as provenance and one new list is reviewed.
const LOOP_REWORK_RECONCILIATION = 'continuousImprovementLoopRework20261113';

async function readLoopRework() {
  return JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8')).reconciliation[LOOP_REWORK_RECONCILIATION];
}

async function readHistoricalReconciliation() {
  const current = await readReconciliation();
  const rework = await readLoopRework();
  const added = new Set(rework.addedDecisionIds);
  const active = current.active.filter(row => !added.has(row.id));
  for (const entry of [...rework.decisions].sort((left, right) => left.position - right.position)) {
    active.splice(entry.position, 0, entry.decision);
  }
  return { ...current, active };
}

async function readReconciliation(beforePlayer = false) {
  const ledger = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/event-catalog-retirements.json', 'utf8'));
  const register = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const mainRegister = await reviewedJson(INTEGRATION_MAIN, catalogPath);
  const active = mainRegister.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Dated main review missing: ${row.id}`);
    return found;
  });
  // Reviews whose source list was deleted outright (no successor list). They
  // leave the active register and are kept, unchanged, as history.
  const retired = (register.reconciliation.sourceRemovalRetirements ?? []).flatMap(entry => entry.decisions);
  let successors = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/event-catalog-current-successors.json', 'utf8')).successors;
  let integration = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/event-integration-retirements.json', 'utf8'));
  const parentPointers = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-catalog-current-successors.json');
  successors = parentPointers.successors.map(row => {
    const found = findWholeResolution(successors, row);
    assert.ok(found, `Dated main resolution missing: ${row.name}`);
    return found;
  });
  const parentRetirements = await reviewedJson(INTEGRATION_MAIN, 'docs/catalog-persistence/event-integration-retirements.json');
  integration = { ...integration, retirements: parentRetirements.retirements.map(row => {
    const found = findWholeResolution(integration, row);
    assert.ok(found, `Dated main retirement missing: ${row.source.name}`);
    return found;
  }) };
  if (!beforePlayer) return { ledger, active, retired, successors, integration };

  // Earlier reviews retain their own manifest fingerprint and catalog size.
  const predecessor = (await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const tip = active.find(row => row.id === pointer.currentId);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  const byId = new Map(active.map(row => row === tip ? row.previousDecision : row)
    .map(row => [row.id, row]));
  // A moved list's successor stands in for the review it nests.
  for (const row of movedFileSuccessors(active)) byId.set(row.previousDecision.id, row);
  // EVT-TICKET-SALE-001 extended the typed client registry; its successor
  // review nests the 86-method review unchanged and stands in for it.
  const ticketSaleClient = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  byId.set(ticketSaleClient.previousDecision.id, ticketSaleClient);
  // The loop rework (pull request #750) retired two reviews of deleted scripts as
  // provenance; the earlier catalog still lists them, so they stand in for themselves.
  for (const entry of (await readLoopRework()).decisions) byId.set(entry.decision.id, entry.decision);
  return {
    ledger,
    retired,
    active: predecessor.map(row => byId.get(row.id)).filter(Boolean),
    successors: successors.map(row => row === pointer ? row.previousResolution : row),
    integration: { ...integration, retirements: integration.retirements
      .map(row => row === retirement ? row.previousResolution : row) },
  };
}

test('dated-storage-parent: storage-parent: music/main reconciliation retains both reviews and the protected migration prefix', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/music-main-reconciliation.json', 'utf8'));
  const { active, successors, integration } = await readReconciliation(true);
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
  const manifest = asReviewedBeforeRegistryRefresh((await mainManifestView()).migrations);
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
    const extension = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/public-ux-main-reconciliation-20261021.json', 'utf8'));
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

test('dated-storage-parent: storage-parent: memory226 and ledger succession preserve213/214/220/221/222/224/225 reviews and reject stale approvals', async () => {
  const document = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
  const transition = document.reconciliation.directorySavedSearchParity;
  const financial = document.reconciliation.directoryFinancialMainIntegration;
  const { active, successors, integration } = await readReconciliation(true);
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
  const storageMerge = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const rawManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references' && !storageMerge.storageMigrationIds.includes(entry.id));
  const manifest = asReviewedBeforeRegistryRefresh(rawManifest);
  assert.equal(canonicalHash(manifest.slice(0, 213)), protectedPrefixHash);
  assert.equal(transition.protectedMainManifestCanonicalSha256, protectedPrefixHash);
  const protectedFinancialPrefixHash = 'c025cf3d2ab92f64bfb92d3656384d7fe0da6919457440d3c24301d7f6b8f232';
  assert.equal(canonicalHash(manifest.slice(0, 220)), protectedFinancialPrefixHash);
  assert.equal(financial.protectedMainManifestCanonicalSha256, protectedFinancialPrefixHash);
  assert.equal(financial.sourceMain, '5114fe95ac8e328b58162a8dcae5c9015a47ce09');
  assert.equal(manifest.length, 239);
  assert.deepEqual(manifest[227], {
    id: SESSION_EXPIRY_ENTRY,
    path: 'tdf-hq/sql/2026-10-31_api_token_session_expiry.sql',
    introducedBy: '8187065f81fad94677c8ba27760ab2cb74f58f79',
  });
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
  assert.equal(canonicalHash(manifest.slice(223, 227)),
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
  const latestCurrent = active.find(entry => entry.id === pointer.currentId);
  const finalCurrent = historicalReview(latestCurrent, 239);
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
  assert.equal(finalCurrent.evidence.valueCount, 239);
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
  const provider = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.providerDomainMainIntegration.document), 'utf8'));
  const aggregate = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.eventMemoryProviderMain20261026.document), 'utf8'));
  const memory226 = history.find(entry => entry.id === 'f48c34252ff8930b3d74');
  const provider223 = history.find(entry => entry.id === provider.currentDecisionId);
  assert.equal(finalCurrent.id, 'd7fc1df6cbade4a92d25');
  // Contract store (2026-11-04, CONTRACT-STORE-001): one migration appended after
  // the 238-entry review, which stays retired and hash-preserved.
  const reviewed238 = history.find(entry => entry.id === '050f309300a1688162fa');
  assert.equal(canonicalHash(reviewed238),
    '8c6aa26845f40678a9625f41a2d67b5dc367d290f4b0432639fd8472b85f4b8b');
  assert.equal(reviewed238.evidence.valueCount, 238);
  assert.equal(reviewed238.reviewed, true);
  assert.equal(finalCurrent.previousDecision.id, reviewed238.id);
  const contractStore = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3ContractStore20261104.document), 'utf8'));
  assert.equal(contractStore.currentDecisionId, finalCurrent.id);
  assert.deepEqual(contractStore.previousManifestDecision, reviewed238);
  assert.equal(contractStore.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 238)));
  assert.equal(contractStore.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(238)));
  assert.deepEqual(contractStore.appendedMigrationIds, ['2026-11-04_contract_store']);
  // Session invoice issuance (2026-11-04, PAY-INVOICE-ISSUE-001): one migration
  // appended after the 237-entry review, which stays retired and hash-preserved.
  const reviewed237 = history.find(entry => entry.id === 'ef3b8933af3854450a02');
  assert.equal(canonicalHash(reviewed237),
    'b3e101545dbce2e26e9603fbc81d735463b22f0cb486b4ae9342f724131fa145');
  assert.equal(reviewed237.evidence.valueCount, 237);
  assert.equal(reviewed237.reviewed, true);
  assert.equal(reviewed238.previousDecision.id, reviewed237.id);
  const invoiceIssue = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3SessionInvoiceIssue20261104.document), 'utf8'));
  assert.equal(invoiceIssue.currentDecisionId, reviewed238.id);
  assert.deepEqual(invoiceIssue.previousManifestDecision, reviewed237);
  assert.equal(invoiceIssue.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 237)));
  assert.equal(invoiceIssue.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(237, 238)));
  assert.deepEqual(invoiceIssue.appendedMigrationIds, ['2026-11-04_session_invoice_issue']);
  // Directory review states (2026-11-04, F-09): one migration appended after the
  // 236-entry review merged to main, which stays retired and hash-preserved.
  const merged236 = history.find(entry => entry.id === '438668da134480da0a9e');
  assert.equal(canonicalHash(merged236),
    'fae5221769f1e07714cd6160b57b8ae09146c3840d305f109d81baa176396032');
  assert.equal(merged236.evidence.valueCount, 236);
  assert.equal(merged236.reviewed, true);
  assert.equal(reviewed237.previousDecision.id, merged236.id);
  const reviewStates = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase3DirectoryReviewStates20261104.document), 'utf8'));
  assert.equal(reviewStates.currentDecisionId, reviewed237.id);
  assert.deepEqual(reviewStates.previousManifestDecision, merged236);
  assert.equal(reviewStates.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 236)));
  assert.deepEqual(reviewStates.appendedMigrationIds, ['2026-11-04_directory_review_status_without_pending']);
  // Domo deposit validator (2026-11-04): one migration appended after the
  // reconciled 235-entry review, which stays retired and hash-preserved.
  const reconciled235 = history.find(entry => entry.id === '27def070486792cad395');
  assert.equal(canonicalHash(reconciled235),
    '085659be48eed02b09860767b22855e80f1fe17bddca460289a6ab13504fca03');
  assert.equal(reconciled235.evidence.valueCount, 235);
  assert.equal(reconciled235.reviewed, true);
  assert.equal(merged236.previousDecision.id, reconciled235.id);
  const depositValidator = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2DomoDepositValidator20261104.document), 'utf8'));
  assert.equal(depositValidator.currentDecisionId, merged236.id);
  assert.equal(depositValidator.previousDecisionId, reconciled235.id);
  assert.deepEqual(depositValidator.previousManifestDecision, reconciled235);
  assert.equal(depositValidator.preservedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 235)));
  assert.equal(depositValidator.appendedSuffixCanonicalSha256, canonicalHash(rawManifest.slice(235, 236)));
  assert.deepEqual(depositValidator.appendedMigrationIds, ['2026-11-04_domo_quote_deposit_policy_validator']);
  // Session-expiry integration (2026-11-02): two concurrent successors of the
  // 227-entry review are reconciled; both stay retired and hash-preserved.
  const audit234 = history.find(entry => entry.id === 'd9468b767dadec9cf3d8');
  const sessionExpiry228 = history.find(entry => entry.id === '040c32c5b44ceadb2dde');
  assert.equal(canonicalHash(audit234),
    '61cbf1e27157b704dcad31fb8800df2d3498c2b57efb3dbc519f63f8b38e1c18');
  assert.equal(canonicalHash(sessionExpiry228),
    'e863458f044f4994d17134c7cd04ae00e8d48c95c2f59f23f076c6d27cfcf85e');
  assert.equal(audit234.evidence.valueCount, 234);
  assert.equal(sessionExpiry228.evidence.valueCount, 228);
  assert.equal(audit234.reviewed, true);
  assert.equal(sessionExpiry228.reviewed, true);
  assert.equal(reconciled235.previousDecision.id, audit234.id);
  assert.deepEqual(reconciled235.concurrentReviewHistory.map(entry => entry.id), [sessionExpiry228.id]);
  assert.equal(sessionExpiry228.previousDecision.id, 'a2bb0e828d5b94c1cb31');
  for (const parent of [audit234, sessionExpiry228]) {
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const sessionExpiry = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2SessionExpiryMainIntegration20261102.document), 'utf8'));
  assert.equal(sessionExpiry.currentDecisionId, reconciled235.id);
  assert.equal(sessionExpiry.previousDecisionId, audit234.id);
  assert.equal(sessionExpiry.concurrentDecisionId, sessionExpiry228.id);
  assert.deepEqual(sessionExpiry.mainManifestDecision, sessionExpiry228);
  assert.deepEqual(sessionExpiry.auditManifestDecision, audit234);
  assert.equal(sessionExpiry.interleavedIndex, 227);
  assert.equal(rawManifest.length, 239);
  // Protected main's 228-entry review is the exact prefix of the merged manifest.
  assert.equal(sessionExpiry.protectedPrefixCanonicalSha256, canonicalHash(rawManifest.slice(0, 228)));
  // The audit's 234-entry review is the merged manifest without main's entry.
  // Earlier reviews were taken over the 235-entry manifest before the deposit validator.
  const reconciledRaw = rawManifest.slice(0, 235);
  assert.equal(sessionExpiry.auditViewCanonicalSha256, canonicalHash(auditView(reconciledRaw)));
  assert.equal(auditView(reconciledRaw).length, 234);
  assert.equal(sessionExpiry.auditSuffixCanonicalSha256, canonicalHash(rawManifest.slice(228, 235)));
  const auditRaw = auditView(reconciledRaw);
  const auditReviewed = auditView(manifest.slice(0, 235));
  // Registry refresh (2026-11-02): one migration appended after the 233-entry
  // formal-audit review, which stays retired and preserved by canonical hash.
  const main233 = history.find(entry => entry.id === '903e61463c0222f30d9b');
  assert.equal(canonicalHash(main233),
    '9adad9f469eb233e58e2cbfac22651f797d7aba5e438d1f10ce880c436e0566c');
  assert.equal(main233.evidence.valueCount, 233);
  assert.equal(main233.reviewed, true);
  assert.equal(audit234.previousDecision.id, main233.id);
  const refresh = JSON.parse(await fs.readFile(path.join(workspaceRoot,
    document.reconciliation.formalAuditPhase2RegistryRefresh20261102.document), 'utf8'));
  assert.equal(refresh.currentDecisionId, audit234.id);
  assert.equal(refresh.previousDecisionId, main233.id);
  assert.equal(refresh.mainDecisionCanonicalSha256, canonicalHash(main233));
  assert.deepEqual(refresh.mainManifestDecision, main233);
  assert.equal(refresh.preservedPrefixCanonicalSha256, canonicalHash(auditRaw.slice(0, 233)));
  assert.equal(refresh.preservedPrefixCanonicalSha256BeforeCompatibleChecksum, canonicalHash(auditReviewed.slice(0, 233)));
  assert.equal(refresh.appendedSuffixCanonicalSha256, canonicalHash(auditRaw.slice(233)));
  assert.deepEqual(refresh.appendedMigrationIds, ['2026-11-02_identity_party_reference_refresh']);
  assert.equal(refresh.reviewedPrefixCanonicalSha256WithoutCompatibleChecksum['227'],
    '3fb0233724d5f6080b6d8933e46f0cd79c34b73fd3070e3e17b742ce1ff5113e');
  // The raw manifest differs from the reviewed one in exactly that one field.
  assert.equal(rawManifest[refresh.inPrefixChange.index].id, REGISTRY_REFRESH_ENTRY);
  assert.deepEqual(rawManifest[refresh.inPrefixChange.index].compatibleAppliedChecksums, refresh.inPrefixChange.added);
  for (const [index, entry] of rawManifest.entries()) {
    if (index === refresh.inPrefixChange.index) {
      const { compatibleAppliedChecksums: _added, ...rest } = entry;
      assert.deepEqual(rest, manifest[index]);
    } else assert.deepEqual(entry, manifest[index]);
  }
  // Formal audit phase 2 (2026-11-02): six audit migrations appended after the
  // exact 227-entry memory/provider review, which stays retired and preserved.
  const main227 = history.find(entry => entry.id === 'a2bb0e828d5b94c1cb31');
  assert.equal(canonicalHash(main227),
    '2a868127eeb27cf8ffcaf4d162dd6123bf0ae8ad9ce54901c0cabb631cf43e6e');
  assert.equal(main227.evidence.valueCount, 227);
  assert.equal(main227.reviewed, true);
  assert.equal(main233.previousDecision.id, main227.id);
  assert.equal(canonicalHash(manifest.slice(0, 227)),
    JSON.parse(await fs.readFile(path.join(workspaceRoot,
      document.reconciliation.formalAuditPhase2MainIntegration20261102.document), 'utf8')).preservedPrefixCanonicalSha256);
  assert.equal(canonicalHash(memory226),
    'd08e3ff9409b47ab035fc67269d79010fd3738333fdc971c305ded3ddfeadf9b');
  assert.equal(canonicalHash(provider223),
    '6bcc3d19c2aea1adb0ce6ede68d59ec60b025b953a083bc5be6090f4be90ed1a');
  assert.deepEqual(aggregate.memoryManifestDecision, memory226);
  assert.deepEqual(aggregate.mainManifestDecision, provider223);
  assert.equal(aggregate.memoryDecisionCanonicalSha256, canonicalHash(memory226));
  assert.equal(aggregate.mainDecisionCanonicalSha256, canonicalHash(provider223));
  assert.equal(canonicalHash(manifest.slice(0, 223)),
    '3123446d4ae490b41d13beb6ca62f17da39fbc90539791c9b5373709c05b1703');
  assert.equal(aggregate.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 223)));
  assert.equal(aggregate.memorySuffixCanonicalSha256, canonicalHash(manifest.slice(223, 227)));
  assert.deepEqual(manifest[222], {
    id: '2026-10-16_provider_domain_payment_binding',
    path: 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql',
    introducedBy: '456be073c944dd3d76eb9e3c2169fe7fbb4041ef',
  });
  assert.equal(provider.mainManifestDecision.id, header222.id);
  assert.equal(provider.mainManifestDecision.reviewed, true);
  assert.equal(provider.mainManifestDecision.evidence.valueCount, 222);
  assert.equal(canonicalHash(provider.mainManifestDecision),
    '866eef0dde46336e216336f13bdf9f83570314d2c17b9cf93f354de0f8ad9404');
  assert.equal(provider.mainDecisionCanonicalSha256, canonicalHash(provider.mainManifestDecision));
  assert.equal(canonicalHash(provider.providerManifestDecision),
    '018a1dc08aafe97b3af2257fe2ad24bac9a8f47f724975df0eec07e729ba8058');
  assert.equal(provider.preservedPrefixCanonicalSha256, canonicalHash(manifest.slice(0, 222)));
  for (const parent of [memory226, provider223]) {
    assert.equal(parent.reviewed, true);
    assert.equal(finalCurrent.classification, parent.classification);
    assert.equal(finalCurrent.disposition, parent.disposition);
    assert.ok(!active.some(entry => entry.id === parent.id));
  }
  const retiredParents = [...historical, current, memoryParent, memory225, header222,
    combined.headerManifestDecision, memory226, provider223, provider.providerManifestDecision, main227, main233,
    audit234, sessionExpiry228, reconciled235, merged236, reviewed237, reviewed238];
  assert.equal(current.classification, decision220.classification);
  assert.equal(current.disposition, decision220.disposition);
  assert.deepEqual(current.sourceReviewReferences.slice(0, decision220.sourceReviewReferences.length),
    decision220.sourceReviewReferences);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(current.evidence.valueCount, 221);
  assert.equal(pointer.currentId, latestCurrent.id);
  assert.equal(pointer.valueCount, 253);
  assert.equal(pointer.previousResolution.valueCount, 252);
  assert.equal(pointer.previousResolution.previousResolution.valueCount, 251);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 249);
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.valueCount, 248);
  assert.equal(pointer.change, latestCurrent.justification);
  const retirement = integration.retirements.find(entry => entry.source.name === 'migrations');
  assert.equal(retirement.replacementId, latestCurrent.id);
  assert.equal(retirement.currentResolution.id, latestCurrent.id);
  assert.equal(retirement.currentResolution.valueCount, 253);
  assert.equal(retirement.previousResolution.currentResolution.valueCount, 252);
  assert.equal(retirement.previousResolution.previousResolution.currentResolution.valueCount, 251);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 250);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 249);
  assert.equal(retirement.previousResolution.previousResolution.previousResolution.previousResolution.previousResolution.currentResolution.valueCount, 248);
  assert.equal(retirement.reason, latestCurrent.justification);
  for (const decision of retiredParents) {
    assert.ok(pointer.previousCurrentIds.includes(decision.id));
    assert.ok(retirement.previousReplacementIds.includes(decision.id));
  }

  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-alert-successor-test-'));
  try {
    const repoDir = path.join(tempRoot, 'repo');
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, 'scripts/production-migrations.json', JSON.stringify({ migrations: rawManifest }));
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

test('dated-storage-parent: storage-parent: event catalog retirement ledger preserves evidence and distinct reviewed successors', async () => {
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

test('dated-storage-parent: storage-parent: reconciled fingerprints match current sources; retired decisions still fail the gate', async () => {
  const { ledger, active, successors } = await readReconciliation();
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-reconciliation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');
  const decisionsPath = path.join(tempRoot, 'decisions.json');
  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    for (const file of new Set(ledger.retirements.map(entry => entry.file))) {
      await writeFile(repoDir, file, await mainSourceBytes(file));
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


test('dated-storage-parent: storage-parent: main integration archives exact old decisions and retains reviewed successors or source-removal evidence', async () => {
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

test('dated-storage-parent: storage-parent: catalog audit excludes ignored local source files from candidate fingerprints', async () => {
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

test('dated-storage-parent: storage-parent: decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
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
      // The removed review is kept as retirement provenance, so any history it
      // references stays reachable in the content-addressed store.
      sourceRemovalRetirements: [{
        reason: 'Catalog refresh unit-test-review: these candidates are no longer in the static list inventory. Their full reviewed decisions are preserved here as history; no catalog values are affected.',
        decisions: [{ ...preserved, id: 'stale-id' }],
      }],
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});


test('dated-storage-parent: storage-parent: ticket main merge retains stale approvals only as provenance', async () => {
  const document = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/catalog-list-decisions.json', 'utf8'));
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
      assert.doesNotMatch(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'tdf-mobile/app/ddex/index.tsx', 'utf8'), /const STATUS_LABELS/);
    }
  }
});


test('dated-storage-parent: storage-parent: storage/main integration preserves both exact parent reviews and manifest views', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/storage-main-integration-20261104.json', 'utf8'));
  const manifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY && entry.id !== '2026-10-20_video_campaigns' && entry.id !== '2026-11-06_video_campaign_identity_references');
  const { active, successors } = await readReconciliation();
  const extension = active.find(entry => entry.id === successors.find(item => item.name === 'migrations').currentId);
  const current = historicalReview(extension, 244);
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item) ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(manifest.length, 244);
  assert.equal(canonicalHash(manifest.slice(0, 239)), ledger.protectedMainPrefixCanonicalSha256);
  assert.equal(canonicalHash(manifest.filter(entry => !ledger.mainOnlyMigrationIds.includes(entry.id))), ledger.storageViewCanonicalSha256);
  assert.equal(current.evidence.valueCount, 244);
  assert.equal(successors.find(entry => entry.name === 'migrations').currentId, extension.id);
  for (const parent of ledger.parentReviews) {
    const entry = [current.previousDecision, ...current.concurrentReviewHistory].find(item => item.id === parent.id);
    assert.equal(canonicalHash(entry), parent.canonicalSha256);
    assert.ok(!active.some(item => item.id === parent.id));
  }
  assert.equal(ledger.executionAllowed, false);
});

test('dated-storage-parent: storage-parent: video campaign migration appends to the exact protected 244-entry manifest', async () => {
  const main = 'e5248e0b3ad30d832ef1ccbf5e595a7ee36b0324';
  const introduction = '4b84238abb4ca17c6aaa9902e3bec16a1cc3daa5';
  const old = JSON.parse((await execFileAsync('git', ['show', `${main}:scripts/production-migrations.json`], { cwd: workspaceRoot })).stdout).migrations;
  const current = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY && entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const priorDecisions = JSON.parse((await execFileAsync('git', ['show', `${main}:${decisionsPath}`], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout);
  const { active } = await readReconciliation(true);
  const latest = active.find(item => item.evidence?.file === manifestPath);
  const videoReview = historicalReview(latest, 245);
  assert.equal(videoReview.id, '969410f2f45854cc54fc');
  assert.deepEqual(videoReview.previousDecision,
    priorDecisions.decisions.find(item => item.id === 'be0b1503a04f9fa295ee'));
  assert.equal(old.length, 244);
  assert.equal(current.length, 246);
  assert.deepEqual(current.slice(0, 244), old);
  assert.deepEqual(current[244], { id: '2026-10-20_video_campaigns', path: 'tdf-hq/sql/2026-10-20_video_campaigns.sql', introducedBy: introduction });
  const originalSql = (await execFileAsync('git', ['show', `${introduction}:${current[244].path}`], { cwd: workspaceRoot })).stdout;
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[244].path), 'utf8'), originalSql);
  await execFileAsync('git', ['merge-base', '--is-ancestor', introduction, 'HEAD'], { cwd: workspaceRoot });
  const forward = '09fd7babb92e45c1da1b9e5017de53b6378b4f7c';
  assert.deepEqual(current[245], { id: '2026-11-06_video_campaign_identity_references', path: 'tdf-hq/sql/2026-11-06_video_campaign_identity_references.sql', introducedBy: forward });
  const priorManifest = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.deepEqual(current.slice(0, 245), priorManifest);
  const priorReview = JSON.parse((await execFileAsync('git', ['show', '7ab198f88f70ce7f27f0e97c34089d4145ce6929:' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === videoReview.id);
  assert.deepEqual(videoReview, priorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, current[245].path), 'utf8'), (await execFileAsync('git', ['show', forward + ':' + current[245].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', forward, 'HEAD'], { cwd: workspaceRoot });
  // ADR 0134: marketplace offers were reviewed on top of the exact 246-entry
  // identity manifest (8ee6c7a) and now append after the two Apple entries that
  // protected main integrated; the manifest without those entries is still that
  // reviewed 247-entry manifest byte for byte.
  const marketplace = '8ee6c7a66e2504f425863ca6f5b83062723efd58';
  const marketplaceIntroduction = '5901003189dac2291380604c66fdd66087e4a587';
  const full = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(full.length, 249);
  // The agreements entry (ADR 0135) is the 251st, the last one before the
  // event administrator entry: its SQL is exactly what its introduction commit
  // carries and that commit is an ancestor.
  const agreementsEntry = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY).at(-1);
  assert.equal(agreementsEntry.id, MARKETPLACE_AGREEMENTS_ENTRY);
  assert.equal(agreementsEntry.path, 'tdf-hq/sql/2026-11-12_marketplace_agreements.sql');
  assert.equal(await fs.readFile(path.join(workspaceRoot, agreementsEntry.path), 'utf8'), (await execFileAsync('git', ['show', agreementsEntry.introducedBy + ':' + agreementsEntry.path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', agreementsEntry.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  assert.deepEqual(full[248], { id: MARKETPLACE_OFFERS_ENTRY, path: 'tdf-hq/sql/2026-11-08_marketplace_offers.sql', introducedBy: marketplaceIntroduction });
  const identityManifest = JSON.parse((await execFileAsync('git', ['show', marketplace + ':scripts/production-migrations.json'], { cwd: workspaceRoot })).stdout).migrations;
  assert.equal(identityManifest.length, 247);
  // The marketplace entry's introduction moved to the review-fix commit (the
  // SQL gained the analytics allowlist and the filled -> published edge);
  // everything before it is still the reviewed identity manifest byte for byte.
  const withoutApple = full.filter(entry => entry.id !== APPLE_POLICY_ENTRY && entry.id !== APPLE_ISSUER_ENTRY);
  assert.deepEqual(withoutApple.slice(0, -1), identityManifest.slice(0, -1));
  assert.equal(withoutApple.at(-1).id, identityManifest.at(-1).id);
  const identityReview = historicalReview(latest, 246);
  assert.equal(identityReview.id, '806180f5957fc4a7520f');
  const identityPriorReview = JSON.parse((await execFileAsync('git', ['show', marketplace + ':' + decisionsPath], { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions.find(item => item.id === identityReview.id);
  assert.deepEqual(identityReview, identityPriorReview);
  assert.equal(await fs.readFile(path.join(workspaceRoot, full[248].path), 'utf8'), (await execFileAsync('git', ['show', marketplaceIntroduction + ':' + full[248].path], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', marketplaceIntroduction, 'HEAD'], { cwd: workspaceRoot });
});

test('dated-storage-parent: storage-parent: Apple policy preserves the exact main prefix and every manual review', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const fromGit = async file => (await execFileAsync('git', ['show', `${main}:${file}`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout;
  const oldManifest = JSON.parse(await fromGit('scripts/production-migrations.json')).migrations;
  // The marketplace offers entry (ADR 0134) appends after the Apple entries;
  // the Apple review is verified over the manifest without it.
  const currentManifest = (await manifestBeforePlayer()).migrations.filter(entry => entry.id !== MARKETPLACE_OFFERS_ENTRY && entry.id !== AUTH_RATE_LIMIT_ENTRY && entry.id !== MARKETPLACE_AGREEMENTS_ENTRY && entry.id !== EVENT_ADMIN_ENTRY && entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY);
  assert.equal(oldManifest.length, 246);
  assert.equal(currentManifest.length, 248);
  assert.deepEqual(currentManifest.slice(0, 246), oldManifest);
  const issuer = currentManifest[247];
  assert.deepEqual(issuer, { id: APPLE_ISSUER_ENTRY, path: 'tdf-hq/sql/2026-11-07_apple_provider_identity_issuer.sql', introducedBy: '7c1525edf0f02fe53f985c24862940b91c11cdfa' });
  assert.equal(await fs.readFile(path.join(workspaceRoot, issuer.path), 'utf8'), (await execFileAsync('git', ['show', `${issuer.introducedBy}:${issuer.path}`], { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', issuer.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  const addition = currentManifest[246];
  assert.equal(addition.id, APPLE_POLICY_ENTRY);
  assert.equal(addition.path, 'tdf-hq/sql/2026-10-13_apple_sign_in_policy.sql');
  const original = JSON.parse((await execFileAsync('git', ['show',
    'e7ec452787e15f09deda4c4328ec6a8c1cece727:scripts/production-migrations.json'],
  { cwd: workspaceRoot })).stdout).migrations.find(row => row.id === APPLE_POLICY_ENTRY);
  assert.deepEqual(addition, original);
  assert.equal(await fs.readFile(path.join(workspaceRoot, addition.path), 'utf8'),
    (await execFileAsync('git', ['show', `${addition.introducedBy}:${addition.path}`],
      { cwd: workspaceRoot })).stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', addition.introducedBy, 'HEAD'],
    { cwd: workspaceRoot });
  const old = JSON.parse(await fromGit('docs/catalog-persistence/catalog-list-decisions.json')).decisions;
  // Compare the Apple review with the records as protected main 8ec7d85c held
  // them: the event administrator successors are read back to the review,
  // pointer and retirement they replaced (each nests its predecessor
  // unchanged), and lists first reviewed after that main are left out.
  const { active: currentDecisions, retired, successors: currentSuccessors,
    integration: currentIntegration } = await readReconciliation(true);
  const mainIds = new Set((await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/catalog-list-decisions.json')).decisions.map(row => row.id));
  // Later successors (the agreement payments manifest review, ADR 0136) nest
  // the event administrator one, so step back until the record that main held.
  const active = currentDecisions.flatMap(row => {
    let reviewed = row;
    while (reviewed && !mainIds.has(reviewed.id)) reviewed = reviewed.previousDecision;
    return reviewed ? [reviewed] : [];
  });
  const heldByMain = (row, idOf) => {
    let held = row;
    while (held && !mainIds.has(idOf(held))) held = held.previousResolution;
    return held;
  };
  const successors = currentSuccessors.map(row => row.name === 'migrations' ? heldByMain(row, held => held.currentId) : row);
  const integration = { ...currentIntegration, retirements: currentIntegration.retirements
    .map(row => row.source.name === 'migrations' ? heldByMain(row, held => held.replacementId) : row) };
  const reviewedParent = async revision => JSON.parse((await execFileAsync('git',
    ['show', `${revision}:docs/catalog-persistence/catalog-list-decisions.json`],
    { cwd: workspaceRoot, maxBuffer: 16 * 1024 * 1024 })).stdout).decisions;
  const priorApple = await reviewedParent('bc16d557c7b246361c83d78191314087ca2dd3d9');
  const currentMain = await reviewedParent('321f4fe6c8e803ab1c32724a8ce5b7814418b79c');
  const offlineMain = await reviewedParent('5c69dc30318d110d9daba042571b8e54f5bf140d');
  const offlineFixture = offlineMain.find(row => row.id === 'b3eadb9d52a262f5371f');
  assert.ok(offlineFixture, 'the offline currency fixture needs its exact protected-main review');
  assert.deepEqual(active.find(row => row.id === offlineFixture.id), offlineFixture);
  // Protected main later reviewed two route-specification lists (#710); keep
  // their exact reviews and leave them out of the historical Apple view too.
  const routeMain = await reviewedParent('b1614afd18b02e8cd0d46f71d798b7f38764bcec');
  const routeReviews = ['afe675892f43a8a4de73', '6f876292d2fcb1dd7a86']
    .map(id => routeMain.find(row => row.id === id));
  for (const review of routeReviews) {
    assert.ok(review, 'the route-specification reviews need their exact protected-main review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The desktop shell (#717) adds nine reviewed playback and desktop lists;
  // keep their exact branch reviews outside the historical Apple view as well.
  const desktopBranch = await reviewedParent('3668640fa56a558bbd3bd95881526d5faf5be22c');
  const desktopReviews = ['9f5a58afbcc5a21f3a81', '162bb73e8a27ecdbd80d', '19b53762aa84c847059a',
    'd3fe8dedc63dbee01dcf', 'dd837be242ffaf6c73c5', '70eef2bc5a9e8ef6ab65', 'ee97985b0d9abc1b1ed3',
    '57fc580c92ca2352698d', '2cf4da8b8443b9be1b48'].map(id => desktopBranch.find(row => row.id === id));
  for (const review of desktopReviews) {
    assert.ok(review, 'the desktop shell reviews need their exact branch review');
    assert.deepEqual(active.find(row => row.id === review.id), review);
  }
  // The marketplace offers branch (ADR 0134, #719) adds eighteen value lists
  // reviewed by the deterministic policy, extends the search page view-mode
  // attribute list (successor of main's exact review 7393e5dd10ebe68ae3e2) and
  // records a successor review of the 249-entry manifest whose exact
  // predecessor is main's 248-entry review; the historical Apple view keeps
  // the exact predecessors and leaves the new lists out.
  const marketplaceReviewIds = ['3d30e49db2fefa10a623', '2471af5d3bf550aade51', '0365580729887c8f5ca7',
    '0dc607f91bd630b1fb87', '4ed56a39eea6e4d4679d', 'aa6a65391d2c0611fdd8',
    'b79670b0b1a1fbc1ee05', 'df679c5b2cb5d91be79e', 'ef5bffc43b9c0ff3a361', '39edb2a76ecef8cd6c57',
    '2a350b9da29a2d080bd8', 'd776481beacf4f4a074f', '16393adee900667601af', '7207f087ef5f8e0b2158',
    '3a7f374bb3b52ef8d34d', 'a9c6757fbe6f2868f4f2', '853a5f768e09d0c32259',
    // Listing-conditions labels on the public classified page (review fix round four).
    '9dd6b72c9435bd64fe09'];
  for (const id of marketplaceReviewIds) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace offers review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  // The auth_rate_limit migration (owner decisions of 2026-11-10) succeeds the
  // marketplace manifest review as the 250-entry tip and adds two reviews; the
  // historical view sets those aside and steps the manifest back twice.
  // The marketplace agreements migration (ADR 0135) succeeds the auth_rate_limit
  // review as the 251-entry tip and adds six reviews (status filter, panel
  // tabs, agreement CHECK lists and the party type); the historical view sets
  // those aside too and steps the manifest back once more.
  const agreementsTip = active.find(row => row.id === 'bc5e5bd4f9d8795d2166');
  assert.equal(agreementsTip.evidence.valueCount, 251);
  const agreementReviews = ['3a5425de732f609db717', '75f5a77be26697f0a281', '06f1e7eef25d710385f4',
    '74460721a4c7f744e3ab', '33d907b3850fb5474551', 'c9a145a03e59580950b5'];
  for (const id of agreementReviews) {
    const review = active.find(row => row.id === id);
    assert.ok(review?.reviewed, `the marketplace agreements review ${id} must be present and reviewed`);
    assert.match(review.reviewMethod, /^deterministic-policy-v\d+$/);
  }
  const authRateLimitTip = agreementsTip.previousDecision;
  assert.equal(authRateLimitTip.id, '52d96d4b31e677ac5948');
  assert.equal(authRateLimitTip.evidence.valueCount, 250);
  const authRateLimitReviews = ['f37745d92e494e02fcba', 'a95cbb8b9256407889ea'];
  const marketplaceManifestReview = authRateLimitTip.previousDecision;
  assert.equal(marketplaceManifestReview.id, '4dc5a2f26154a0f125db');
  assert.equal(marketplaceManifestReview.evidence.valueCount, 249);
  assert.equal(marketplaceManifestReview.previousDecision.id, 'a20e04bc426cc6383584');
  const searchViewReview = active.find(row => row.id === '79bdbe2ae9b350e0d1da');
  assert.equal(searchViewReview.reviewMethod, 'successor-of-reviewed-decisions');
  assert.equal(searchViewReview.previousDecision.id, '7393e5dd10ebe68ae3e2');
  const marketplaceSuccessors = new Map([
    [agreementsTip.id, marketplaceManifestReview.previousDecision],
    [searchViewReview.id, searchViewReview.previousDecision]]);
  const laterMainIds = new Set([offlineFixture.id, ...routeReviews.map(row => row.id),
    ...desktopReviews.map(row => row.id), ...marketplaceReviewIds, ...authRateLimitReviews, ...agreementReviews]);
  const historicalAppleView = active.filter(row => !laterMainIds.has(row.id))
    .map(row => marketplaceSuccessors.get(row.id) ?? row);
  assert.equal(old.length, 1405);
  assert.equal(priorApple.length, 1407);
  assert.equal(currentMain.length, 1409);
  const retiredIds = new Set(retired.map(row => row.id));
  assert.equal(historicalAppleView.length, 1411 - retired.length);
  const appleView = (await reviewedParent('15e2228d24080a520d4bed187731cc3db09d474a'))
    .filter(row => !retiredIds.has(row.id));
  assert.equal(historicalAppleView.length, appleView.length);
  for (const [index, row] of appleView.entries()) {
    assert.deepEqual(historicalAppleView[index], row, `Apple review order or content changed at ${row.id}`);
  }
  assert.equal(active.length,
    1423 + marketplaceReviewIds.length + authRateLimitReviews.length + agreementReviews.length - retired.length);
  const history = new Map(retired.map(row => [row.id, row]));
  const visit = row => {
    if (!row) return;
    if (!history.has(row.id)) history.set(row.id, row);
    visit(row.previousDecision);
    for (const branch of row.concurrentReviewHistory ?? []) visit(branch);
  };
  for (const row of active) visit(row);
  for (const row of [...old, ...priorApple, ...currentMain, ...offlineMain, ...routeMain]) {
    assert.deepEqual(history.get(row.id), row);
  }
  const pointer = successors.find(row => row.name === 'migrations');
  const agreementsCurrentTip = active.find(row => row.id === pointer.currentId);
  assert.equal(agreementsCurrentTip.evidence.valueCount, 251);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').replacementId,
    agreementsCurrentTip.id);
  assert.equal(pointer.previousResolution.currentId, agreementsCurrentTip.previousDecision.id);
  const currentTip = agreementsCurrentTip.previousDecision;
  assert.equal(currentTip.evidence.valueCount, 250);
  assert.equal(pointer.previousResolution.previousResolution.currentId, currentTip.previousDecision.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousReplacementId,
    currentTip.id);
  const marketplaceTip = currentTip.previousDecision;
  assert.equal(marketplaceTip.evidence.valueCount, 249);
  const tip = marketplaceTip.previousDecision;
  assert.equal(tip.evidence.valueCount, 248);
  assert.equal(historicalReview(tip, 247).previousDecision.id, '806180f5957fc4a7520f');
  assert.equal(tip.previousDecision.id, '4e282e4c258854caa655');
  assert.equal(pointer.previousResolution.previousResolution.previousResolution.currentId, tip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousReplacementId,
    marketplaceTip.id);
  assert.equal(integration.retirements.find(row => row.source.name === 'migrations').previousResolution.previousResolution.previousReplacementId,
    tip.id);
  const security = active.find(row => row.evidence?.file === 'tdf-hq/src/TDF/Catalog/Security.hs'
    && row.evidence?.name === 'expectedSecurityRoleAssignmentPolicyBindings');
  assert.equal(security.previousDecision.id, 'ff4edc0f8d2debecf71c');
  const currentSecurity = await fs.readFile(path.join(workspaceRoot, security.evidence.file), 'utf8');
  const withoutApple = currentSecurity.replace(
    '  , ("account.apple.customer", "apple-account-create", "customer", True)\n', '');
  assert.notEqual(withoutApple, currentSecurity);
  assert.equal(withoutApple, await fromGit(security.evidence.file));
});

test('dated-storage-parent: storage-parent: event administrator delegation appends one migration to the protected 251-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: EVENT_ADMIN_ENTRY, path: 'tdf-hq/sql/2026-11-11_event_admin_delegation.sql',
    introducedBy: EVENT_ADMIN_INTRODUCTION };
  assert.equal(main.migrations.length, 251);
  // The agreement payments entry (ADR 0136) appends after this one and has its own review.
  assert.deepEqual({ ...combined, migrations: combined.migrations.filter(entry => entry.id !== MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY) },
    { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${EVENT_ADMIN_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', EVENT_ADMIN_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN, decisionsPath)).decisions;
  // The active manifest review is the agreement payments successor; this
  // review, its pointer and its retirement are nested one step back, unchanged.
  const tipPointer = successors.find(row => row.name === 'migrations');
  const pointer = tipPointer.previousResolution;
  const current = active.find(row => row.id === tipPointer.currentId).previousDecision;
  assert.equal(current.id, pointer.currentId);
  assert.equal(current.evidence.valueCount, 252);
  assert.equal(current.reviewed, true);
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 251);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations').previousResolution;
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(EVENT_ADMIN_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // The typed client registry is the only other successor; it also nests its parent review.
  // EVT-TICKET-SALE-001 later added four methods: its 90-method successor keeps
  // this 86-method review intact as its previousDecision.
  const clientNow = active.find(row => row.id === successors.find(row => row.name === 'SocialEventsAPI').currentId);
  assert.equal(clientNow.evidence.valueCount, 90);
  const client = clientNow.previousDecision;
  assert.equal(client.id, '083bd1a66e9f23f02866');
  assert.equal(client.evidence.valueCount, 86);
  assert.deepEqual(client.previousDecision, priorMain.find(row => row.id === '2511cbba2fc7279bc28a'));
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const movedReviews = assertMovedListsNest(active, priorMain);
  // Every other review of protected main is retained exactly; three lists are new.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, client.previousDecision.id, ...movedReviews]);
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else assert.deepEqual(history.get(row.id), row);
  }
  assert.equal(priorMain.length, 1449);
  // 1452 after this review, plus the nine agreement payments reviews (ADR 0136),
  // less the reviews retired with their deleted source lists; the delivery
  // protocol review (SYS-VERIFY-002) is counted separately.
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});

test('dated-storage-parent: storage-parent: marketplace agreement payments append one migration to the protected 252-entry manifest', async () => {
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const main = await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, manifestPath);
  const combined = await manifestBeforePlayer();
  const migration = { id: MARKETPLACE_AGREEMENT_PAYMENTS_ENTRY,
    path: 'tdf-hq/sql/2026-11-12_marketplace_agreement_payments.sql',
    introducedBy: AGREEMENT_PAYMENTS_INTRODUCTION };
  assert.equal(main.migrations.length, 252);
  assert.deepEqual(combined, { ...main, migrations: [...main.migrations, migration] });
  const { stdout } = await execFileAsync('git', ['show', `${AGREEMENT_PAYMENTS_INTRODUCTION}:${migration.path}`],
    { cwd: workspaceRoot });
  assert.equal(await fs.readFile(path.join(workspaceRoot, migration.path), 'utf8'), stdout);
  await execFileAsync('git', ['merge-base', '--is-ancestor', AGREEMENT_PAYMENTS_INTRODUCTION, 'HEAD'],
    { cwd: workspaceRoot });
  const { active, retired, successors, integration } = await readReconciliation(true);
  const priorMain = (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN, decisionsPath)).decisions;
  const pointer = successors.find(row => row.name === 'migrations');
  const current = active.find(row => row.id === pointer.currentId);
  assert.equal(current.evidence.valueCount, 253);
  assert.equal(current.reviewed, true);
  assert.equal(current.reviewMethod, 'successor-of-reviewed-decisions');
  // The complete review of protected main is nested unchanged, and so are the
  // pointer and the retirement that named it.
  assert.deepEqual(current.previousDecision, priorMain.find(row => row.evidence?.file === manifestPath));
  assert.equal(current.previousDecision.evidence.valueCount, 252);
  assert.equal(pointer.previousCurrentId, current.previousDecision.id);
  assert.equal(pointer.change, current.justification);
  assert.deepEqual(pointer.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors
    .find(row => row.name === 'migrations'));
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, current.id);
  assert.equal(retirement.previousReplacementId, current.previousDecision.id);
  assert.deepEqual(retirement.previousResolution, (await reviewedJson(AGREEMENT_PAYMENTS_PROTECTED_MAIN,
    'docs/catalog-persistence/event-integration-retirements.json')).retirements
    .find(row => row.source.name === 'migrations'));
  // Every other review of protected main is retained exactly; nine lists are
  // new (payment provider, fee payer, payout and payment CHECK lists and the
  // two Policy sum types) and reviewed by the deterministic policy.
  // Moved lists (ADR 0133) nest the exact review of protected main.
  const history = new Map([...active, ...retired].map(row => [row.id, row]));
  const replaced = new Set([current.previousDecision.id, ...assertMovedListsNest(active, priorMain)]);
  // The 86-method client review was since superseded by EVT-TICKET-SALE-001
  // and is retained exactly as the previousDecision of its successor.
  const clientSuccessor = active.find(row => row.previousDecision?.id === '083bd1a66e9f23f02866');
  for (const row of priorMain) {
    if (replaced.has(row.id)) assert.ok(!history.has(row.id));
    else if (row.id === '083bd1a66e9f23f02866') assert.deepEqual(clientSuccessor.previousDecision, row);
    else assert.deepEqual(history.get(row.id), row);
  }
  const payoutReviews = ['d580ad14a8f01218c4a9', 'fbe5e4e4d0289e1caaf4', 'cfa5a05214e95ed07ef0',
    '203395cfa370d78f86a1', '1f6946049f264576684d'];
  const chargeReviews = ['09d7dc6ebc55642a4bb2', '584b9609b82ad71578ed', 'e1a25b160e801cf866df',
    '32574b309f1d005ed165'];
  for (const id of [...payoutReviews, ...chargeReviews]) {
    assert.ok(!priorMain.some(row => row.id === id));
    assert.ok(history.get(id)?.reviewed, `the marketplace agreement payments review ${id} must be present and reviewed`);
    assert.match(history.get(id).reviewMethod, /^deterministic-policy-v\d+$/);
  }
  assert.equal(priorMain.length, 1452);
  assert.equal(active.filter(row => row.id !== DELIVERY_PROTOCOL_REVIEW).length, 1461 - retired.length);
});


test('dated-storage-parent: storage-parent: player server preserves both reviewed migration histories and appends only reviewed SQL', async () => {
  const main = await reviewedJson(PLAYER_SERVER_MAIN_BASE, manifestPath);
  const player = await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, manifestPath);
  const current = (await mainManifestView());
  const extension = player.migrations.filter(row => playerMigrations.has(row.id));
  assert.deepEqual(extension.map(row => row.id), [...playerMigrations]);
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...extension] });
  for (const row of extension) {
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'],
      { cwd: workspaceRoot });
    const { stdout } = await execFileAsync('git', ['show', `${row.introducedBy}:${row.path}`],
      { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
  }

  const { active, successors, integration } = await readReconciliation();
  const pointer = successors.find(row => row.name === 'migrations');
  const actual = active.find(row => row.id === pointer.currentId);
  const findTip = doc => doc.decisions.find(row => row.evidence?.file === manifestPath);
  const mainReview = findTip(await reviewedJson(PLAYER_SERVER_MAIN_BASE, catalogPath));
  const playerReview = findTip(await reviewedJson(PLAYER_SERVER_REVIEW_HEAD, catalogPath));
  assert.deepEqual(actual.previousDecision, mainReview);
  assert.deepEqual(actual.concurrentReviewHistory, [playerReview]);
  assert.equal(actual.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  const retirement = integration.retirements.find(row => row.source.name === 'migrations');
  assert.equal(retirement.replacementId, actual.id);
  assert.equal(retirement.previousResolution.replacementId, mainReview.id);
});

test('dated-storage-parent: storage-parent: loop rework retires the reviews of two deleted scripts as provenance and reviews one new list', async () => {
  const { active } = await readReconciliation();
  const rework = await readLoopRework();
  assert.equal(rework.decisions.length, 2);
  assert.ok(rework.reason.length > 80);
  for (const entry of rework.decisions) {
    assert.equal(entry.decision.reviewed, true);
    assert.ok(!active.some(row => row.id === entry.decision.id));
    await assert.rejects(fs.access(path.join(workspaceRoot, entry.source.file)), { code: 'ENOENT' });
  }
  assert.deepEqual(rework.addedDecisionIds, ['16e3988d48089cc77b7e']);
  for (const id of rework.addedDecisionIds) {
    assert.equal(active.find(row => row.id === id)?.reviewed, true);
  }
  // The earlier view re-inserts the retired reviews and drops the new one; the catalog
  // itself keeps growing with main, so only the difference is fixed.
  const historical = (await readHistoricalReconciliation()).active;
  assert.equal(historical.length, active.length + rework.decisions.length - rework.addedDecisionIds.length);
  for (const entry of rework.decisions) assert.deepEqual(historical[entry.position], entry.decision);
});

// One explicit review classifies the delivery protocol; it is new, unique and
// carries its requirement. The rest of the catalog follows main.
test('dated-storage-parent: storage-parent: delivery evidence protocol adds exactly one reviewed list', async () => {
  const current = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', catalogPath, 'utf8'));
  const baseline = await reviewedJson('5084d8a8a4d7a7a3b8d30d1f55ae5d9c8c032b9b', catalogPath);
  assert.ok(!baseline.decisions.some(row => row.id === DELIVERY_PROTOCOL_REVIEW));
  const additions = current.decisions.filter(row => row.id === DELIVERY_PROTOCOL_REVIEW);
  assert.equal(additions.length, 1);
  assert.equal(additions[0].classification, 'genuine-technical-constant');
  assert.equal(additions[0].disposition, 'explicit-allowlist');
  assert.equal(additions[0].reviewed, true);
  assert.equal(additions[0].evidence.file, 'scripts/lib/delivery-matrix.mjs');
  assert.equal(additions[0].evidence.name, 'DELIVERY_CHECKS');
  assert.match(additions[0].justification, /SYS-VERIFY-002/);
  assert.equal(current.decisions.filter(row => row.evidence?.file === 'scripts/lib/delivery-matrix.mjs').length, 1);
});

// These parent tests describe a dated schema. Only the two/one explicitly
// appended migration rows are projected away; combined-source checks below
// validate the actual unprojected registry and complete parent histories.
const INTEGRATION_MAIN = '966fb55dca70a49c0a340b674f8cc133cc413a2d';
const INTEGRATION_SOURCE = '880a90b02575464f842a6b4d33789c47baa22e5e';
const INTEGRATION_EXTRA_IDS = new Set(["2026-11-06_content_storage_security", "2026-11-07_content_storage_identity_guards"]);
const INTEGRATION_MAIN_REVIEW = '912cda11c7b6eb622f76';
async function actualManifest() {
  return JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'scripts/production-migrations.json', 'utf8'));
}
async function mainManifestView() {
  const actual = await actualManifest();
  return { ...actual, migrations: actual.migrations.filter(row => !INTEGRATION_EXTRA_IDS.has(row.id)) };
}
async function sourceManifestView() {
  const original = (await reviewedJson(INTEGRATION_SOURCE, 'scripts/production-migrations.json')).migrations;
  const current = (await actualManifest()).migrations;
  return { migrations: original.map(row => {
    const actual = current.find(item => item.id === row.id);
    assert.deepEqual(actual, row, `Source manifest entry changed: ${row.id}`);
    return actual;
  }) };
}
const reviewIndexes = new WeakMap();
function wholeReview(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  let index = reviewIndexes.get(value);
  if (!index) {
    index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      if (node.id) index.set(node.id, [...(index.get(node.id) ?? []), node]);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); reviewIndexes.set(value, index);
  }
  return (index.get(expected.id) ?? []).find(row => canonicalReview(row) === canonicalReview(expected));
}

async function sourceReconciliation() {
  const register = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', catalogPath, 'utf8'));
  const original = await reviewedJson(INTEGRATION_SOURCE, catalogPath);
  const active = original.decisions.map(row => {
    const found = wholeReview(register, row);
    assert.ok(found, `Complete original source review missing: ${row.id}`);
    return found;
  });
  return { active, successors: (await reviewedJson(INTEGRATION_SOURCE,
    'docs/catalog-persistence/event-catalog-current-successors.json')).successors };
}

test('dated-storage-parent: storage-parent: storage security appends one migration without changing the integrated manifest or its review', async () => {
  const ledger = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/storage-security-successor-20261106.json', 'utf8'));
  const manifest = (await sourceManifestView()).migrations.filter(entry => !['2026-10-20_video_campaigns', '2026-11-06_video_campaign_identity_references', '2026-11-07_content_storage_identity_guards'].includes(entry.id));
  const { active } = await sourceReconciliation();
  const current = findHistoricalReview(active.find(entry => entry.evidence?.file === 'scripts/production-migrations.json'), ledger.currentDecisionId);
  const canonicalHash = value => createHash('sha256').update(JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item) ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item)).digest('hex');
  assert.equal(manifest.length, 245);
  assert.equal(canonicalHash(manifest.slice(0, 244)), ledger.previousManifestCanonicalSha256);
  assert.equal(canonicalHash(manifest), ledger.currentManifestCanonicalSha256);
  assert.equal(canonicalHash(current.previousDecision), ledger.previousDecisionCanonicalSha256);
  assert.equal(current.previousDecision.id, ledger.previousDecisionId);
  assert.deepEqual(manifest[244], ledger.addedMigration);
  assert.equal(ledger.addedMigration.id, '2026-11-06_content_storage_security');
  assert.equal(ledger.executionAllowed, false);
});

test('dated-storage-parent: storage-parent: storage current-main integration retains exact protected prefix and both complete parent migration reviews', async () => {
  const main = 'f615c862b1a7d50406cc0506d21d7650e607fa5f';
  const original = '376155380e0e462f9985572bd0420dff459de318';
  const manifestPath = 'scripts/production-migrations.json';
  const decisionsPath = 'docs/catalog-persistence/catalog-list-decisions.json';
  const old = JSON.parse((await execFileAsync('git', ['show', `${main}:${manifestPath}`], { cwd: workspaceRoot })).stdout).migrations;
  const source = JSON.parse((await execFileAsync('git', ['show', `${original}:${manifestPath}`], { cwd: workspaceRoot })).stdout).migrations;
  const current = (await sourceManifestView()).migrations;
  assert.equal(old.length, 246); assert.equal(current.length, 248);
  assert.deepEqual(current.slice(0,246), old);
  assert.deepEqual(current[246], source.find(entry => entry.id === '2026-11-06_content_storage_security'));
  assert.deepEqual(current[247], { id: '2026-11-07_content_storage_identity_guards', path: 'tdf-hq/sql/2026-11-07_content_storage_identity_guards.sql', introducedBy: 'a8b31cefcf5c6ce45a3fc38406f23f11a3616b56' });
  const { active } = await sourceReconciliation();
  const latest = active.find(entry => entry.evidence?.file === manifestPath);
  for (const parent of [main, original]) {
    const previous = JSON.parse((await execFileAsync('git', ['show', `${parent}:${decisionsPath}`], { cwd: workspaceRoot, maxBuffer: 16*1024*1024 })).stdout).decisions.find(entry => entry.evidence?.file === manifestPath);
    assert.deepEqual(findHistoricalReview(latest,previous.id), previous);
  }
  for (const entry of current.slice(246)) {
    const originalSql = (await execFileAsync('git', ['show', `${entry.introducedBy}:${entry.path}`], { cwd: workspaceRoot })).stdout;
    assert.equal(await fs.readFile(path.join(workspaceRoot,entry.path),'utf8'), originalSql);
    await execFileAsync('git',['merge-base','--is-ancestor',entry.introducedBy,'HEAD'],{cwd:workspaceRoot});
  }
});


test('dated-storage-parent: storage-parent: combined registry retains the exact main prefix, original SQL and whole parent reviews', async () => {
  const current = await actualManifest();
  const main = await reviewedJson(INTEGRATION_MAIN, manifestPath);
  const original = await reviewedJson(INTEGRATION_SOURCE, manifestPath);
  const suffix = original.migrations.filter(row => INTEGRATION_EXTRA_IDS.has(row.id));
  assert.deepEqual(current, { ...main, migrations: [...main.migrations, ...suffix] });
  assert.equal(new Set(current.migrations.map(row => row.id)).size, current.migrations.length);
  for (const row of suffix) {
    const { stdout } = await execFileAsync('git', ['show', `${INTEGRATION_SOURCE}:${row.path}`], { cwd: workspaceRoot });
    assert.equal(await fs.readFile(path.join(workspaceRoot, row.path), 'utf8'), stdout);
    await execFileAsync('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: workspaceRoot });
  }
  const register = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', catalogPath, 'utf8'));
  const pointerFile = 'docs/catalog-persistence/event-catalog-current-successors.json';
  const retireFile = 'docs/catalog-persistence/event-integration-retirements.json';
  const pointers = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', pointerFile, 'utf8'));
  const retirements = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', retireFile, 'utf8'));
  const pointer = pointers.successors.find(row => row.name === 'migrations');
  const tip = register.decisions.find(row => row.id === pointer.currentId);
  const retirement = retirements.retirements.find(row => row.source.name === 'migrations');
  assert.equal(tip.evidence.valueCount, current.migrations.length);
  assert.equal(pointer.valueCount, current.migrations.length);
  assert.equal(retirement.replacementId, tip.id);
  assert.deepEqual(tip.previousDecision, (await reviewedJson(INTEGRATION_MAIN, catalogPath))
    .decisions.find(row => row.evidence?.file === manifestPath));
  const canonical = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
    ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
  for (const parent of [INTEGRATION_MAIN, INTEGRATION_SOURCE]) {
    const old = await reviewedJson(parent, catalogPath);
    for (const row of old.decisions) {
      const found = wholeReview(register, row);
      assert.ok(found, `Lost complete parent review ${parent}:${row.id}`);
      assert.equal(canonical(found), canonical(row));
    }
    for (const [file, actual, key] of [[pointerFile, pointers, 'successors'], [retireFile, retirements, 'retirements']]) {
      const rows = (await reviewedJson(parent, file))[key];
      for (const row of rows) assert.ok(wholeResolution(actual, row), `Lost complete parent resolution ${parent}:${file}`);
    }
  }
});
function wholeResolution(value, expected) {
  return Boolean(findWholeResolution(value, expected));
}

const canonicalReview = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findWholeResolution(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if ((Object.hasOwn(expected, 'currentId') && value.currentId === expected.currentId || Object.hasOwn(expected, 'replacementId') && value.replacementId === expected.replacementId) && canonicalReview(value) === canonicalReview(expected)) return value;
  for (const child of Object.values(value)) {
    const found = findWholeResolution(child, expected);
    if (found) return found;
  }
}
async function mainSourceBytes(file) {
  if (file === 'tdf-mobile/src/features/generatedFeatureRegistry.ts') {
    const { stdout } = await execFileAsync('git', ['show', '4d866387ac6938d7aeaad5d1fa2985f8e02932e7:src/features/generatedFeatureRegistry.ts'], { cwd: path.join(workspaceRoot, 'tdf-mobile'), maxBuffer: 16 * 1024 * 1024 });
    return stdout; // exact dated fixture; the actual mirror has its own successor check below
  }
  return file === manifestPath ? JSON.stringify(await mainManifestView())
    : integrationSource('cdf57961e4058f403ee4dd903bacfced2a5ee258', file);
}

function findHistoricalReview(value, id) {
  if (!value) return undefined;
  if (value.id === id) return value;
  const previous = findHistoricalReview(value.previousDecision, id);
  if (previous) return previous;
  for (const branch of value.concurrentReviewHistory ?? []) {
    const found = findHistoricalReview(branch, id);
    if (found) return found;
  }
}


test('dated-storage-parent: storage-parent: actual Mobile mirror matches its generator and stale predecessor cannot authorize the new list', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-mobile-mirror-'));
  const mobileFile = 'tdf-mobile/src/features/generatedFeatureRegistry.ts';
  const generatedFile = 'src/features/generatedFeatureRegistry.ts';
  const register = JSON.parse(await readPreservedParentFile('cdf57961e4058f403ee4dd903bacfced2a5ee258', catalogPath, 'utf8'));
  const current = register.decisions.find(row => row.id === 'ea29c6e8a6cd165802a8');
  assert.equal(current?.evidence.valueCount, 165);
  assert.equal(current.previousDecision.id, 'fd606f7d00f82c78cb20');
  try {
    const output = path.join(temporary, 'generated');
    await fs.mkdir(path.join(output, 'src/features'), { recursive: true });
    await generatePreservedParentMirror('cdf57961e4058f403ee4dd903bacfced2a5ee258', output);
    const source = await integrationSource('cdf57961e4058f403ee4dd903bacfced2a5ee258', mobileFile);
    assert.equal(source, await fs.readFile(path.join(output, generatedFile), 'utf8'));
    const repo = path.join(temporary, 'repo');
    await fs.mkdir(repo);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repo });
    await writeFile(repo, mobileFile, source);
    await execFileAsync('git', ['add', '--', mobileFile], { cwd: repo });
    const report = JSON.parse(await integrationSource('cdf57961e4058f403ee4dd903bacfced2a5ee258', 'docs/catalog-persistence/reports/static-list-inventory.json'));
    const ids = new Set(report.candidates.filter(row => row.file === mobileFile).map(row => row.id));
    const selected = register.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size);
    assert.ok(ids.has(current.id));
    const decisions = path.join(temporary, 'decisions.json');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [auditScriptPath, '--root', repo, '--decisions', decisions, '--fail-on-unreviewed'];
    await execFileAsync(process.execPath, args, { cwd: repo });
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row.id === current.id ? current.previousDecision : row)));
    await assert.rejects(execFileAsync(process.execPath, args, { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});
}

const composedRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const composedExec = promisify(execFile);
const composedCache = new Map();
const preservedIndexes = new WeakMap();
const composedCurrentFiles = new Map();
const preservedFiles = new Map([
  ['docs/catalog-persistence/catalog-list-decisions.json', 'decisions'],
  ['docs/catalog-persistence/event-catalog-current-successors.json', 'successors'],
  ['docs/catalog-persistence/event-integration-retirements.json', 'retirements'],
]);
const canonicalComposed = value => JSON.stringify(value, (_key, item) => item && typeof item === 'object' && !Array.isArray(item)
  ? Object.fromEntries(Object.keys(item).sort().map(key => [key, item[key]])) : item);
function findPreserved(value, expected) {
  if (!value || typeof value !== 'object') return undefined;
  if (!preservedIndexes.has(value)) {
    const index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      index.set(canonicalComposed(node), node);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value); preservedIndexes.set(value, index);
  }
  return preservedIndexes.get(value).get(canonicalComposed(expected));
}
async function composedParent(ref, file) {
  const key = `${ref}:${file}`;
  if (!composedCache.has(key)) composedCache.set(key, composedExec('git', ['show', key],
    { cwd: composedRoot, maxBuffer: 64 * 1024 * 1024 }).then(({ stdout }) => JSON.parse(stdout)));
  return composedCache.get(key);
}
async function readPreservedParentFile(ref, file, encoding) {
  const actualBytes = await integrationRead('d5cb26bfd0ace6f606f827d81bfc4180acaebcf1', file, encoding);
  if (file === 'scripts/production-migrations.json' || preservedFiles.has(file)) {
    if (!composedCurrentFiles.has(file)) composedCurrentFiles.set(file, { bytes: actualBytes, value: JSON.parse(actualBytes) });
    assert.equal(composedCurrentFiles.get(file).bytes, actualBytes, `Composed source changed during parent assertions: ${file}`);
  }
  if (file === 'scripts/production-migrations.json') {
    const actual = composedCurrentFiles.get(file).value; const parent = await composedParent(ref, file);
    return JSON.stringify({ ...parent, migrations: parent.migrations.map(row => {
      const found = actual.migrations.find(item => item.id === row.id);
      assert.deepEqual(found, row, `Dated migration changed: ${ref}:${row.id}`); return found;
    }) });
  }
  const key = preservedFiles.get(file);
  if (!key) return actualBytes;
  const actual = composedCurrentFiles.get(file).value; const parent = await composedParent(ref, file);
  return JSON.stringify({ ...parent, [key]: parent[key].map(row => {
    const found = findPreserved(actual, row);
    assert.ok(found, `Lost whole parent review/resolution: ${ref}:${file}:${row.id ?? row.name}`);
    return found;
  }) });
}

const COMPOSED_PARENTS = ["d65017d10f16e8089c43b82bdbcacf3177d206f5", "cdf57961e4058f403ee4dd903bacfced2a5ee258"];

test('dated-storage-parent: actual storage/refund composition retains all 261 migrations, immutable SQL and both complete review graphs', async () => {
  const file = 'scripts/production-migrations.json';
  const actual = JSON.parse(await integrationRead('d5cb26bfd0ace6f606f827d81bfc4180acaebcf1', file, 'utf8'));
  const [main, storage] = await Promise.all(COMPOSED_PARENTS.map(ref => composedParent(ref, file)));
  const ids = new Set(main.migrations.map(row => row.id));
  const extra = storage.migrations.filter(row => !ids.has(row.id));
  assert.deepEqual(extra.map(row => row.id), ['2026-11-06_content_storage_security', '2026-11-07_content_storage_identity_guards']);
  assert.deepEqual(actual, { ...main, migrations: [...main.migrations, ...extra] });
  assert.equal(actual.migrations.length, 261); assert.equal(new Set(actual.migrations.map(row => row.id)).size, 261);
  for (const row of actual.migrations) {
    const ref = ids.has(row.id) ? COMPOSED_PARENTS[0] : COMPOSED_PARENTS[1];
    const { stdout } = await composedExec('git', ['show', `${ref}:${row.path}`], { cwd: composedRoot, maxBuffer: 32 * 1024 * 1024 });
    assert.equal(await fs.readFile(path.join(composedRoot, row.path), 'utf8'), stdout, `Immutable SQL changed: ${row.id}`);
    await composedExec('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: composedRoot });
  }
  for (const [reviewFile, key] of preservedFiles) {
    const current = JSON.parse(await integrationRead('d5cb26bfd0ace6f606f827d81bfc4180acaebcf1', reviewFile, 'utf8'));
    for (const ref of COMPOSED_PARENTS) {
      const prior = await composedParent(ref, reviewFile);
      for (const row of prior[key]) assert.ok(findPreserved(current, row), `Lost whole parent object: ${ref}:${reviewFile}`);
      for (const [field, value] of Object.entries(prior)) {
        if (field === key) continue;
        if (value && typeof value === 'object') assert.ok(findPreserved(current, value), `Lost whole parent metadata: ${ref}:${reviewFile}:${field}`);
        else assert.equal(current[field], value, `Lost parent metadata: ${ref}:${reviewFile}:${field}`);
      }
    }
  }
});
}

const integrationRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const integrationExec = promisify(execFile);
const integrationCache = new Map();
const integrationIndexes = new WeakMap();
const integrationCurrentFiles = new Map();
const integrationFiles = new Map([
  ['docs/catalog-persistence/catalog-list-decisions.json', 'decisions'],
  ['docs/catalog-persistence/event-catalog-current-successors.json', 'successors'],
  ['docs/catalog-persistence/event-integration-retirements.json', 'retirements'],
]);
// Index small object shapes so each lookup compares only plausible matches.
// Serializing each entire subtree here duplicates the expanded review history.
function integrationLookupKey(node) {
  if (Array.isArray(node)) return `array/${node.length}`;
  return JSON.stringify([Object.keys(node).sort(), node.id ?? null, node.name ?? null]);
}

function integrationFind(value, expected) {
  if (!value || typeof value !== 'object' || !expected || typeof expected !== 'object') return undefined;
  if (!integrationIndexes.has(value)) {
    const index = new Map();
    const visit = node => {
      if (!node || typeof node !== 'object') return;
      const key = integrationLookupKey(node);
      const bucket = index.get(key);
      if (bucket) bucket.push(node);
      else index.set(key, [node]);
      for (const child of Object.values(node)) visit(child);
    };
    visit(value);
    integrationIndexes.set(value, index);
  }
  const candidates = integrationIndexes.get(value).get(integrationLookupKey(expected)) ?? [];
  return candidates.find(node => isDeepStrictEqual(node, expected));
}
async function integrationParent(ref, file) {
  const key = `${ref}:${file}`;
  if (!integrationCache.has(key)) integrationCache.set(key, integrationExec('git', ['show', key],
    { cwd: integrationRoot, maxBuffer: 64 * 1024 * 1024 }).then(({ stdout }) => JSON.parse(stdout)));
  return integrationCache.get(key);
}
// The decisions file stores its review history by reference
// (scripts/lib/catalog-decision-history.mjs); read it fully expanded, exactly
// as the nested document it represents.
let expandedDecisionsText;
async function integrationCurrentText(file, encoding) {
  if (file !== DECISIONS_FILE) return fs.readFile(path.join(integrationRoot, file), encoding);
  expandedDecisionsText ??= `${JSON.stringify(readExpandedDecisions(integrationRoot), null, 2)}\n`;
  return expandedDecisionsText;
}
async function integrationRead(ref, file, encoding) {
  const actualBytes = await integrationCurrentText(file, encoding);
  if (file === 'scripts/production-migrations.json' || integrationFiles.has(file)) {
    if (!integrationCurrentFiles.has(file)) integrationCurrentFiles.set(file, { bytes: actualBytes, value: JSON.parse(actualBytes) });
    assert.equal(integrationCurrentFiles.get(file).bytes, actualBytes, `Composed source changed during parent assertions: ${file}`);
  }
  if (file === 'scripts/production-migrations.json') {
    const actual = integrationCurrentFiles.get(file).value; const parent = await integrationParent(ref, file);
    return JSON.stringify({ ...parent, migrations: parent.migrations.map(row => {
      const found = actual.migrations.find(item => item.id === row.id);
      assert.deepEqual(found, row, `Dated migration changed: ${ref}:${row.id}`); return found;
    }) });
  }
  const key = integrationFiles.get(file);
  if (!key) return actualBytes;
  const actual = integrationCurrentFiles.get(file).value; const parent = await integrationParent(ref, file);
  return JSON.stringify({ ...parent, [key]: parent[key].map(row => {
    const found = integrationFind(actual, row);
    assert.ok(found, `Lost whole parent review/resolution: ${ref}:${file}:${row.id ?? row.name}`);
    return found;
  }) });
}


async function integrationSource(ref, file) {
  // Original dated source for negative fixtures; never an active decision.
  if (file.startsWith('tdf-mobile/')) {
    const { stdout } = await integrationExec('git', ['ls-tree', ref, 'tdf-mobile'], { cwd: integrationRoot });
    const pin = stdout.match(/^160000 commit ([0-9a-f]{40})\t/); assert.ok(pin);
    return (await integrationExec('git', ['show', `${pin[1]}:${file.slice('tdf-mobile/'.length)}`],
      { cwd: path.join(integrationRoot, 'tdf-mobile'), maxBuffer: 64 * 1024 * 1024 })).stdout;
  }
  return (await integrationExec('git', ['show', `${ref}:${file}`],
    { cwd: integrationRoot, maxBuffer: 64 * 1024 * 1024 })).stdout;
}

// Retain both original published parents and additionally guard latest published main.
const INTEGRATION_PARENTS = ["b99ce783caa8446920c5950f5111865e7f05cfa3", "d5cb26bfd0ace6f606f827d81bfc4180acaebcf1", "5a81672285dc9c03130fc9fd8355e2b4c58e9e12"];

// Qualify the new append against both published parents before running their historical assertions.
async function manifestBeforeVenueCoordinates(file) {
  const current = JSON.parse(await fs.readFile(path.join(integrationRoot, file), 'utf8'));
  const entry = current.migrations.at(-1);
  const original = (await integrationParent('218797e0c3c8d4c5af839f10c42b3549d4f7f7c5', file)).migrations.at(-1);
  assert.equal(entry.id, '2026-11-24_venue_coordinate_constraint');
  assert.deepEqual(entry, original);
  return { ...current, migrations: current.migrations.slice(0, -1) };
}

test('venue coordinates append the original SQL after the protected migration sequence', async () => {
  const file = 'scripts/production-migrations.json';
  const parent = await integrationParent('bddc0a2a6c18ef101db3c7750a4ec7fe7f01b4d8', file);
  assert.deepEqual(await manifestBeforeVenueCoordinates(file), parent);
  const { migrations } = await integrationParent('218797e0c3c8d4c5af839f10c42b3549d4f7f7c5', file);
  const entry = migrations.at(-1);
  await integrationExec('git', ['merge-base', '--is-ancestor', entry.introducedBy, 'HEAD'], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, entry.path), 'utf8'), await integrationSource(entry.introducedBy, entry.path));
});

async function manifestBeforeTaxonomy(file) {
  const current = await manifestBeforeVenueCoordinates(file);
  const original = (await integrationParent('be996410572a72f8f787b2cd5d345b30523ba1ea', file)).migrations.at(-1);
  assert.equal(original.id, '2026-11-21_location_genre_filters');
  assert.deepEqual(current.migrations.at(-3), original);
  const additions = current.migrations.slice(-2);
  assert.deepEqual(additions.map(entry => entry.id), ['2026-11-24_unicode_genre_suggestion_keys', '2026-11-24_preserve_selected_genre_ids']);
  for (const entry of additions) {
    assert.equal(entry.path, `tdf-hq/sql/${entry.id}.sql`);
    assert.equal(await fs.readFile(path.join(integrationRoot, entry.path), 'utf8'), await integrationSource(entry.introducedBy, entry.path));
    await integrationExec('git', ['merge-base', '--is-ancestor', entry.introducedBy, 'HEAD'], { cwd: integrationRoot });
  }
  return { ...current, migrations: current.migrations.slice(0, -3) };
}

test('taxonomy appends its original migration to the full loudness manifest', async () => {
  const file = 'scripts/production-migrations.json';
  assert.deepEqual(await manifestBeforeTaxonomy(file), await integrationParent('228ab63e448b93970aa0bb32af6320c4cfb88e57', file));
  const { migrations } = await integrationParent('be996410572a72f8f787b2cd5d345b30523ba1ea', file);
  const entry = migrations.at(-1);
  await integrationExec('git', ['merge-base', '--is-ancestor', entry.introducedBy, 'HEAD'], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, entry.path), 'utf8'), await integrationSource(entry.introducedBy, entry.path));
});

async function manifestBeforeLoudness(file) {
  const current = await manifestBeforeTaxonomy(file);
  const original = (await integrationParent('f008ebb18d9aa7c2e892301ddf435d0da6020e2f', file)).migrations.at(-1);
  assert.equal(original.id, '2026-11-21_music_audio_measurement');
  assert.deepEqual(current.migrations.at(-1), original);
  return { ...current, migrations: current.migrations.slice(0, -1) };
}

function decisionBeforeLoudness(decisions) {
  const coordinates = decisions.find(row => row.evidence?.file === 'scripts/production-migrations.json' && row.evidence?.name === 'migrations');
  assert.equal(coordinates.reviewBatch, 'venue-coordinate-constraint-main-integration-20261124');
  assert.equal(coordinates.evidence.valueCount, coordinates.previousDecision.evidence.valueCount + 1);
  assert.equal(coordinates.concurrentReviewHistory[0].reviewBatch, 'venue-coordinate-constraint-20261124');
  const taxonomy = coordinates.previousDecision;
  assert.equal(taxonomy.reviewBatch, 'taxonomy-filters-main-integration-20261124');
  assert.equal(taxonomy.evidence.valueCount, taxonomy.previousDecision.evidence.valueCount + 3);
  assert.equal(taxonomy.concurrentReviewHistory[0].evidence.file, 'scripts/production-migrations.json');
  const tip = taxonomy.previousDecision;
  assert.equal(tip.reviewBatch, 'music-loudness-main-integration-20261124');
  assert.equal(tip.evidence.valueCount, tip.previousDecision.evidence.valueCount + 1);
  assert.equal(tip.concurrentReviewHistory[0].reviewBatch, 'music-loudness-check-20261121');
  return tip.previousDecision;
}

test('music loudness appends the original SQL migration to the complete main manifest', async () => {
  const file = 'scripts/production-migrations.json';
  const parent = await integrationParent('39935739e2def3817eaa2b84229cc2cc4b4941f1', file);
  assert.deepEqual(await manifestBeforeLoudness(file), parent);
  const { migrations } = await manifestBeforeTaxonomy(file);
  const { introducedBy, path: sql } = migrations.at(-1);
  await integrationExec('git', ['merge-base', '--is-ancestor', introducedBy, 'HEAD'], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, sql), 'utf8'), await integrationSource(introducedBy, sql));
});

// EVT-VENUE-SEARCH-001 appends one migration after the directory semantic search entry.
// Earlier reviews are verified over the manifest without that entry.
const VENUE_SEARCH_ENTRY = '2026-11-21_venue_search_sql';
const withoutVenueSearch = manifest => (
  { ...manifest, migrations: manifest.migrations.filter(row => row.id !== VENUE_SEARCH_ENTRY) });

// COMMUNITY-ARTIST-SIMILARITY-001 (2026-11-21) appends one reviewed migration
// after the venue search entry; it has its own test below and nests the venue
// search review unchanged. Earlier reviews are verified without that entry.
const ARTIST_SIMILARITY_ENTRY = '2026-11-21_artist_similarity';
const ARTIST_SIMILARITY_BATCH = 'artist-similarity-2026-11-21';
const withoutArtistSimilarity = manifest => (
  { ...manifest, migrations: manifest.migrations.filter(row => row.id !== ARTIST_SIMILARITY_ENTRY) });
const beforeArtistSimilarity = decision => (
  decision.reviewBatch === ARTIST_SIMILARITY_BATCH ? decision.previousDecision : decision);

test('venue search appends its migration last, unchanged since its introduction, with the previous review nested', async () => {
  const file = 'scripts/production-migrations.json';
  const manifest = withoutArtistSimilarity(await manifestBeforeLoudness(file));
  const entry = manifest.migrations.at(-1);
  assert.equal(entry.id, VENUE_SEARCH_ENTRY);
  assert.equal(manifest.migrations.filter(row => row.id === VENUE_SEARCH_ENTRY).length, 1);
  await integrationExec('git', ['merge-base', '--is-ancestor', entry.introducedBy, 'HEAD'], { cwd: integrationRoot });
  const { stdout } = await integrationExec('git', ['show', entry.introducedBy + ':' + entry.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, entry.path), 'utf8'), stdout);
  const decisions = readExpandedDecisions(integrationRoot).decisions;
  const tip = beforeArtistSimilarity(decisionBeforeLoudness(decisions));
  assert.equal(tip.reviewBatch, 'venue-search-sql-20261121');
  assert.equal(tip.evidence.valueCount, manifest.migrations.length);
  assert.equal(tip.previousDecision.evidence.valueCount, manifest.migrations.length - 1);
  assert.equal(tip.classification, tip.previousDecision.classification);
  assert.equal(tip.disposition, tip.previousDecision.disposition);
});

// Course follow-up reminders append one migration after the studio gallery.
// Verify it is the original, unchanged entry, then compare the rest as before.
const COURSE_REMINDER_ENTRY = '2026-11-20_course_follow_up_reminder_delivery';
async function withoutCourseReminder(manifest) {
  const appended = manifest.migrations.at(-1);
  if (appended?.id !== COURSE_REMINDER_ENTRY) return manifest;
  await integrationExec('git', ['merge-base', '--is-ancestor', appended.introducedBy, 'HEAD'], { cwd: integrationRoot });
  const { stdout } = await integrationExec('git', ['show', appended.introducedBy + ':' + appended.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, appended.path), 'utf8'), stdout);
  return { ...manifest, migrations: manifest.migrations.slice(0, -1) };
}

// MSG-DELIVERY-001 (2026-11-20) appends one reviewed migration after the
// course reminder; strip it explicitly so the compositions below stay exact.
async function withoutChatDelivery(manifest) {
  const appended = manifest.migrations.at(-1);
  if (appended?.id !== '2026-11-20_chat_external_delivery') return manifest;
  await integrationExec('git', ['merge-base', '--is-ancestor', appended.introducedBy, 'HEAD'], { cwd: integrationRoot });
  const { stdout } = await integrationExec('git', ['show', appended.introducedBy + ':' + appended.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, appended.path), 'utf8'), stdout);
  return { ...manifest, migrations: manifest.migrations.slice(0, -1) };
}

test('actual UX/storage composition preserves current main, immutable SQL and both complete review graphs', async () => {
  const file = 'scripts/production-migrations.json';
  const raw = withoutVenueSearch(withoutArtistSimilarity(await manifestBeforeLoudness(file)));
  // The directory semantic search migration (DIR-SEMANTIC-SEARCH-001) is the
  // last reviewed append; it has its own test below.
  assert.equal(raw.migrations.at(-1).id, '2026-11-21_directory_semantic_search');
  const combined = await withoutCourseReminder(await withoutChatDelivery({ ...raw, migrations: raw.migrations.slice(0, -1) }));
  assert.equal(combined.migrations.at(-1).id, '2026-11-07_studio_gallery');
  const actual = { ...combined, migrations: combined.migrations.slice(0, -1) };
  const [ux, main] = await Promise.all(INTEGRATION_PARENTS.map(ref => integrationParent(ref, file)));
  assert.deepEqual(actual, main);
  assert.deepEqual(actual.migrations.slice(0, ux.migrations.length), ux.migrations);
  assert.equal(actual.migrations.length, 261);
  assert.equal(new Set(actual.migrations.map(row => row.id)).size, 261);
  for (const row of actual.migrations) {
    assert.equal(await fs.readFile(path.join(integrationRoot, row.path), 'utf8'),
      await integrationSource(INTEGRATION_PARENTS[1], row.path), `Immutable SQL changed: ${row.id}`);
    await integrationExec('git', ['merge-base', '--is-ancestor', row.introducedBy, 'HEAD'], { cwd: integrationRoot });
  }
  for (const [reviewFile, key] of integrationFiles) {
    const current = JSON.parse(await integrationCurrentText(reviewFile, 'utf8'));
    for (const ref of INTEGRATION_PARENTS) {
      const prior = await integrationParent(ref, reviewFile);
      for (const row of prior[key]) assert.ok(integrationFind(current, row), `Lost whole parent object: ${ref}:${reviewFile}`);
      for (const [field, value] of Object.entries(prior)) {
        if (field === key) continue;
        if (value && typeof value === 'object') assert.ok(integrationFind(current, value), `Lost whole parent metadata: ${ref}:${reviewFile}:${field}`);
        else assert.ok(current[field] === value || current.sourceContextHistory?.some(entry => entry.metadata?.[field] === value),
          `Lost parent metadata: ${ref}:${reviewFile}:${field}`);
      }
    }
  }
});

test('studio gallery appends its original migration and preserves both complete manifest reviews', async () => {
  const file = 'scripts/production-migrations.json';
  const base = '985117b3d9e23b1a84837040d39784e3debcb48e';
  const gallery = '279c1fa994e9cada3e39dc6953a8ef24a6faf7ea';
  const before = await integrationParent(base, file);
  const original = (await integrationParent(gallery, file)).migrations.at(-1);
  const raw = withoutVenueSearch(withoutArtistSimilarity(await manifestBeforeLoudness(file)));
  assert.equal(original.id, '2026-11-07_studio_gallery');
  // The directory semantic search migration is appended last (next test).
  assert.equal(raw.migrations.at(-1).id, '2026-11-21_directory_semantic_search');
  const current = await withoutCourseReminder(await withoutChatDelivery({ ...raw, migrations: raw.migrations.slice(0, -1) }));
  assert.deepEqual(current, { ...before, migrations: [...before.migrations, original] });
  const { stdout } = await integrationExec('git', ['show', original.introducedBy + ':' + original.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, original.path), 'utf8'), stdout);
  for (const [reviewFile, key] of integrationFiles) {
    const currentReview = JSON.parse(await integrationCurrentText(reviewFile, 'utf8'));
    for (const parent of [base, gallery]) {
      const old = await integrationParent(parent, reviewFile);
      const row = old[key].find(r => r.evidence?.file === file || r.name === 'migrations' || r.source?.name === 'migrations');
      if (row) assert.ok(integrationFind(currentReview, row), 'Missing reviewed migration history for ' + parent);
    }
  }
});

test('directory semantic search appends its migration after the chat delivery migration and keeps the reviewed manifest decision', async () => {
  const file = 'scripts/production-migrations.json';
  const main = '6142acf4cb33bee1aff32daf4c283210344feb4b';
  const before = await integrationParent(main, file);
  const current = withoutVenueSearch(withoutArtistSimilarity(await manifestBeforeLoudness(file)));
  const added = current.migrations.at(-1);
  assert.deepEqual(added, {
    id: '2026-11-21_directory_semantic_search',
    path: 'tdf-hq/sql/2026-11-21_directory_semantic_search.sql',
    introducedBy: 'd48fd5f1117f6778759185ac2e6ba559937bc360',
  });
  assert.deepEqual(current, { ...before, migrations: [...before.migrations, added] });
  const { stdout } = await integrationExec('git', ['show', added.introducedBy + ':' + added.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, added.path), 'utf8'), stdout);
  const reviewFile = 'docs/catalog-persistence/catalog-list-decisions.json';
  const decisions = readExpandedDecisions(integrationRoot).decisions;
  // The register on main is larger than the shared helper's buffer.
  const published = await integrationExec('git', ['show', main + ':' + reviewFile],
    { cwd: integrationRoot, maxBuffer: 512 * 1024 * 1024 });
  const previous = JSON.parse(published.stdout).decisions
    .find(row => row.evidence?.file === file && row.evidence?.name === 'migrations');
  const tip = beforeArtistSimilarity(decisionBeforeLoudness(decisions));
  // EVT-VENUE-SEARCH-001 reviews the next append and nests this review unchanged.
  const successor = tip.reviewBatch === 'venue-search-sql-20261121' ? tip.previousDecision : tip;
  assert.equal(successor.evidence.valueCount, before.migrations.length + 1);
  assert.deepEqual(successor.previousDecision, previous);
});

test('artist similarity appends exactly one migration after venue search and nests the reviewed manifest decision', async () => {
  const file = 'scripts/production-migrations.json';
  const main = '1184f9ffeffa2fd5f280fad2f026c12ab7224f4d';
  const before = await integrationParent(main, file);
  const current = await manifestBeforeLoudness(file);
  const added = current.migrations.at(-1);
  assert.deepEqual(added, {
    id: ARTIST_SIMILARITY_ENTRY,
    path: 'tdf-hq/sql/2026-11-21_artist_similarity.sql',
    introducedBy: '89d467302501d47ce53c1551b031c46cb7bb1162',
  });
  assert.deepEqual(current, { ...before, migrations: [...before.migrations, added] });
  await integrationExec('git', ['merge-base', '--is-ancestor', added.introducedBy, 'HEAD'], { cwd: integrationRoot });
  const { stdout } = await integrationExec('git', ['show', added.introducedBy + ':' + added.path], { cwd: integrationRoot });
  assert.equal(await fs.readFile(path.join(integrationRoot, added.path), 'utf8'), stdout);
  const reviewFile = 'docs/catalog-persistence/catalog-list-decisions.json';
  const published = await integrationExec('git', ['show', main + ':' + reviewFile],
    { cwd: integrationRoot, maxBuffer: 512 * 1024 * 1024 });
  const readObject = (objectPath) => execFileSync('git', ['show', main + ':' + objectPath],
    { cwd: integrationRoot, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 });
  const previous = expandAtRevision(JSON.parse(published.stdout), readObject).decisions
    .find(row => row.evidence?.file === file && row.evidence?.name === 'migrations');
  const successor = decisionBeforeLoudness(readExpandedDecisions(integrationRoot).decisions);
  assert.equal(previous.reviewBatch, 'venue-search-sql-20261121');
  assert.equal(successor.evidence.valueCount, before.migrations.length + 1);
  assert.equal(successor.reviewBatch, ARTIST_SIMILARITY_BATCH);
  assert.deepEqual(successor.previousDecision, previous);
});

test('My tickets retains both complete published review graphs and immutable migrations', async () => {
  const assertParent = async (ref) => {
    for (const [file, key] of integrationFiles) {
      const current = integrationCurrentFiles.get(file)?.value
        ?? JSON.parse(await integrationCurrentText(file, 'utf8'));
      const before = await integrationParent(ref, file);
      for (const row of before[key]) {
        assert.ok(integrationFind(current, row), `Lost whole ticket parent object: ${ref}:${file}`);
      }
      for (const [field, value] of Object.entries(before)) {
        if (field === key) continue;
        if (value && typeof value === 'object') {
          assert.ok(integrationFind(current, value), `Lost whole ticket parent metadata: ${ref}:${file}:${field}`);
        } else {
          assert.ok(current[field] === value
            || current.sourceContextHistory?.some(entry => entry.metadata?.[field] === value),
            `Lost ticket parent context: ${ref}:${file}:${field}`);
        }
      }
    }
    const file = 'scripts/production-migrations.json';
    const current = JSON.parse(await fs.readFile(path.join(integrationRoot, file), 'utf8'));
    const before = await integrationParent(ref, file);
    for (const migration of before.migrations) {
      assert.deepEqual(current.migrations.find(row => row.id === migration.id), migration);
      assert.equal(await fs.readFile(path.join(integrationRoot, migration.path), 'utf8'),
        await integrationSource(ref, migration.path));
    }
  };
  await assertParent('a4851729d1ca6816edb3fa535e4b65b7ea5b32dd');
  await assertParent('5fc4facba6664408f7ec5fd95ec02109a6ea824b');
});


async function generatePreservedParentMirror(ref, output) {
  // Dated positive and negative assertions execute their own published
  // generator and source registry, never today's different feature list.
  const fixture = path.join(output, 'historical-source');
  await fs.mkdir(path.join(fixture, 'scripts'), { recursive: true });
  await fs.mkdir(path.join(fixture, 'tdf-hq/assets'), { recursive: true });
  const script = 'scripts/generate-mobile-feature-registry.mjs';
  const registry = 'tdf-hq/assets/feature-registry.json';
  await fs.writeFile(path.join(fixture, script), await integrationSource(ref, script));
  await fs.writeFile(path.join(fixture, registry), await integrationSource(ref, registry));
  await integrationExec(process.execPath, [path.join(fixture, script)],
    { cwd: fixture, env: { ...process.env, TDF_MOBILE_ROOT: output } });
}

test('current My tickets mirror matches its generator and rejects the prior main review', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-my-tickets-mirror-'));
  const file = 'tdf-mobile/src/features/generatedFeatureRegistry.ts';
  try {
    const output = path.join(temporary, 'generated');
    await fs.mkdir(path.join(output, 'src/features'), { recursive: true });
    await integrationExec(process.execPath, [path.join(integrationRoot, 'scripts/generate-mobile-feature-registry.mjs')],
      { cwd: integrationRoot, env: { ...process.env, TDF_MOBILE_ROOT: output } });
    const source = await fs.readFile(path.join(integrationRoot, file), 'utf8');
    assert.equal(source, await fs.readFile(path.join(output, 'src/features/generatedFeatureRegistry.ts'), 'utf8'));
    const repo = path.join(temporary, 'repo');
    await fs.mkdir(repo);
    await integrationExec('git', ['init', '-b', 'main'], { cwd: repo });
    await fs.mkdir(path.join(repo, 'tdf-mobile/src/features'), { recursive: true });
    await fs.writeFile(path.join(repo, file), source);
    await integrationExec('git', ['add', '--', file], { cwd: repo });
    const report = JSON.parse(await fs.readFile(path.join(integrationRoot, 'docs/catalog-persistence/reports/static-list-inventory.json'), 'utf8'));
    const ids = new Set(report.candidates.filter(row => row.file === file).map(row => row.id));
    const register = readExpandedDecisions(integrationRoot);
    const selected = register.decisions.filter(row => ids.has(row.id));
    assert.equal(selected.length, ids.size);
    const current = selected.find(row => row.evidence?.name === 'mobileFeatureRegistry');
    assert.ok(current);
    // The taxonomy filters successor (one appended feature) keeps the My
    // tickets review whole as its predecessor.
    assert.equal(current.id, 'd2149b0cbd417238e171');
    assert.equal(current.previousDecision.id, '105fd7b2786498f6bb56');
    assert.equal(current.previousDecision.previousDecision.id, 'ea29c6e8a6cd165802a8');
    const decisions = path.join(temporary, 'decisions.json');
    await fs.writeFile(decisions, JSON.stringify(selected));
    const args = [path.join(integrationRoot, 'scripts/catalog-list-audit.mjs'), '--root', repo, '--decisions', decisions, '--fail-on-unreviewed'];
    await integrationExec(process.execPath, args, { cwd: repo });
    await fs.writeFile(decisions, JSON.stringify(selected.map(row => row === current ? current.previousDecision : row)));
    await assert.rejects(integrationExec(process.execPath, args, { cwd: repo }),
      error => error.code === 1 && error.stderr.includes('1 unreviewed candidate(s), 1 stale decision(s)'));
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});

test('sidebar and tab lists retain their six individually reviewed decisions', async () => {
  const batch = 'cognitive-load-owner-decisions-20261116';
  const published = await integrationParent('f5dd1736c0d01605e5c7e17914b065bbc782119b', DECISIONS_FILE);
  const reviewed = published.decisions.filter(row => row.reviewBatch === batch);
  assert.equal(reviewed.length, 6);
  assert.deepEqual(readExpandedDecisions(integrationRoot).decisions.filter(row => row.reviewBatch === batch), reviewed);
});
