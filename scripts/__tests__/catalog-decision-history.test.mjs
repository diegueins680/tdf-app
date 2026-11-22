import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync, mkdirSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import {
  DECISIONS_FILE,
  HISTORY_DIR,
  REF_KEY,
  compactDocument,
  compactFile,
  expandAtRevision,
  expandDocument,
  inlineHistory,
  objectHash,
  readHistory,
  verifyHistory,
} from '../lib/catalog-decision-history.mjs';

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

const decision = (id, extra = {}) => ({
  id, classification: 'security-system-registry', disposition: 'retain', specializedModel: 'manifest',
  reviewed: true, justification: 'A substantive reviewed justification.', ...extra,
});

// A merge-shaped document: the successor keeps one parent chain as
// previousDecision and the other as concurrentReviewHistory, both sharing an
// older common ancestor — the duplication this store removes.
function sampleDocument() {
  const ancestor = decision('a', { evidence: { valueCount: 1 } });
  const left = decision('l', { evidence: { valueCount: 2 }, previousDecision: ancestor });
  const right = decision('r', { evidence: { valueCount: 2 }, previousDecision: ancestor });
  return {
    schemaVersion: 1,
    reconciliation: { retiredDecisions: [decision('old')], note: 'kept inline' },
    decisions: [
      decision('m', { evidence: { valueCount: 3 }, previousDecision: left, concurrentReviewHistory: [right] }),
      decision('plain'),
    ],
  };
}

function tempRepo(document) {
  const root = mkdtempSync(path.join(os.tmpdir(), 'decision-history-'));
  mkdirSync(path.join(root, path.dirname(DECISIONS_FILE)), { recursive: true });
  writeFileSync(path.join(root, DECISIONS_FILE), `${JSON.stringify(document, null, 2)}\n`);
  return root;
}

test('compaction stores each review once and expansion restores the exact document', () => {
  const original = sampleDocument();
  const { document, objects } = compactDocument(original);
  // ancestor, left, right and the retired decision; active decisions stay inline.
  assert.equal(objects.size, 4);
  assert.equal(document.decisions[0].id, 'm');
  assert.deepEqual(Object.keys(document.decisions[0].previousDecision), [REF_KEY]);
  assert.deepEqual(inlineHistory(document), []);
  assert.equal(JSON.stringify(expandDocument(document, objects), null, 2), JSON.stringify(original, null, 2));
});

test('compaction is deterministic and idempotent', () => {
  const first = compactDocument(sampleDocument());
  const again = compactDocument(expandDocument(first.document, first.objects));
  assert.deepEqual(again.document, first.document);
  assert.deepEqual([...again.objects.keys()].sort(), [...first.objects.keys()].sort());
});

test('a file round-trips byte for byte and verifies', (t) => {
  const original = sampleDocument();
  const root = tempRepo(original);
  t.after(() => rmSync(root, { recursive: true, force: true }));
  compactFile(root);
  assert.equal(readdirSync(path.join(root, HISTORY_DIR)).length, 4);
  assert.deepEqual(verifyHistory(root), { objects: 4, decisions: 2 });
  const expanded = expandDocument(JSON.parse(readFileSync(path.join(root, DECISIONS_FILE), 'utf8')), readHistory(root));
  assert.equal(`${JSON.stringify(expanded, null, 2)}\n`, `${JSON.stringify(original, null, 2)}\n`);
});

test('verification rejects edited, missing, orphaned or inline history', (t) => {
  const root = tempRepo(sampleDocument());
  t.after(() => rmSync(root, { recursive: true, force: true }));
  compactFile(root);
  const dir = path.join(root, HISTORY_DIR);
  const [name] = readdirSync(dir);
  const file = path.join(dir, name);
  const bytes = readFileSync(file, 'utf8');

  writeFileSync(file, bytes.replace('A substantive', 'An edited'));
  assert.throws(() => verifyHistory(root), /does not match its content hash/);

  rmSync(file);
  assert.throws(() => verifyHistory(root), /Missing review history object/);
  writeFileSync(file, bytes);

  const orphan = decision('orphan');
  writeFileSync(path.join(dir, `${objectHash(orphan).slice(7)}.json`), `${JSON.stringify(orphan, null, 2)}\n`);
  assert.throws(() => verifyHistory(root), /Unreferenced history objects/);
  rmSync(path.join(dir, `${objectHash(orphan).slice(7)}.json`));

  const document = JSON.parse(readFileSync(path.join(root, DECISIONS_FILE), 'utf8'));
  document.decisions[1].previousDecision = decision('inline');
  writeFileSync(path.join(root, DECISIONS_FILE), `${JSON.stringify(document, null, 2)}\n`);
  assert.throws(() => verifyHistory(root), /Inline review history must be compacted/);
  compactFile(root);
  assert.equal(verifyHistory(root).objects, 5);
});

test('a revision without references expands to itself; with references it reads the store at that revision', () => {
  const original = sampleDocument();
  assert.deepEqual(expandAtRevision(original, () => assert.fail('no store read expected')), original);
  const { document, objects } = compactDocument(original);
  const files = new Map([...objects].map(([ref, stored]) => [`${HISTORY_DIR}/${ref.slice(7)}.json`, JSON.stringify(stored)]));
  assert.deepEqual(expandAtRevision(document, (file) => files.get(file)), original);
});

test('the committed decisions history is compact, complete and intact', () => {
  const { objects, decisions } = verifyHistory(repoRoot);
  assert.ok(decisions > 0 && objects > 0);
});
