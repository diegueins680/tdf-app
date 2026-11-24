// Content-addressed review history for docs/catalog-persistence/catalog-list-decisions.json.
//
// Active decisions (the top-level `decisions` array) stay inline: the audit and
// the builder read only their own fields. Every other decision object — the
// previousDecision chains, concurrentReviewHistory, retirement and
// reconciliation records — is stored once under
// docs/catalog-persistence/catalog-decision-history/<sha256>.json and replaced
// by { "$historyRef": "sha256:<hex>" }. One file per object keeps parallel
// branches from conflicting: a merge is the union of both directories.
//
// The hash covers the exact stored bytes (JSON.stringify of the stored object,
// key order preserved), so expanding a compacted document reproduces the
// original document byte for byte, and any edit to a stored review is detected.
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';

export const DECISIONS_FILE = 'docs/catalog-persistence/catalog-list-decisions.json';
export const HISTORY_DIR = 'docs/catalog-persistence/catalog-decision-history';
export const REF_KEY = '$historyRef';
const REF_PATTERN = /^sha256:([0-9a-f]{64})$/;

const isPlainObject = (value) => value !== null && typeof value === 'object' && !Array.isArray(value);
export const isRef = (value) => isPlainObject(value) && Object.keys(value).length === 1
  && typeof value[REF_KEY] === 'string';
/** A review record: any object carrying a candidate id and a classification. */
export const isDecisionLike = (value) => isPlainObject(value) && !isRef(value)
  && typeof value.id === 'string' && typeof value.classification === 'string';

export const objectHash = (stored) => 'sha256:' + createHash('sha256').update(JSON.stringify(stored)).digest('hex');
const fileNameFor = (ref) => `${REF_PATTERN.exec(ref)[1]}.json`;

/**
 * Replace every non-active decision object with a reference. Returns the
 * compacted document and the objects it references (hash -> stored object).
 */
export function compactDocument(document) {
  const objects = new Map();
  const memo = new Map();
  const compactValue = (value, inlineThis) => {
    if (Array.isArray(value)) return value.map((item) => compactValue(item, false));
    if (!isPlainObject(value) || isRef(value)) return value;
    if (!inlineThis && memo.has(value)) return memo.get(value);
    const stored = {};
    for (const [key, child] of Object.entries(value)) stored[key] = compactValue(child, false);
    if (inlineThis || !isDecisionLike(value)) return stored;
    const ref = objectHash(stored);
    objects.set(ref, stored);
    const replacement = { [REF_KEY]: ref };
    memo.set(value, replacement);
    return replacement;
  };
  const compacted = {};
  for (const [key, value] of Object.entries(document)) {
    compacted[key] = key === 'decisions' && Array.isArray(value)
      ? value.map((decision) => compactValue(decision, true))
      : compactValue(value, false);
  }
  return { document: compacted, objects };
}

/** Load and integrity-check every stored object in the history directory. */
export function readHistory(root) {
  const dir = path.join(root, HISTORY_DIR);
  const objects = new Map();
  if (!existsSync(dir)) return objects;
  for (const name of readdirSync(dir).sort()) {
    if (!name.endsWith('.json')) throw new Error(`Unexpected file in ${HISTORY_DIR}: ${name}`);
    const bytes = readFileSync(path.join(dir, name), 'utf8');
    const stored = JSON.parse(bytes);
    const ref = objectHash(stored);
    if (fileNameFor(ref) !== name) throw new Error(`History object ${name} does not match its content hash (${ref})`);
    if (bytes !== `${JSON.stringify(stored, null, 2)}\n`) throw new Error(`History object ${name} is not canonically formatted`);
    objects.set(ref, stored);
  }
  return objects;
}

/** Resolve every reference; identical references expand to one shared object. */
export function expandDocument(document, objects) {
  const expanded = new Map();
  const expandValue = (value, trail) => {
    if (Array.isArray(value)) return value.map((item) => expandValue(item, trail));
    if (!isPlainObject(value)) return value;
    if (isRef(value)) {
      const ref = value[REF_KEY];
      if (!REF_PATTERN.test(ref)) throw new Error(`Malformed history reference: ${ref}`);
      if (expanded.has(ref)) return expanded.get(ref);
      if (!objects.has(ref)) throw new Error(`Missing review history object ${ref}`);
      if (trail.has(ref)) throw new Error(`Cyclic review history at ${ref}`);
      const result = expandValue(objects.get(ref), new Set([...trail, ref]));
      expanded.set(ref, result);
      return result;
    }
    const result = {};
    for (const [key, child] of Object.entries(value)) result[key] = expandValue(child, trail);
    return result;
  };
  return expandValue(document, new Set());
}

/** References reachable from a document (directly or through stored objects). */
export function reachableRefs(document, objects) {
  const seen = new Set();
  const visit = (value) => {
    if (Array.isArray(value)) { value.forEach(visit); return; }
    if (!isPlainObject(value)) return;
    if (isRef(value)) {
      const ref = value[REF_KEY];
      if (seen.has(ref)) return;
      seen.add(ref);
      if (objects.has(ref)) visit(objects.get(ref));
      return;
    }
    Object.values(value).forEach(visit);
  };
  visit(document);
  return seen;
}

/** Inline (non-active) decision objects left in a document that should be references. */
export function inlineHistory(document) {
  const found = [];
  const visit = (value, where, active) => {
    if (Array.isArray(value)) { value.forEach((item, index) => visit(item, `${where}[${index}]`, false)); return; }
    if (!isPlainObject(value) || isRef(value)) return;
    if (!active && isDecisionLike(value)) found.push(`${where} (${value.id})`);
    for (const [key, child] of Object.entries(value)) visit(child, `${where}.${key}`, false);
  };
  for (const [key, value] of Object.entries(document)) {
    if (key === 'decisions' && Array.isArray(value)) {
      value.forEach((decision, index) => visit(decision, `decisions[${index}]`, true));
    } else visit(value, key, false);
  }
  return found;
}

/** Read the decisions document with its full review history resolved. */
export function readExpandedDecisions(root, file = DECISIONS_FILE) {
  const document = JSON.parse(readFileSync(path.join(root, file), 'utf8'));
  return expandDocument(document, readHistory(root));
}

/** Expand a decisions document read from a git revision (old documents carry no refs). */
export function expandAtRevision(document, readObject) {
  const objects = new Map();
  const load = (value) => {
    if (Array.isArray(value)) { value.forEach(load); return; }
    if (!isPlainObject(value)) return;
    if (isRef(value)) {
      const ref = value[REF_KEY];
      if (objects.has(ref)) return;
      const stored = JSON.parse(readObject(`${HISTORY_DIR}/${fileNameFor(ref)}`));
      if (objectHash(stored) !== ref) throw new Error(`History object ${ref} does not match its content hash`);
      objects.set(ref, stored);
      load(stored);
      return;
    }
    Object.values(value).forEach(load);
  };
  load(document);
  return expandDocument(document, objects);
}

/**
 * Compact inline history in the decisions file into the store. Existing stored
 * objects are kept; new ones are written. Returns counts for reporting.
 */
export function compactFile(root, file = DECISIONS_FILE) {
  const decisionsPath = path.join(root, file);
  const existing = readHistory(root);
  const current = JSON.parse(readFileSync(decisionsPath, 'utf8'));
  const { document, objects } = compactDocument(expandDocument(current, existing));
  const dir = path.join(root, HISTORY_DIR);
  mkdirSync(dir, { recursive: true });
  let written = 0;
  for (const [ref, stored] of objects) {
    if (existing.has(ref)) continue;
    writeFileSync(path.join(dir, fileNameFor(ref)), `${JSON.stringify(stored, null, 2)}\n`);
    written += 1;
  }
  writeFileSync(decisionsPath, `${JSON.stringify(document, null, 2)}\n`);
  return { referenced: objects.size, written };
}

/** Full integrity check used by CI. Throws with every problem found. */
export function verifyHistory(root, file = DECISIONS_FILE) {
  const objects = readHistory(root);
  const document = JSON.parse(readFileSync(path.join(root, file), 'utf8'));
  const problems = [];
  const inline = inlineHistory(document);
  if (inline.length) problems.push(`Inline review history must be compacted (node scripts/catalog-decision-history.mjs compact): ${inline.slice(0, 5).join(', ')}${inline.length > 5 ? ` and ${inline.length - 5} more` : ''}`);
  for (const [ref, stored] of objects) {
    const nested = inlineHistory({ history: stored }).filter((where) => where !== `history (${stored.id})`);
    if (nested.length) problems.push(`Stored object ${ref} nests history inline: ${nested[0]}`);
  }
  try { expandDocument(document, objects); } catch (error) { problems.push(error.message); }
  const reachable = reachableRefs(document, objects);
  const orphans = [...objects.keys()].filter((ref) => !reachable.has(ref));
  if (orphans.length) problems.push(`Unreferenced history objects: ${orphans.slice(0, 5).join(', ')}${orphans.length > 5 ? ` and ${orphans.length - 5} more` : ''}`);
  if (problems.length) throw new Error(problems.join('\n'));
  return { objects: objects.size, decisions: document.decisions.length };
}
