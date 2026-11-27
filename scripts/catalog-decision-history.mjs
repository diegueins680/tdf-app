#!/usr/bin/env node
// Maintain the content-addressed review history of the catalog-list decisions.
//
//   node scripts/catalog-decision-history.mjs compact   move inline history into the store
//   node scripts/catalog-decision-history.mjs verify    integrity check (CI)
//   node scripts/catalog-decision-history.mjs expand [--output PATH]
//                                                       print the fully nested document
import { writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { compactFile, readExpandedDecisions, verifyHistory } from './lib/catalog-decision-history.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const [command, ...rest] = process.argv.slice(2);

if (command === 'compact') {
  const { referenced, written } = compactFile(root);
  const { objects } = verifyHistory(root);
  console.log(`Compacted review history: ${referenced} referenced objects, ${written} new, ${objects} stored.`);
} else if (command === 'verify') {
  const { objects, decisions } = verifyHistory(root);
  console.log(`Catalog decision history verified: ${decisions} active decisions, ${objects} stored review objects.`);
} else if (command === 'expand') {
  const outputIndex = rest.indexOf('--output');
  const text = `${JSON.stringify(readExpandedDecisions(root), null, 2)}\n`;
  if (outputIndex >= 0) writeFileSync(path.resolve(rest[outputIndex + 1]), text);
  else process.stdout.write(text);
} else {
  console.error('Usage: node scripts/catalog-decision-history.mjs compact | verify | expand [--output PATH]');
  process.exit(2);
}
