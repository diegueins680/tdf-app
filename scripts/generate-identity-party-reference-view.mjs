#!/usr/bin/env node
import { readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

const root = fileURLToPath(new URL('../', import.meta.url));
const snake = value => value.replace(/([a-z0-9])([A-Z])/g, '$1_$2').toLowerCase();
const references = new Map();
function scan(directory) {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) scan(path);
    else if (entry.isFile() && path.endsWith('.hs')) {
      for (const [, block] of readFileSync(path, 'utf8').matchAll(/\[persistLowerCase\|([\s\S]*?)\|\]/g)) {
        let table;
        for (const line of block.split('\n')) {
          if (/^[A-Z]\w*\b/.test(line)) table = /\bsql=(\w+)/.exec(line)?.[1] ?? snake(line.split(/\s/)[0]);
          const field = /^\s+(\w+)\s+PartyId\b/.exec(line);
          if (field && table) {
            const column = /\bsql=(\w+)/.exec(line)?.[1] ?? snake(field[1]);
            references.set(`${table}.${column}`, [table, column]);
          }
        }
      }
    }
  }
}
scan(join(root, 'tdf-hq/src'));
if (references.size < 200) throw new Error('Model reference discovery unexpectedly incomplete');
const rows = [...references].sort(([a], [b]) => a.localeCompare(b, 'en')).map(([, [table, column]]) => `  ('${table}', '${column}')`);
const generated = `-- BEGIN MODEL PARTY REFERENCES\nCREATE OR REPLACE VIEW identity_known_party_reference AS\nSELECT * FROM (VALUES\n${rows.join(',\n')}\n) AS reference(table_name,column_name);\nREVOKE ALL ON identity_known_party_reference FROM PUBLIC;\n-- END MODEL PARTY REFERENCES`;
// The registry lives in the newest forward migration carrying the generation
// markers: the database view is whatever the last applied CREATE OR REPLACE
// published. Applied migrations are never rewritten; a model change adds the
// markers to a new, unreleased migration, which then becomes the target.
const sqlDirectory = join(root, 'tdf-hq/sql');
const marked = readdirSync(sqlDirectory)
  .filter(name => name.endsWith('.sql') && !name.endsWith('_rollback.sql'))
  .sort()
  .filter(name => readFileSync(join(sqlDirectory, name), 'utf8').includes('-- BEGIN MODEL PARTY REFERENCES'));
if (marked.length === 0) throw new Error('No migration carries the Party reference registry markers');
const path = join(sqlDirectory, marked[marked.length - 1]);
const source = readFileSync(path, 'utf8');
const updated = source.replace(/-- BEGIN MODEL PARTY REFERENCES[\s\S]*?-- END MODEL PARTY REFERENCES/, generated);
if (!updated.includes(generated)) throw new Error('Migration generation markers missing');
if (process.argv.includes('--check')) {
  if (updated !== source) throw new Error('Identity Party reference registry differs from the models; review a forward migration after release.');
} else writeFileSync(path, updated);
console.log(`Verified ${references.size} model-declared Party references in ${marked[marked.length - 1]}.`);
