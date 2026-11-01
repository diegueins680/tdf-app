// Exact frozen publication fixture generator; no database or application calls.
import { readFileSync, writeFileSync, lstatSync } from 'node:fs';
import { join, resolve, dirname, basename } from 'node:path';
import { createHash } from 'node:crypto';
import assert from 'node:assert/strict';

const root = '/private/tmp/tdf-universal-music-search-20261026';
const sha = value => createHash('sha256').update(value).digest('hex');
const source = readFileSync(join(root, 'scripts/test-music-track-search.mjs'), 'utf8');
assert.equal(sha(source), 'dfdbe15b58975255caa4d60b6d653c0e8414cfd5657968be776053c8c254e64e');
assert.equal(sha(readFileSync(join(root, 'tdf-hq/test/TDF/MusicPublicationSpec.hs'))), 'f1e201ad215e4485d6bbf6c22c88dc482996e68183637a084395350cc04ee0e3');
const start = source.indexOf('  const fixtureSource =');
const end = source.indexOf('  const make =', start);
assert(start > 0 && end > start);
const chunk = source.slice(start, end);
assert(chunk.includes('music_track_search_publishable'));
const statements = [];
new Function('root', 'readFileSync', 'join', 'assert', 'sql', chunk)(root, readFileSync, join, assert, value => statements.push(value));
assert.equal(statements.length, 1);
const output = resolve(process.argv[2] ?? '');
assert.equal(basename(output), 'fixture.sql');
assert(dirname(output).startsWith('/private/tmp/tdf-track-search-http-c88-'));
assert(!lstatSync(dirname(output)).isSymbolicLink());
// No substitutions, storage-provider overrides or publication-guard bypasses.
writeFileSync(output, statements[0] + '\n', { flag: 'wx' });
