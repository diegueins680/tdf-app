import {readFileSync,writeFileSync} from 'node:fs';
import {join} from 'node:path';
import assert from 'node:assert/strict';
const root='/private/tmp/tdf-audio-http-qualification-20261026';
const source=readFileSync(join(root,'scripts/test-music-catalog-search.mjs'),'utf8');
const chunk=source.slice(source.indexOf('  const fixtureSource ='),source.indexOf('  const make ='));
assert(chunk.includes('music_search_publishable'));
let statements=[];
new Function('root','readFileSync','join','assert','sql',chunk)(root,readFileSync,join,assert,text=>statements.push(text));
assert.equal(statements.length,1);
// Only the owned synthetic fixture provider changes, to exercise real signing.
// No object data or network storage is claimed or requested by this probe.
writeFileSync('/private/tmp/tdf-audio-http-evidence-20261026/fixture.sql',statements[0].replaceAll("'local_private'","'s3_compatible'")+'\n');
