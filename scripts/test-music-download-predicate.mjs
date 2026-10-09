// Execute the exact SQL predicate used by both Haskell download handlers.
// This focused contract test is read-only and does not replace handler E2E.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { userInfo } from 'node:os';

const source = readFileSync(new URL('../tdf-hq/src/TDF/Server/MusicReleaseAssets.hs', import.meta.url), 'utf8');
const match = source.match(/^downloadableAssetPredicate =\n  ("[^\n]+")$/m);
assert.ok(match, 'Predicate representation changed: update the contract reader, never silently skip it');
const predicate = JSON.parse(match[1]);
assert.equal(source.split('<> downloadableAssetPredicate <>').length - 1, 2,
  'Both free-grant and entitled-download handlers must use the predicate');
const result = execFileSync('psql', ['-X', '-h', '127.0.0.1', '-p', '5432', '-d', 'postgres',
  '-Atq', '-v', 'ON_ERROR_STOP=1', '-c', `
  WITH asset(name,processing_state,asset_role,immutable,storage_class,expected) AS (VALUES
    ('promoted master','valid','master_audio',TRUE,'standard',TRUE),
    ('infrequent master','valid','master_audio',TRUE,'infrequent',TRUE),
    ('mutable master','valid','master_audio',FALSE,'standard',FALSE),
    ('quarantined master','valid','master_audio',TRUE,'quarantine',FALSE),
    ('uploaded master','uploaded','master_audio',FALSE,'quarantine',FALSE),
    ('failed master','failed','master_audio',TRUE,'standard',FALSE),
    ('invalid master','invalid','master_audio',TRUE,'standard',FALSE),
    ('deleted master','deleted','master_audio',TRUE,'standard',FALSE),
    ('processing master','processing','master_audio',TRUE,'standard',FALSE),
    ('non-ready derivative','valid','stream_audio',TRUE,'standard',FALSE),
    ('non-ready artwork','valid','cover_original',TRUE,'standard',FALSE),
    ('legacy ready master','ready','master_audio',TRUE,'standard',TRUE),
    ('ready derivative','ready','stream_audio',TRUE,'standard',TRUE),
    ('ready graphic','ready','cover_display',TRUE,'standard',TRUE)
  ) SELECT name FROM asset WHERE ((${predicate}) IS TRUE) IS DISTINCT FROM expected;
`], { encoding: 'utf8', timeout: 15000,
  env: { PATH: process.env.PATH, PGUSER: userInfo().username, PGCONNECT_TIMEOUT: '5' } }).trim();
assert.equal(result, '', `Incorrect download decisions: ${result}`);
console.log('PASS 14 download-state cases against the production SQL predicate; read-only PostgreSQL');
