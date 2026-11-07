// Execute the shipped preference migration and actual public handler query on
// an owned PostgreSQL fixture. Does not establish native decoding or HTTP auth.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase, musicTestEnvironment } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const source = readFileSync(join(root, 'tdf-hq/src/TDF/Server/MusicRelease.hs'), 'utf8');
function publicQuery(text) {
  assert(text.includes(':<|> getArtistFeaturedTrack False'), 'Public route never enables private references');
  const block = text.split('getArtistFeaturedTrack includeUnavailable artistId edgeCountry = do')[1]
    ?.split('getOwnFeaturedTrack ::')[0];
  assert(block?.includes('requirePublicFeature'), 'Public feature gate');
  assert(block.includes('Assets.trustedEdgeTerritory edgeCountry'), 'Trusted territory');
  assert(block.includes('private, no-store'), 'Public preference must not be shared-cacheable');
  const gapless = block.replace(/\\\n\s*\\/g, '');
  const match = gapless.match(/jsonOne err404\s+("(?:[^"\\]|\\.)*")/s);
  assert(match, 'Actual public query');
  const statement = JSON.parse(match[1]);
  const list = gapless.slice(match.index + match[0].length).match(/^\s*\[([^\]]*)\]/s);
  assert(list, 'Actual public bindings');
  const bindings = list[1].split(',').map(value => value.trim().replace(/\s+/g, ' '));
  assert.deepEqual(bindings, ['PersistInt64 artistId', 'PersistBool includeUnavailable',
    'PersistBool includeUnavailable', 'PersistBool includeUnavailable', 'optionalText territory', 'PersistInt64 artistId']);
  assert.equal((statement.match(/\?/g) ?? []).length, bindings.length);
  assert(statement.includes('selection.artist_party_id=?'), 'Scope selection to the requested artist');
  for (const field of ['recording_id', 'legacy_release_id', 'updated_at']) {
    assert(statement.includes(`CASE WHEN ?::boolean OR choice.available THEN choice.${field} END`), `Hide unavailable ${field}`);
  }
  return statement;
}
function writeContract(text) {
  const body = text.split('setOwnFeaturedTrack user edgeCountry MusicArtistFeaturedTrackRequest{..} = do')[1];
  assert(body?.includes('unless (musicFeaturedArtistPartyId == currentPartyId user) (throwError err403)'), 'Expected owner prevents a switched account clear/write');
  assert(body.includes('[ PersistInt64 (currentPartyId user)'), 'Derive the database actor from authentication');
  assert(body.includes('requirePublicFeature'), 'Feature gate is enforced on writes');
}
const statement = publicQuery(source); writeContract(source);
assert.throws(() => publicQuery(source.replace('getArtistFeaturedTrack False', 'getArtistFeaturedTrack True')));
assert.throws(() => publicQuery(source.replace('selection.artist_party_id=?', 'selection.artist_party_id IS NOT NULL')));
assert.throws(() => writeContract(source.replace('musicFeaturedArtistPartyId == currentPartyId user', 'True')));
console.log('PASS shipped query/bindings, public/private projection and expected-owner guards; removed-guard controls detected');
if (process.argv.includes('--contract-only')) process.exit(0);

const env = musicTestEnvironment({ source: process.env, username: process.env.USER, runtime: process.env.TMPDIR ?? '/tmp' });
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const command = (program, args, options = {}) => execFileSync(program, args, { env, encoding: 'utf8',
  timeout: 90000, maxBuffer: 8 * 1024 * 1024, ...options }).trim();
const owner = disposableMusicDatabase({ database, command, env });
const args = ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-v', 'VERBOSITY=verbose', '-d', database];
const sql = input => command('psql', args, { input });
const apply = file => command('psql', [...args, '-f', join(root, 'tdf-hq/sql', file)]);
const migration = '2026-10-30_artist_featured_track.sql';
const rollback = '2026-10-30_artist_featured_track_rollback.sql';
const identityForward = '2026-11-07_artist_featured_identity_guard.sql';
const identityRollback = '2026-11-07_artist_featured_identity_guard_rollback.sql';
const canonicalIdentity = readFileSync(join(root, 'tdf-hq/sql/2026-09-17_identity_reconciliation.sql'), 'utf8');
const canonicalGuard = canonicalIdentity.slice(canonicalIdentity.indexOf('CREATE OR REPLACE FUNCTION identity_reject_archived_reference()'),
  canonicalIdentity.indexOf('CREATE OR REPLACE FUNCTION identity_reject_archived_party_write()'));
assert(canonicalGuard.startsWith('CREATE OR REPLACE FUNCTION identity_reject_archived_reference()'));
const withoutIdentityGuard = process.argv.includes('--without-identity-guard-negative-control');
const quote = value => value === null ? 'NULL' : typeof value === 'boolean' ? String(value)
  : typeof value === 'number' ? String(value) : `'${value.replaceAll("'", "''")}'`;
function read(artist, privateView = false, territory = 'EC') {
  const values = [artist, privateView, privateView, privateView, territory, artist];
  let index = 0;
  return JSON.parse(sql(statement.replaceAll('?', () => quote(values[index++]))));
}
const set = (actor, recording = null, legacy = null, territory = 'EC') =>
  sql(`SELECT music_set_artist_featured_track(${actor},${quote(recording)}::uuid,${quote(legacy)}::bigint,${quote(territory)})`) === 't';
try {
  owner.create();
  for (const file of ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql', '2026-09-15_music_version_parties.sql',
    '2026-09-15_music_party_details.sql', '2026-09-16_music_correction_asset_graph.sql',
    '2026-09-16_music_correction_concurrency.sql', '2026-09-16_music_resource_graph_validation.sql',
    '2026-10-11_music_publication_isolation.sql', '2026-10-15_music_exact_minor_units.sql']) apply(file);
  const fixtureSource = readFileSync(join(root, 'tdf-hq/test/TDF/MusicPublicationSpec.hs'), 'utf8');
  const list = fixtureSource.split('[ "CREATE OR REPLACE FUNCTION music_spec_publishable')[1].split(']) []')[0];
  const fixture = ('"CREATE OR REPLACE FUNCTION music_spec_publishable' + list).match(/"(?:[^"\\]|\\.)*"/g).map(value => JSON.parse(value)).join('\n').replace("VALUES (vid,ARRAY['Worldwide'],'full','none',FALSE)", "VALUES (vid,ARRAY['US'],'full','none',FALSE)");
  sql("INSERT INTO party(display_name,is_org,created_at) VALUES ('Music publication spec',FALSE,NOW());\n" + fixture);
  const [release, version] = sql("SELECT release_id,version_id FROM music_spec_publishable('featured')").split('|');
  const actor = Number(sql(`SELECT artist_party_id FROM music_release WHERE id='${release}'`));
  const other = Number(sql("INSERT INTO party(display_name) VALUES ('Synthetic unrelated artist') RETURNING id"));
  const recording = sql(`SELECT recording_id FROM music_release_track WHERE release_version_id='${version}' LIMIT 1`);
  apply(migration); apply(migration);
  sql('CREATE TABLE identity_party_archive(party_id BIGINT PRIMARY KEY REFERENCES party(id));\n' + canonicalGuard);
  if (!withoutIdentityGuard) { apply(identityForward); apply(identityForward); }
  assert.equal(set(actor, recording, null, 'US'), false, 'Approved is not published');
  sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='${version}'`);
  sql('SELECT pg_sleep(0.02); SELECT music_publish_due(100)');
  assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${version}'`), 'published');
  assert.equal(set(actor, recording, null, 'US'), true);
  assert.equal(read(actor, false, 'US').recordingId, recording);
  assert.equal(set(other, recording), false, 'Another artist cannot select the recording');
  const legacy = Number(sql(`INSERT INTO artist_release(artist_party_id,title,spotify_url) VALUES (${actor},'Synthetic direct legacy','https://media.invalid/legacy.mp3') RETURNING id`));
  assert.equal(set(actor, recording, legacy), false, 'Two source identities are ambiguous');
  assert.equal(set(other, null, legacy), false, 'Legacy membership is also scoped');
  assert.equal(set(actor, null, legacy), true);
  assert.equal(read(actor).legacyReleaseId, legacy);
  assert.equal(set(actor, recording, null, 'US'), true);
  const hidden = read(actor);
  assert.deepEqual([hidden.recordingId, hidden.legacyReleaseId, hidden.updatedAt, hidden.available], [null, null, null, false]);
  assert.equal(read(actor, true).recordingId, recording, 'Owner retains unavailable reference');
  assert.equal(read(actor, false, 'US').recordingId, recording, 'Actual asset gate allows the authorized territory');
  assert.equal(set(actor, recording), false, 'Unavailable selection cannot be re-saved');
  const before = sql('TABLE music_artist_featured_track');
  apply(rollback); apply(rollback); apply(migration); apply(migration);
  assert.equal(sql('TABLE music_artist_featured_track'), before, 'Rollback/reapply preserves preferences');
  sql(`UPDATE music_release_version SET state='suspended' WHERE id='${version}'; UPDATE music_release_version SET state='withdrawn' WHERE id='${version}'`);
  assert.equal(read(actor, false, 'US').recordingId, null, 'Withdrawal is rechecked');
  assert.equal(read(actor, true, 'US').recordingId, recording);
  assert.equal(set(actor), true, 'Owner can clear an unavailable choice');
  assert.equal(read(actor, true).recordingId, null);
  assert.equal(sql('SELECT count(*) FROM music_artist_featured_track'), '0');
  console.log('PASS actual PostgreSQL: publication, canonical/legacy membership, ambiguity, territory, private diagnostics, withdrawal, clear and preserving rollback');
  // These are fresh, test-owned rows in this run's unique disposable database.
  const archived = Number(sql("INSERT INTO party(display_name) VALUES ('Synthetic archived preference subject') RETURNING id"));
  sql(`INSERT INTO identity_party_archive(party_id) VALUES (${archived})`);
  const denied = action => assert.throws(action, error => typeof error.stderr === 'string'
    && error.stderr.includes('55000') && error.stderr.includes('archived contact requires administrative resolution'));
  denied(() => sql(`INSERT INTO music_artist_featured_track(artist_party_id,legacy_release_id) VALUES (${archived},${legacy})`));
  console.log('PASS featured identity archived INSERT denied55000');
  sql(`INSERT INTO music_artist_featured_track(artist_party_id,legacy_release_id) VALUES (${actor},${legacy})`);
  apply(identityForward); apply(identityForward);
  assert.equal(sql('SELECT count(*) FROM music_artist_featured_track'), '1');
  console.log('PASS featured identity active INSERT and forward replay retain preference');
  sql(`INSERT INTO identity_party_archive(party_id) VALUES (${actor}); UPDATE music_artist_featured_track SET updated_at=clock_timestamp() WHERE artist_party_id=${actor}`);
  assert.equal(sql('SELECT artist_party_id FROM music_artist_featured_track'), String(actor));
  console.log('PASS featured identity unchanged historical archived reference remains writable');
  denied(() => sql(`UPDATE music_artist_featured_track SET artist_party_id=${archived} WHERE artist_party_id=${actor}`));
  assert.equal(sql('SELECT artist_party_id FROM music_artist_featured_track'), String(actor));
  console.log('PASS featured identity archived reassignment denied55000 with prior row retained');
  const guard = () => sql("SELECT tgenabled::text || '|' || encode(tgargs,'hex') FROM pg_trigger WHERE tgrelid='music_artist_featured_track'::regclass AND tgname='identity_archive_reference_guard'");
  const expectedGuard = 'O|' + Buffer.from('artist_party_id\0').toString('hex');
  assert.equal(guard(), expectedGuard);
  assert.throws(() => apply(identityRollback), error => typeof error.stderr === 'string'
    && error.stderr.includes('55000') && error.stderr.includes('Featured preference evidence exists; use a forward identity repair'));
  assert.equal(sql('SELECT artist_party_id FROM music_artist_featured_track'), String(actor));
  assert.equal(guard(), expectedGuard);
  console.log('PASS featured identity populated rollback refuses55000 before dropping protection');
  sql(`DELETE FROM music_artist_featured_track WHERE artist_party_id=${actor}`);
  apply(identityRollback); apply(identityRollback);
  assert.equal(guard(), '');
  assert.equal(sql("SELECT to_regclass('music_artist_featured_track') IS NOT NULL"), 't');
  apply(identityForward); apply(identityForward);
  assert.equal(guard(), expectedGuard);
  console.log('PASS featured identity owned empty rollback/reinstall preserves table and canonical guard');
  apply(identityRollback);
  sql('DROP FUNCTION identity_reject_archived_reference()');
  assert.throws(() => apply(identityForward), error => typeof error.stderr === 'string'
    && error.stderr.includes('55000') && error.stderr.includes('requires the canonical archived-reference guard'));
  assert.equal(guard(), '');
  sql(canonicalGuard); apply(identityForward);
  console.log('PASS featured identity missing canonical helper fails closed then restores');
  apply(identityRollback);
  sql('ALTER TABLE music_artist_featured_track DROP CONSTRAINT music_artist_featured_track_artist_party_id_fkey');
  assert.throws(() => apply(identityForward), error => typeof error.stderr === 'string'
    && error.stderr.includes('55000') && error.stderr.includes('requires the original NOT NULL BIGINT Party FK'));
  assert.equal(guard(), '');
  sql('ALTER TABLE music_artist_featured_track ADD CONSTRAINT music_artist_featured_track_artist_party_id_fkey FOREIGN KEY(artist_party_id) REFERENCES party(id) ON DELETE CASCADE');
  apply(identityForward); assert.equal(guard(), expectedGuard);
  console.log('PASS featured identity missing expected FK fails closed then restores');

} finally { await owner.cleanup(); }
