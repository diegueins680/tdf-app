// Executes the shipped catalog SQL, real public view and delivery predicate.
// Uses only an owned disposable database; no HTTP/storage or production claims.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase, musicTestEnvironment } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const env = musicTestEnvironment({ source: process.env, username: process.env.USER, runtime: process.env.TMPDIR ?? '/tmp' });
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const command = (program, args, options = {}) => execFileSync(program, args, {
  env, encoding: 'utf8', timeout: 60000, maxBuffer: 8 * 1024 * 1024, ...options,
}).trim();
const owner = disposableMusicDatabase({ database, command, env });
const psql = args => command('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database, ...args]);
const sql = input => command('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database], { input });
const quote = value => value === null ? 'NULL' : typeof value === 'number' ? String(value)
  : `'${value.replaceAll("'", "''")}'`;
const source = readFileSync(join(root, 'tdf-hq/src/TDF/Server/MusicRelease.hs'), 'utf8');
const block = source.split('listPublicReleases edgeCountry query artistPartyId requestedLimit requestedOffset = do')[1]
  .split('\ngetPublicRelease ::')[0];
const gapless = block.replace(/\\\n\s*\\/g, '');
const literal = gapless.match(/jsonRows\s+("(?:[^"\\]|\\.)*")/s)?.[1];
assert(literal, 'Locate the actual listPublicReleases SQL literal');
const catalogSql = JSON.parse(literal);
const query = (text, territory = 'EC', limit = 100, offset = 0, artist = null, mutation = x => x) => {
  // The correction normalizes its two input parameters once and then binds
  // the track delivery territory. Keep this runnable on the baseline too.
  const trackAware = catalogSql.includes('JOIN music_recording');
  const params = trackAware ? [text, text, territory, artist, artist, territory, territory, territory, limit, offset]
    : [text, text, text, artist, artist, territory, territory, territory, limit, offset];
  let index = 0;
  const rendered = mutation(catalogSql).replaceAll('?', () => quote(params[index++]));
  assert.equal(index, params.length, 'Shipped SQL parameter contract');
  return sql(rendered).split('\n').filter(Boolean).map(line => JSON.parse(line));
};

try {
  owner.create();
  for (const name of ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql', '2026-09-15_music_version_parties.sql',
    '2026-09-15_music_party_details.sql', '2026-09-16_music_correction_asset_graph.sql',
    '2026-09-16_music_correction_concurrency.sql', '2026-09-16_music_resource_graph_validation.sql',
    '2026-10-11_music_publication_isolation.sql']) {
    psql(['-f', join(root, 'tdf-hq/sql', name)]);
  }
  // Reuse the existing normalizer's exact SQL, not a test reimplementation.
  const directory = readFileSync(join(root, 'tdf-hq/sql/2026-08-14_music_directory_core.sql'), 'utf8');
  const normalizer = directory.match(/CREATE OR REPLACE FUNCTION directory_normalize_text[\s\S]*?\$\$;/)?.[0];
  assert(normalizer);
  sql(normalizer);

  // Reuse the existing submission-valid publication fixture. The two fixture
  // substitutions make the recording title distinct and set its territory.
  const fixtureSource = readFileSync(join(root, 'tdf-hq/test/TDF/MusicPublicationSpec.hs'), 'utf8');
  const fixtureList = fixtureSource.split('[ "CREATE OR REPLACE FUNCTION music_spec_publishable')[1].split(']) []')[0];
  const fixtureLiterals = ('"CREATE OR REPLACE FUNCTION music_spec_publishable' + fixtureList)
    .match(/"(?:[^"\\]|\\.)*"/g);
  let fixture = fixtureLiterals.map(value => JSON.parse(value)).join('\n');
  assert(fixture.includes("VALUES ('Spec '||slug,180000"));
  fixture = fixture.replace('music_spec_publishable(slug TEXT)', 'music_search_publishable(slug TEXT, recording_title TEXT, territory TEXT[])')
    .replace("VALUES ('Spec '||slug,180000", 'VALUES (recording_title,180000')
    .replace("  INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES (recording_title,180000,'not_explicit',actor) RETURNING id INTO rec;", `
  IF slug='another' THEN
    SELECT id INTO rec FROM music_recording WHERE canonical_title=recording_title ORDER BY created_at,id LIMIT 1;
  ELSE
    INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES (recording_title,180000,'not_explicit',actor) RETURNING id INTO rec;
  END IF;`)
    .replace("VALUES (vid,ARRAY['Worldwide'],'full','none',FALSE)", "VALUES (vid,territory,'full','none',FALSE)")
    .replace('cover UUID;', 'cover UUID; rec2 UUID; master2 UUID; mr2 UUID; cr2 UUID;')
    .replace('  PERFORM music_refresh_validation_flags(vid);', `
  -- Multiple matching renditions must not duplicate a release result.
  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at)
    VALUES (vid,rec,master,'stream_audio','local_private','standard','music-private','derivatives/'||vid||'/stream-low.m4a','audio/mp4',32000,encode(sha256((vid||'low')::bytea),'hex'),'ready',TRUE,'{"bitrate_kbps":96}',actor,NOW());
  IF slug='mixed' THEN
    -- The release is visible in EC through a second track, while the searched
    -- recording is US-only. A release-wide visibility check alone leaks it.
    UPDATE music_availability_rule SET release_track_id=(SELECT id FROM music_release_track WHERE release_version_id=vid AND recording_id=rec) WHERE release_version_id=vid;
    INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('Public instrumental companion',180000,'not_explicit',actor) RETURNING id INTO rec2;
    INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist) VALUES (vid,rec2,2,'Spec Artist');
    INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role) VALUES (vid,rec2,mp,'main_artist'),(vid,rec2,mp,'composer');
    INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES (vid,rec2,'master','owned',ARRAY['Worldwide'],'2026-09-11',actor) RETURNING id INTO mr2;
    INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES (vid,rec2,'composition','licensed',ARRAY['Worldwide'],'2026-09-11',actor) RETURNING id INTO cr2;
    INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on) VALUES (mr2,mp,10000,ARRAY['Worldwide'],'2026-09-11'),(cr2,mp,10000,ARRAY['Worldwide'],'2026-09-11');
    INSERT INTO music_asset(release_version_id,recording_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)
      VALUES (vid,rec2,'master_audio','local_private','standard','music-private','masters/'||vid||'/companion.wav','companion.wav','audio/wav',123456,encode(sha256((vid||'cm')::bytea),'hex'),'ready',TRUE,actor,NOW()) RETURNING id INTO master2;
    INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)
      VALUES (vid,rec2,master2,'stream_audio','local_private','standard','music-private','derivatives/'||vid||'/companion.m4a','audio/mp4',65432,encode(sha256((vid||'cs')::bytea),'hex'),'ready',TRUE,actor,NOW());
    INSERT INTO music_availability_rule(release_version_id,release_track_id,territories,listening_policy,download_policy,purchasable)
      SELECT vid,id,ARRAY['EC'],'full','none',FALSE FROM music_release_track WHERE release_version_id=vid AND recording_id=rec2;
  END IF;
  PERFORM music_refresh_validation_flags(vid);`);
  sql("INSERT INTO party(display_name,is_org,created_at) VALUES ('Music publication spec',FALSE,NOW());\n" + fixture);
  const make = (slug, title = 'Canción sólo de pista', territory = 'Worldwide') => {
    const [release, version] = sql(`SELECT release_id,version_id FROM music_search_publishable(${quote(slug)},${quote(title)},ARRAY[${quote(territory)}])`).split('|');
    assert(release && version);
    return { release, version };
  };
  const publish = ({ version }) => {
    // A separate transaction starts after the scheduled instant; no disabled
    // triggers or publication bypasses, including for hidden fixtures.
    sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='${version}'; SELECT pg_sleep(0.01);`);
    sql(`SELECT music_publish_due(100);`);
  };
  const visible = make('visible'); publish(visible);
  const privateRelease = make('private');
  const excluded = make('territory', 'Canción sólo de pista', 'US'); publish(excluded);
  const withdrawn = make('withdrawn'); publish(withdrawn);
  sql(`UPDATE music_release_version SET state='suspended' WHERE id='${withdrawn.version}'; UPDATE music_release_version SET state='withdrawn' WHERE id='${withdrawn.version}';`);
  const embargoed = make('embargoed');
  sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=NOW()+interval '1 day',embargo_until_utc=NOW()+interval '1 day',release_timezone='UTC' WHERE id='${embargoed.version}';`);
  assert.equal(query('Spec visible')[0]?.id, visible.release, 'Existing release-title search control');
  assert.equal(query('Canción sólo de pista')[0]?.id, visible.release, 'Published recording title differing from release title must be searchable');
  for (const text of ['cancion', 'CANCION', '  canción   sólo ', 'sólo de']) {
    assert.deepEqual(query(text).map(row => row.id), [visible.release], `Accent/case/partial match: ${text}`);
  }
  assert.deepEqual(query('Canción', 'EC').map(row => row.id), [visible.release], 'Private/withdrawn/embargoed/territory-excluded recordings stay hidden');
  assert(query('Canción', 'US').some(row => row.id === excluded.release), 'Territory positive control');
  assert(!query('Canción', null).some(row => row.id === excluded.release), 'Unknown territory fails closed');
  assert.equal(query("'; DROP TABLE music_recording; --").length, 0, 'Query remains parameter text');
  assert.equal(query('%_').length, 0, 'Punctuation does not become a match-all wildcard');
  assert.equal(query('Spec Artist').length, 1, 'Existing artist-name search remains available');
  assert.equal(query('no-matching-fixture').length, 0, 'Useful empty result');
  console.log('PASS: shipped SQL finds recording titles; existing release/artist behavior; accent/case/partial; publication and territory isolation');

  // Several independently published releases may contain the same title.
  // Return each release once, preserving the existing stable release ordering.
  const another = make('another'); publish(another);
  assert.equal(sql(`SELECT count(DISTINCT recording_id) FROM music_release_track WHERE release_version_id IN ('${visible.version}','${another.version}')`), '1',
    'Two release contexts deliberately reference the same canonical recording');
  const full = query('Canción');
  assert.equal(full.length, 2);
  assert.equal(new Set(full.map(row => row.id)).size, 2);
  assert.deepEqual([query('Canción', 'EC', 1, 0)[0].id, query('Canción', 'EC', 1, 1)[0].id], full.map(row => row.id));
  assert.equal(query('Canción', 'EC', 100, 0, -1).length, 0, 'Artist filter remains active');
  assert.equal(query('').length, 2, 'Empty browsing still respects visibility');
  assert(full.some(row => row.id === another.release));
  console.log('PASS: release-level deduplication, stable limit/offset pagination, artist filter, empty browsing');

  // Removing only the delivery gate must disclose the otherwise-hidden track
  // of an EC-visible release below. The mutation proves the control detects it.
  const gate = 'music_public_asset_accessible(audio.id,?::text)';
  assert(catalogSql.includes(gate), 'Track matches must reuse the delivery authorization predicate');
  const denied = make('mixed', 'Invisible authorized-track sentinel', 'US');
  publish(denied);
  assert.equal(query('Spec mixed')[0]?.id, denied.release, 'Mixed release is discoverable in EC');
  assert.equal(query('Public instrumental companion')[0]?.id, denied.release, 'EC track is searchable');
  assert.equal(query('Invisible authorized-track sentinel').length, 0, 'US-only track does not reveal an EC-visible release by hidden title');
  assert.equal(query('Invisible authorized-track sentinel', 'US')[0]?.id, denied.release, 'US authorized-track positive control');
  assert.equal(query('Invisible authorized-track sentinel', 'EC', 100, 0, null,
    text => text.replace(gate, '(?::text IS NULL OR TRUE)')).length, 1,
  'Negative control detects removal of track delivery authorization');
  assert(!query('Canción', 'EC').some(row => [privateRelease.release, excluded.release, withdrawn.release, embargoed.release].includes(row.id)));
  console.log('PASS: per-track territory isolation within visible release; deliberate missing-authorization mutant detected');
  const unicode = make('unicode', '夜の波'); publish(unicode);
  assert.equal(query('夜の波')[0]?.id, unicode.release, 'Literal non-Latin title matching is retained');
  assert.equal(query('不存在').length, 0, 'A query without normalized Latin characters does not match every release');
  console.log('PASS: literal non-Latin matching and empty-normalization isolation');
} finally {
  await owner.cleanup();
  console.log('Cleaned only this run’s owned disposable database');
}
