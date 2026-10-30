// Actual appended track-search SQL and Haskell binding contract against an owned
// PostgreSQL database. Selected migration fixture, not HTTP/storage/full-schema proof.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase, musicTestEnvironment } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const source = readFileSync(join(root, 'tdf-hq/src/TDF/Server/MusicRelease.hs'), 'utf8');
const gate = 'music_public_asset_accessible(asset.id,?::text)';
const expectedBindings = ['PersistText rawQuery', 'PersistText rawQuery', 'optionalText territory',
  'PersistInt64 (fromIntegral limit)', 'PersistInt64 (fromIntegral offset)'];
function extract(handler) {
  const block = handler.split('searchPublicTracks edgeCountry query requestedLimit requestedOffset =')[1];
  assert(block, 'Actual search handler must exist');
  assert(block.includes('requirePublicFeature'), 'Reuse the public feature gate');
  assert(block.includes('Assets.trustedEdgeTerritory edgeCountry'), 'Use trusted territory');
  assert(block.includes('T.null rawQuery then pure []'), 'Blank query must skip the catalog query');
  assert(block.includes('T.length rawQuery > 200'), 'Bound query length after trimming');
  assert(block.includes('limit >= 1 && limit <= 100 && offset >= 0'), 'Bound pagination');
  assert(block.includes('fromMaybe 13 requestedLimit'), 'Default matches universal suggestion size');
  assert(block.includes('addHeader ("private, no-store" :: Text)'), 'Metadata response cache policy');
  assert(block.includes('search `catchError`'), 'Handler errors receive the same cache policy');
  const gapless = block.replace(/\\\n\s*\\/g, '');
  const match = gapless.match(/jsonRows\s+("(?:[^"\\]|\\.)*")/s);
  assert(match, 'Actual SQL literal');
  const statement = JSON.parse(match[1]);
  assert(statement.includes(gate), 'Reuse authoritative delivery policy');
  assert(statement.includes('matching AS MATERIALIZED'), 'Match public contexts before scoped authorization');
  assert(statement.includes('authorized AS MATERIALIZED'), 'Authorize before public result projection');
  const list = gapless.slice(match.index + match[0].length).match(/^\s*\[([^\]]*)\]/s);
  assert(list, 'Actual Haskell bindings');
  const bindings = list[1].split(',').map(item => item.trim().replace(/\s+/g, ' '));
  assert.deepEqual(bindings, expectedBindings, 'Actual binding expressions and positional order');
  assert.equal((statement.match(/\?/g) ?? []).length, bindings.length);
  return { statement, bindings };
}
const { statement, bindings } = extract(source);
// Controls validate the shipped argument list, not a manually inferred SQL count.
assert.throws(() => extract(source.replace(/PersistText rawQuery,\s*PersistText rawQuery,\s*optionalText territory/,
  'PersistText rawQuery, PersistText rawQuery')), assert.AssertionError);
assert.throws(() => extract(source.replace(/PersistText rawQuery,\s*PersistText rawQuery,\s*optionalText territory/,
  'optionalText territory, PersistText rawQuery, PersistText rawQuery')), assert.AssertionError);
assert.throws(() => extract(source.replaceAll(gate, '(?::text IS NULL OR TRUE)')), assert.AssertionError);
console.log('PASS contract: actual SQL/bindings, public/trusted-territory boundary, input/cache guards; omitted/reordered binding and removed-policy controls detected');
if (process.argv.includes('--contract-only')) process.exit(0);

const env = musicTestEnvironment({ source: process.env, username: process.env.USER, runtime: process.env.TMPDIR ?? '/tmp' });
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const command = (program, args, options = {}) => execFileSync(program, args, {
  env, encoding: 'utf8', timeout: 60000, maxBuffer: 8 * 1024 * 1024, ...options,
}).trim();
const owner = disposableMusicDatabase({ database, command, env });
const psqlArgs = ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database];
const sql = input => command('psql', psqlArgs, { input });
const quote = value => value === null ? 'NULL' : typeof value === 'number' ? String(value)
  : `'${value.replaceAll("'", "''")}'`;
function query(text, territory = 'EC', limit = 100, offset = 0, mutate = x => x) {
  const values = { 'PersistText rawQuery': text, 'optionalText territory': territory,
    'PersistInt64 (fromIntegral limit)': limit, 'PersistInt64 (fromIntegral offset)': offset };
  const parameters = bindings.map(binding => {
    assert(Object.hasOwn(values, binding));
    return values[binding];
  });
  let index = 0;
  const rendered = mutate(statement).replaceAll('?', () => quote(parameters[index++]));
  assert.equal(index, parameters.length, 'Rendered placeholders equal actual Haskell bindings');
  return sql(rendered).split('\n').filter(Boolean).map(line => JSON.parse(line));
}
const same = (rows, ids, message) => assert.deepEqual(rows.map(row => row.recordingId), ids, message);
try {
  owner.create();
  const migrations = ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql', '2026-09-15_music_version_parties.sql',
    '2026-09-15_music_party_details.sql', '2026-09-16_music_correction_asset_graph.sql',
    '2026-09-16_music_correction_concurrency.sql', '2026-09-16_music_resource_graph_validation.sql',
    '2026-10-11_music_publication_isolation.sql', '2026-10-15_music_exact_minor_units.sql'];
  for (const migration of migrations) command('psql', [...psqlArgs, '-f', join(root, 'tdf-hq/sql', migration)]);
  const normalizer = readFileSync(join(root, 'tdf-hq/sql/2026-08-14_music_directory_core.sql'), 'utf8')
    .match(/CREATE OR REPLACE FUNCTION directory_normalize_text[\s\S]*?\$\$;/)?.[0];
  assert(normalizer); sql(normalizer);
  const fixtureSource = readFileSync(join(root, 'tdf-hq/test/TDF/MusicPublicationSpec.hs'), 'utf8');
  const fixtureList = fixtureSource.split('[ "CREATE OR REPLACE FUNCTION music_spec_publishable')[1].split(']) []')[0];
  const fixtureLiterals = ('"CREATE OR REPLACE FUNCTION music_spec_publishable' + fixtureList).match(/"(?:[^"\\]|\\.)*"/g);
  let fixture = fixtureLiterals.map(value => JSON.parse(value)).join('\n');
  const replace = (from, to) => { assert(fixture.includes(from), `Publication fixture anchor: ${from}`); fixture = fixture.replace(from, to); };
  replace('music_spec_publishable(slug TEXT)', 'music_track_search_publishable(slug TEXT, recording_title TEXT, mode TEXT, reuse UUID)');
  replace('cover UUID;', 'cover UUID; rec2 UUID; master2 UUID; mr2 UUID; cr2 UUID;');
  replace("INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('Spec '||slug,180000,'not_explicit',actor) RETURNING id INTO rec;",
    "IF reuse IS NULL THEN INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES (recording_title,180000,'not_explicit',actor) RETURNING id INTO rec; ELSE rec := reuse; END IF;");
  replace("VALUES (vid,ARRAY['Worldwide'],'full','none',FALSE)",
    "VALUES (vid,CASE WHEN mode='mixed' OR mode='us' THEN ARRAY['US'] ELSE ARRAY['Worldwide'] END,CASE WHEN mode='preview' THEN 'preview' WHEN mode='none' THEN 'none' ELSE 'full' END,'none',FALSE)");
  replace('  PERFORM music_refresh_validation_flags(vid);', `
  UPDATE music_asset SET technical_metadata='{"bitrate_kbps":192,"loudness_lufs":-14.5,"object_key":"never-public","uploader":"private","nested":{"secret":true}}' WHERE release_version_id=vid AND asset_role='stream_audio';
  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at)
    VALUES (vid,rec,master,'stream_audio','local_private','standard','music-private','derivatives/'||vid||'/low.m4a','audio/mp4',32000,encode(sha256((vid||'low')::bytea),'hex'),'ready',TRUE,'{"bitrate_kbps":"96","loudness_lufs":false,"sensitive":"excluded"}',actor,NOW());
  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at)
    VALUES (vid,rec,master,'preview_audio','local_private','standard','music-private','derivatives/'||vid||'/preview.m4a','audio/mp4',16000,encode(sha256((vid||'preview')::bytea),'hex'),'ready',TRUE,jsonb_build_object('preview',music_preview_spec(180000,NULL,NULL)),actor,NOW()),
      (vid,rec,master,'preview_audio','local_private','standard','music-private','derivatives/'||vid||'/stale.m4a','audio/mp4',8000,encode(sha256((vid||'stale')::bytea),'hex'),'ready',TRUE,'{"preview":{"startMs":0,"durationMs":1234,"selection":"explicit"}}',actor,NOW()),
      (vid,rec,master,'stream_audio','local_private','standard','music-private','derivatives/'||vid||'/pending.m4a','audio/mp4',8000,encode(sha256((vid||'pending')::bytea),'hex'),'processing',FALSE,'{}',actor,NULL);
  IF mode='album' OR mode='mixed' THEN
    UPDATE music_availability_rule SET release_track_id=(SELECT id FROM music_release_track WHERE release_version_id=vid AND recording_id=rec) WHERE release_version_id=vid;
    INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('B Canción nocturna',180000,'not_explicit',actor) RETURNING id INTO rec2;
    INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist) VALUES (vid,rec2,2,'Guest Artist');
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
  IF mode='blank' THEN UPDATE music_release_track SET display_artist='' WHERE release_version_id=vid; END IF;
  IF mode='missing' THEN
    UPDATE music_asset SET processing_state='processing',ready_at=NULL WHERE release_version_id=vid AND asset_role IN ('stream_audio','preview_audio');
    PERFORM music_refresh_validation_flags(vid);
    RETURN QUERY SELECT rid,vid;
    RETURN;
  END IF;
  PERFORM music_refresh_validation_flags(vid);`);
  sql("INSERT INTO party(display_name,is_org,created_at) VALUES ('Music publication spec',FALSE,NOW());\n" + fixture);
  const make = (slug, title, mode = 'full', reuse = null) => {
    const [release, version] = sql(`SELECT release_id,version_id FROM music_track_search_publishable(${quote(slug)},${quote(title)},${quote(mode)},${quote(reuse)}::uuid)`).split('|');
    const recording = sql(`SELECT recording_id FROM music_release_track WHERE release_version_id='${version}' AND track_number=1`);
    return { release, version, recording };
  };
  const publish = item => {
    sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='${item.version}'; SELECT pg_sleep(0.01);`);
    sql('SELECT music_publish_due(100)');
    assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${item.version}'`), 'published', 'Fixture follows actual publication state machine');
  };
  const album = make('album', 'A opening', 'album'); publish(album);
  const b = sql(`SELECT recording_id FROM music_release_track WHERE release_version_id='${album.version}' AND track_number=2`);
  same(query('cancion noct'), [b], 'Only matching B is returned, never first track A');
  same(query('Guest Artist'), [b], 'Track display artist is searchable');
  assert.equal(query('Spec album').length, 2, 'Release title matches both authorized tracks');
  for (const q of ['Canción', 'CANCION', 'nocturna', 'B Canción']) same(query(q), [b], 'Accent, case and partial search');
  console.log('PASS recording selection: matching B identity, track/release/artist metadata, accent/case/partial');

  const mixed = make('mixed', 'US hidden sentinel', 'mixed'); publish(mixed);
  for (const q of ['US hidden sentinel', 'Spec mixed', 'Spec Artist']) {
    assert(!query(q).some(row => row.recordingId === mixed.recording), 'Hidden title stays absent even when release/artist matches');
  }
  same(query('US hidden sentinel', 'US'), [mixed.recording], 'Authorized territory positive');
  same(query('US hidden sentinel', null), [], 'Unknown territory fails closed');
  assert.equal(query('US hidden sentinel', 'EC', 100, 0,
    x => x.replace(gate, '(?::text IS NULL OR TRUE)')).length, 1, 'Removing delivery policy reveals hidden track: negative control detected');
  const privateItem = make('private', 'Private sentinel');
  const revoked = make('revoked', 'Revoked sentinel'); publish(revoked);
  sql(`UPDATE music_release_version SET state='suspended' WHERE id='${revoked.version}'; UPDATE music_release_version SET state='withdrawn' WHERE id='${revoked.version}';`);
  const us = make('us', 'US release sentinel', 'us'); publish(us);
  for (const item of [privateItem, revoked, us]) assert(!query('Spec Artist').some(row => row.recordingId === item.recording), 'Private/withdrawn/territory unavailable hidden through broad artist match');
  same(query('Private sentinel'), [], 'Private publication absent');
  same(query('Revoked sentinel'), [], 'Revoked publication absent');
  const denied = make('none', 'Listening denied sentinel', 'none'); publish(denied);
  same(query('Listening denied sentinel'), [], 'Published metadata without listening rights never yields a hit');
  assert(!query('Spec Artist').some(row => row.recordingId === denied.recording));
  const missing = make('missing', 'Missing rendition sentinel', 'missing');
  assert(Number(sql(`SELECT count(*) FROM music_check_submission('${missing.version}') WHERE error_code='track_assets_missing'`)) > 0,
    'Real submission policy detects missing ready streaming rendition without trigger bypass');
  same(query('Missing rendition sentinel'), [], 'Unpublishable missing-ready-rendition asset excluded');
  console.log('PASS authorization: private, withdrawn, per-track territory, denied listening and broad metadata match; missing rendition rejected by real publication check; removal mutant detected');

  assert.throws(() => make('blank', 'Blank artist sentinel', 'blank'),
    error => String(error.stderr).includes('music_release_track_display_artist_check'),
    'Actual schema rejects blank track attribution; defensive fallback is not tested through impossible data');
  const preview = make('preview', 'Preview sentinel', 'preview'); publish(preview);
  const previewHit = query('Preview sentinel')[0];
  assert.deepEqual(previewHit.sources.map(row => row.role), ['preview_audio'], 'Preview-only result never contains full rendition');
  const fullHit = query('A opening')[0];
  assert.deepEqual(fullHit.sources.map(row => row.role).sort(), ['preview_audio', 'stream_audio', 'stream_audio']);
  assert.equal(new Set(fullHit.sources.map(row => row.assetId)).size, 3, 'Distinct authorized renditions once each');
  const expectedKeys = ['trackId','recordingId','releaseId','releaseVersionId','releaseSlug','releaseTitle','title','displayArtist','durationMs','sources'].sort();
  for (const hit of query('Spec Artist')) {
    assert.deepEqual(Object.keys(hit).sort(), expectedKeys, 'Minimal metadata DTO');
    for (const asset of hit.sources) {
      assert.deepEqual(Object.keys(asset).sort(), ['assetId','role','mediaType','technicalMetadata'].sort());
      for (const [key, value] of Object.entries(asset.technicalMetadata)) {
        assert(['bitrate_kbps','loudness_lufs'].includes(key)); assert.equal(typeof value,'number');
      }
      assert.equal(sql(`SELECT music_public_asset_accessible('${asset.assetId}','EC')`), 't');
      assert.equal(sql(`SELECT count(*) FROM music_asset WHERE id='${asset.assetId}' AND release_version_id='${hit.releaseVersionId}' AND recording_id='${hit.recordingId}'`), '1', 'Asset uses exact selected context');
    }
  }
  assert(fullHit.sources.some(row => row.technicalMetadata.bitrate_kbps === 192));
  assert(fullHit.sources.some(row => Object.keys(row.technicalMetadata).length === 0), 'Wrong-type metadata is omitted');
  console.log('PASS rendition/metadata: ready full/current preview only; master, pending and stale preview absent; allowlisted numeric-only metadata and exact context');

  const duplicate = make('duplicate-context', 'A opening', 'full', album.recording); publish(duplicate);
  same(query('A opening'), [album.recording], 'Canonical recording deduplicated across release contexts');
  assert.equal(query('A opening')[0].releaseId, duplicate.release, 'Newest authorized matching context selected');
  assert.equal(query('Spec album').find(row => row.recordingId === album.recording).releaseId, album.release, 'Choose matching context rather than a nonmatching newer release');
  const all = query('Spec Artist');
  assert.equal(new Set(all.map(row => row.recordingId)).size, all.length, 'No recording duplicates from renditions/contexts');
  const pages = all.map((_, offset) => query('Spec Artist', 'EC', 1, offset)[0]);
  assert.deepEqual(pages, all, 'Stable offset pages equal full deduplicated ordering');
  assert.deepEqual(query('Spec Artist', 'EC', 1, all.length), [], 'End page empty');
  assert.deepEqual(query('Spec Artist'), all, 'Repeated query deterministic');
  const unicode = make('unicode', '夜の波'); publish(unicode);
  same(query('夜の'), [unicode.recording], 'Non-Latin literal partial');
  same(query('不存在'), [], 'Non-Latin query never becomes match-all');
  same(query('%_'), [], 'SQL wildcard characters remain literal');
  const literal = make('literal', 'Mix 100%_ready'); publish(literal);
  same(query('%_'), [literal.recording], 'Literal wildcard-containing title remains searchable without matching other records');
  same(query("'; DROP TABLE music_recording; --"), [], 'Parameter text cannot alter SQL');
  console.log('PASS canonical deduplication: multiple matching contexts and quality variants, stable pages, non-Latin/literal/injection controls');
  console.log(`PASS real PostgreSQL track search: ${migrations.length} selected migrations plus actual directory normalizer; no HTTP/storage/full-schema claim`);
} finally {
  await owner.cleanup();
  console.log('Cleaned only this run’s owned disposable database');
}
