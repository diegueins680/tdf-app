// Executes actual detail/library SQL against the real delivery-policy function.
// Owns one random disposable database with the complete canonical schema.
// No HTTP/storage/production assertions; never relax the shared owner/URL guards.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
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
const psqlArgs = ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database];
const sql = input => command('psql', psqlArgs, { input });
const quote = value => value === null ? 'NULL' : typeof value === 'number' ? String(value)
  : `'${value.replaceAll("'", "''")}'`;
const source = readFileSync(join(root, 'tdf-hq/src/TDF/Server/MusicRelease.hs'), 'utf8');
const names = ['getPublicRelease', 'listFavorites', 'listPlaylists', 'listPlaybackHistory'];
const nextNames = ['listMyReleases', 'addFavorite', 'createPlaylist', 'createInfringementReport'];
const gate = 'music_public_asset_accessible(asset.id,?::text)';
const validateBindings = (name, statement, bindings) => {
  const expected = name === 'getPublicRelease'
    ? ['PersistText slug', 'optionalText territory', 'optionalText territory', 'optionalText territory']
    : ['optionalText territory', 'optionalText territory', 'optionalText territory', 'PersistInt64 (currentPartyId user)'];
  if (statement.includes(gate)) expected.unshift('optionalText territory');
  assert.deepEqual(bindings, expected, `${name}: actual Haskell binding order must match its SQL`);
};
const statements = Object.fromEntries(names.map((name, index) => {
  const block = source.split(`${name} ::`)[1].split(`\n${nextNames[index]} ::`)[0];
  const gapless = block.replace(/\\\n\s*\\/g, '');
  const match = gapless.match(/(?:jsonOne err404|jsonRows)\s+("(?:[^"\\]|\\.)*")/s);
  assert(match, `Locate actual ${name} SQL`);
  const statement = JSON.parse(match[1]);
  const list = gapless.slice(match.index + match[0].length).match(/^\s*\[([^\]]*)\]/s);
  assert(list, `Locate actual ${name} Haskell parameter list`);
  // Trusted lexical translation: these four handlers bind only the whitelisted
  // optionalText/PersistText/PersistInt64 expressions below. Reject other forms.
  const bindings = list[1].split(',').map(item => item.trim().replace(/\s+/g, ' '));
  validateBindings(name, statement, bindings);
  assert.throws(() => validateBindings(name, statement, bindings.slice(1)), assert.AssertionError,
    `${name}: detect an omitted Haskell argument`);
  if (statement.includes(gate)) {
    const changed = [...bindings];
    const ownerIndex = name === 'getPublicRelease' ? 1 : changed.length - 1;
    [changed[0], changed[ownerIndex]] = [changed[ownerIndex], changed[0]];
    assert.throws(() => validateBindings(name, statement, changed), assert.AssertionError,
      `${name}: detect reordered territory and slug/owner arguments`);
  }
  return [name, { statement, bindings }];
}));
const query = (name, territory, actor, slug, removeGate = false) => {
  const { statement: shipped, bindings } = statements[name];
  const values = { 'optionalText territory': territory, 'PersistText slug': slug,
    'PersistInt64 (currentPartyId user)': actor };
  const params = bindings.map(binding => {
    assert(Object.hasOwn(values, binding), `${name}: unsupported Haskell binding ${binding}`);
    return values[binding];
  });
  let index = 0;
  const statement = removeGate ? shipped.replace(gate, '(?::text IS NULL OR TRUE)') : shipped;
  const rendered = statement.replaceAll('?', () => quote(params[index++]));
  assert.equal(index, params.length, `${name} positional parameter contract`);
  return sql(rendered).split('\n').filter(Boolean).map(line => JSON.parse(line));
};
const failures = [];
const check = (condition, label) => { if (!condition) { failures.push(label); console.log(`FAIL: ${label}`); } };

try {
  owner.create();
  // Use the same immutable production fixtures and canonical batch renderer as
  // the complete integration rehearsal. This database remains owned solely by
  // disposableMusicDatabase; do not broaden its name or connection guards.
  for (const relative of ['scripts/__tests__/fixtures/production-schema-20260814.sql',
    'scripts/__tests__/fixtures/catalog-production-source-fixture.sql']) {
    command('psql', [...psqlArgs, '-f', join(root, relative)]);
  }
  const sourceRevision = command('git', ['rev-parse', 'HEAD'], { cwd: root });
  const batch = command(process.execPath, ['scripts/render-production-migration-batch.mjs'], {
    cwd: root, env: { ...env, SOURCE_COMMIT: sourceRevision }, maxBuffer: 64 * 1024 * 1024,
  });
  const manifest = JSON.parse(readFileSync(join(root, 'scripts/production-migrations.json'), 'utf8'));
  sql(batch);
  assert.equal(Number(sql('SELECT count(*) FROM tdf_schema_migration')), manifest.migrations.length,
    'Apply the complete canonical migration registry');
  const ledgerQuery = 'SELECT jsonb_agg(to_jsonb(m) ORDER BY migration_id) FROM tdf_schema_migration m';
  const ledgerBefore = sql(ledgerQuery);
  sql(batch);
  assert.equal(sql(ledgerQuery), ledgerBefore, 'Canonical replay cannot rewrite migration history');
  const policyQuery = "SELECT pg_get_functiondef('music_public_asset_accessible(uuid,text)'::regprocedure)";
  const policyBefore = sql(policyQuery);
  console.log(`PASS schema: complete ${manifest.migrations.length}-entry canonical apply/replay`);
  // Preserve the real submission/approval fixture and add valid preview assets
  // before it becomes immutable. No publication/immutability trigger bypass.
  const fixtureSource = readFileSync(join(root, 'tdf-hq/test/TDF/MusicPublicationSpec.hs'), 'utf8');
  const fixtureList = fixtureSource.split('[ "CREATE OR REPLACE FUNCTION music_spec_publishable')[1].split(']) []')[0];
  const fixtureLiterals = ('"CREATE OR REPLACE FUNCTION music_spec_publishable' + fixtureList)
    .match(/"(?:[^"\\]|\\.)*"/g);
  let fixture = fixtureLiterals.map(value => JSON.parse(value)).join('\n');
  fixture = fixture.replace('music_spec_publishable(slug TEXT)', 'music_source_publishable(slug TEXT, mode TEXT)')
    .replace('cover UUID;', 'cover UUID; rec2 UUID; master2 UUID; mr2 UUID; cr2 UUID;')
    .replace("VALUES (vid,ARRAY['Worldwide'],'full','none',FALSE)", "VALUES (vid,CASE WHEN mode='mixed' THEN ARRAY['US'] ELSE ARRAY['Worldwide'] END,CASE WHEN mode='preview' THEN 'preview' ELSE 'full' END,'none',FALSE)")
    .replace('  PERFORM music_refresh_validation_flags(vid);', `
  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at)
    VALUES (vid,rec,master,'preview_audio','local_private','standard','music-private','derivatives/'||vid||'/preview.m4a','audio/mp4',16000,encode(sha256((vid||'preview')::bytea),'hex'),'ready',TRUE,jsonb_build_object('preview',music_preview_spec(180000,NULL,NULL)),actor,NOW());
  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at)
    VALUES (vid,rec,master,'preview_audio','local_private','standard','music-private','derivatives/'||vid||'/old-preview.m4a','audio/mp4',8000,encode(sha256((vid||'old-preview')::bytea),'hex'),'ready',TRUE,'{"preview":{"startMs":0,"durationMs":1234,"selection":"explicit"}}',actor,NOW());
  IF mode='mixed' THEN
    UPDATE music_availability_rule SET release_track_id=(SELECT id FROM music_release_track WHERE release_version_id=vid AND recording_id=rec) WHERE release_version_id=vid;
    INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('EC companion',180000,'not_explicit',actor) RETURNING id INTO rec2;
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
  const actor = Number(sql("SELECT id FROM party WHERE display_name='Music publication spec'"));
  const other = Number(sql("INSERT INTO party(display_name) VALUES ('Other listener') RETURNING id"));
  const make = mode => {
    const [release, version] = sql(`SELECT release_id,version_id FROM music_source_publishable(${quote(mode)},${quote(mode)})`).split('|');
    sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='${version}'; SELECT pg_sleep(0.01);`);
    sql('SELECT music_publish_due(100)');
    const slug = sql(`SELECT canonical_slug FROM music_release WHERE id='${release}'`);
    return { release, version, slug };
  };
  const preview = make('preview'), full = make('full'), mixed = make('mixed');
  const playlist = sql(`INSERT INTO music_playlist(owner_party_id,name,visibility) VALUES (${actor},'Saved sources','private') RETURNING id`);
  for (const [index, item] of [preview, full, mixed].entries()) {
    sql(`INSERT INTO music_favorite(party_id,recording_id) SELECT ${actor},recording_id FROM music_release_track WHERE release_version_id='${item.version}';
      INSERT INTO music_playlist_item(playlist_id,recording_id,position,added_by) SELECT '${playlist}',recording_id,${index}*10+track_number,${actor} FROM music_release_track WHERE release_version_id='${item.version}';
      INSERT INTO music_playback_history(party_id,recording_id,last_release_version_id) SELECT ${actor},recording_id,'${item.version}' FROM music_release_track WHERE release_version_id='${item.version}';`);
  }
  const entries = (name, item, territory = 'EC') => {
    const rows = query(name, territory, actor, item.slug);
    return name === 'getPublicRelease' ? rows[0].tracks : name === 'listPlaylists' ? rows[0].items : rows;
  };
  for (const name of names) {
    const previewTrack = entries(name, preview).find(track => track.title === 'Spec preview');
    check(previewTrack.sources.length === 1 && previewTrack.sources[0].role === 'preview_audio', `${name}: preview-only listener must receive no denied full rendition`);
    const fullTrack = entries(name, full).find(track => track.title === 'Spec full');
    assert.deepEqual(fullTrack.sources.map(item => item.role).sort(), ['preview_audio', 'stream_audio'], `${name}: retain authorized full and current preview`);
    const unavailableTrack = entries(name, mixed).find(track => track.title === 'Spec mixed');
    check(unavailableTrack.sources.length === 0, `${name}: EC must receive no US-only sources`);
    assert(unavailableTrack.recordingId && unavailableTrack.title === 'Spec mixed', `${name}: retain saved/public metadata identity`);
    const usTrack = entries(name, mixed, 'US').find(track => track.title === 'Spec mixed');
    assert.deepEqual(usTrack.sources.map(item => item.role).sort(), ['preview_audio', 'stream_audio'], `${name}: US positive control`);
    for (const item of entries(name, mixed)) {
      for (const rendition of item.sources) {
        check(sql(`SELECT music_public_asset_accessible(${quote(rendition.assetId)},'EC')`) === 't', `${name}: every emitted source agrees with delivery policy`);
        assert(!('url' in rendition), `${name}: metadata projection issues no credentials`);
      }
    }
    if (statements[name].statement.includes(gate)) {
      const rows = query(name, 'EC', actor, preview.slug, true);
      const tracks = name === 'getPublicRelease' ? rows[0].tracks : name === 'listPlaylists' ? rows[0].items : rows;
      assert(tracks.find(track => track.title === 'Spec preview').sources.some(item => item.role === 'stream_audio'), `${name}: removing only projection policy exposes denied full source`);
      console.log(`PASS mutant: ${name} removal of source authorization is detected`);
    }
    console.log(`CHECKED controls: ${name} authorized full/preview, stale-preview exclusion, territorial positive, metadata identity, delivery agreement`);
  }
  for (const name of names.slice(1)) assert.deepEqual(query(name, 'EC', other, preview.slug), [], `${name}: other account receives no saved rows`);
  assert.deepEqual(query('getPublicRelease', null, actor, mixed.slug), [], 'Unknown territory cannot discover a territory-specific release');
  for (const name of names.slice(1)) {
    const track = entries(name, mixed, null).find(item => item.title === 'Spec mixed');
    assert.equal(track.sources.length, 0, `${name}: unknown territory never grants US access`);
  }
  sql(`UPDATE music_release_version SET state='suspended' WHERE id='${preview.version}'; UPDATE music_release_version SET state='withdrawn' WHERE id='${preview.version}';`);
  assert.deepEqual(query('getPublicRelease', 'EC', actor, preview.slug), [], 'Withdrawn detail is unavailable');
  for (const name of names.slice(1)) {
    const track = entries(name, preview).find(item => item.title === 'Spec preview');
    assert.equal(track.sources.length, 0, `${name}: withdrawn saved entry has no sources`);
    assert.equal(track.available, false, `${name}: preserve unavailable saved row`);
  }
  console.log('PASS controls: account isolation, unknown territory and withdrawal preserve existing metadata/row semantics');
  assert.deepEqual(failures, [], 'Source arrays must agree with authorized delivery for the requesting territory');
  assert.equal(sql(policyQuery), policyBefore, 'Fixtures cannot replace the real delivery-policy function');
  sql(batch);
  assert.equal(sql(ledgerQuery), ledgerBefore, 'Final readiness retains exact migration history');
  console.log(JSON.stringify({ kind: 'music-source-projection-qualification', sourceRevision,
    migrations: manifest.migrations.length, surfaces: names.length, policyRemovalControls: names.length,
    bindingArgumentControls: names.length * 2,
    policySha256: createHash('sha256').update(policyBefore).digest('hex'),
    handlerSha256: createHash('sha256').update(source).digest('hex'),
    scope: 'Complete canonical schema and finite real SQL/lexical controls; not compiled HTTP, bytes, performance or production acceptance',
  }));
  console.log('PASS: all four shipped source projections exclude unauthorized rendition references');
} finally {
  await owner.cleanup();
  console.log('Cleaned only this run’s owned disposable database');
}
