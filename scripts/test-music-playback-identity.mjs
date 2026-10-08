// Focused real PostgreSQL regression; independent of Docker/storage providers.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
assert(['127.0.0.1', 'localhost'].includes(process.env.PGHOST ?? '127.0.0.1'), 'Loopback PostgreSQL only');
for (const key of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) {
  assert(!process.env[key], `Refusing alternate libpq routing: ${key}`);
}
assert.match(process.env.PGPORT ?? '5432', /^\d+$/);
const env = { ...process.env, PGHOST: '127.0.0.1', PGCONNECT_TIMEOUT: '5',
  PGOPTIONS: '-c statement_timeout=15000 -c lock_timeout=10000' };
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const command = (program, args, options = {}) => execFileSync(program, args, {
  env, encoding: 'utf8', timeout: 30000, maxBuffer: 8 * 1024 * 1024, ...options,
}).trim();
const owner = disposableMusicDatabase({ database, command, env });
const psql = args => command('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database, ...args]);
const sql = query => psql(['-c', query]);
const apply = name => psql(['-f', join(root, 'tdf-hq/sql', name)]);
const definition = () => sql("SELECT md5(pg_get_functiondef('music_record_playback_event(uuid,uuid,integer,bigint,text,uuid,uuid,text,bigint,bigint,text,text,timestamp with time zone,jsonb)'::regprocedure))");
const migration = '2026-09-16_music_playback_identity.sql';
const rollback = '2026-09-16_music_playback_identity_rollback.sql';
try {
  owner.create();
  for (const name of ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql']) apply(name);
  const original = definition();
  apply(migration); apply(migration); apply(rollback); apply(rollback);
  assert.equal(definition(), original, 'Empty rollback must restore exact function');
  console.log('PASS playback migration: repeated apply and empty rollback');

  const actor = sql("INSERT INTO party(display_name) VALUES ('Synthetic playback actor') RETURNING id");
  const other = sql("INSERT INTO party(display_name) VALUES ('Synthetic other actor') RETURNING id");
  const release = sql(`INSERT INTO music_release(artist_party_id,canonical_slug,release_kind,created_by)
    VALUES (${actor},'synthetic-playback','single',${actor}) RETURNING id`);
  const version = sql(`INSERT INTO music_release_version(release_id,version_number,title,display_artist,
    explicit_content,recording_copyright_text,work_copyright_text,created_by)
    VALUES ('${release}',1,'Synthetic playback','Synthetic actor','not_explicit','Synthetic','Synthetic',${actor}) RETURNING id`);
  const recording = sql(`INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by)
    VALUES ('Synthetic playback',180000,'not_explicit',${actor}) RETURNING id`);
  apply(migration); apply(migration);
  const fixture = () => psql(['-v', `version_id=${version}`, '-v', `recording_id=${recording}`,
    '-v', `actor_id=${actor}`, '-v', `other_actor_id=${other}`,
    '-f', join(root, 'tdf-hq/test/sql/music_playback_identity.sql')]);
  fixture();
  console.log('PASS playback identity: owner, strict replay, sequence, anonymous and legacy diagnosis');

  const event = randomUUID();
  assert.equal(sql(`SELECT music_record_playback_event('${event}','${randomUUID()}',0,${actor},NULL,
    '${version}','${recording}','play_start',0,0,NULL,NULL,NOW(),'{}')`), 'inserted');
  const evidence = () => sql(`SELECT row_to_json(e) FROM music_playback_event e WHERE event_id='${event}'`);
  const before = evidence();
  apply(rollback);
  assert.equal(definition(), original);
  assert.equal(evidence(), before, 'Populated rollback must preserve event');
  assert.equal(sql("SELECT to_regclass('music_playback_session_sanitation') IS NULL"), 't');
  apply(migration); fixture();
  assert.equal(evidence(), before, 'Reapplication must preserve event');
  console.log('PASS playback migration: exact populated rollback, reapply and immutable event');
} finally {
  await owner.cleanup();
  console.log('Cleaned only this run’s disposable PostgreSQL database');
}
