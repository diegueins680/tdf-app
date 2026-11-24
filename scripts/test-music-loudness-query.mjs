// MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md).
// Runs the query the handler runs, extracted from the Haskell source, on real
// PostgreSQL with the real migrations. It shows which master file and which
// measurement row the pure rule (TDF.MusicRelease.Loudness.trackCheck) is
// given; the rule itself is covered by LoudnessSpec. A test, not a proof.
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { tmpdir, userInfo } from 'node:os';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase, musicTestEnvironment } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const runtime = mkdtempSync(join(tmpdir(), 'tdf-music-loudness-query-'));
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const env = musicTestEnvironment({ source: process.env, username: userInfo().username, runtime });

function command(program, args, options = {}) {
  const result = spawnSync(program, args, { env, encoding: 'utf8', timeout: 120000, ...options });
  assert.equal(result.error, undefined, `${program}: ${result.error}`);
  assert.equal(result.status, 0, `${program}: ${result.stderr}\n${result.stdout}`);
  return result.stdout.trim();
}
const sql = (query) => command('psql', ['-XAtq', '-F', '|', '-v', 'ON_ERROR_STOP=1', '-d', database, '-c', query]);

// The SQL is the Haskell string literal, with its line gaps removed.
function handlerSql() {
  const source = readFileSync(join(root, 'tdf-hq/src/TDF/MusicRelease/Loudness.hs'), 'utf8');
  const start = source.indexOf('loudnessTracksSql =');
  assert.ok(start > 0, 'loudnessTracksSql is defined');
  const open = source.indexOf('"', start);
  const literal = source.slice(open + 1).replace(/\\\n\s*\\/g, '');
  const close = literal.indexOf('"');
  const text = literal.slice(0, close);
  assert.ok(!text.includes('\\'), 'no other escapes in the query literal');
  assert.equal(text.split('?').length, 2, 'exactly one parameter');
  return text;
}

const testDatabase = disposableMusicDatabase({ database, command, env });
try {
  testDatabase.create();
  for (const file of ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql', '2026-09-16_music_ddex_operations.sql',
    '2026-10-12_music_upload_rebind.sql', '2026-11-21_music_audio_measurement.sql',
    '2026-11-21_music_audio_measurement.sql']) {
    command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', join(root, 'tdf-hq/sql', file)]);
  }
  const artist = sql("INSERT INTO party(display_name) VALUES ('Synthetic loudness artist') RETURNING id");
  const release = randomUUID(), version = randomUUID(), otherVersion = randomUUID();
  sql(`INSERT INTO music_release(id,artist_party_id,canonical_slug,release_kind,created_by)
    VALUES ('${release}',${artist},'loudness-${release}','album',${artist});
    INSERT INTO music_release_version(id,release_id,version_number,title,display_artist,created_by)
    VALUES ('${version}','${release}',1,'Synthetic','Synthetic',${artist}),
           ('${otherVersion}','${release}',2,'Synthetic','Synthetic',${artist});`);
  let seed = 0;
  const hash = () => (++seed).toString(16).padStart(64, '0');
  function track(number, targetVersion = version) {
    const recording = randomUUID();
    sql(`INSERT INTO music_recording(id,canonical_title,created_by) VALUES ('${recording}','Track ${number}',${artist});
      INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist)
      VALUES ('${targetVersion}','${recording}',${number},'Synthetic');`);
    return recording;
  }
  function master(recording, { state = 'valid', immutable = true, age = 0, targetVersion = version } = {}) {
    const id = randomUUID(), sha = hash();
    sql(`INSERT INTO music_asset(id,release_version_id,recording_id,asset_role,storage_provider,storage_class,
        bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,created_at)
      VALUES ('${id}','${targetVersion}','${recording}','master_audio','s3_compatible','standard','bucket','${id}/original',
        'audio/wav',10,'${sha}','${state}',${immutable},${artist},NOW()-INTERVAL '${age} minutes')`);
    return { id, sha };
  }
  function measured(asset, integrated, peak) {
    const id = sql(`SELECT music_begin_audio_measurement('${asset.id}','${asset.sha}','loudness','ffmpeg-ebur128-v1',NULL,NULL)`);
    assert.equal(sql(`SELECT music_finish_audio_measurement('${id}',NULL,NULL,
      '{"state":"measured","integratedLufsTenths":${integrated},"truePeakDbtpTenths":${peak}}')`), 't');
    return id;
  }
  function failed(asset, reason) {
    const id = sql(`SELECT music_begin_audio_measurement('${asset.id}','${asset.sha}','loudness','ffmpeg-ebur128-v1',NULL,NULL)`);
    assert.equal(sql(`SELECT music_finish_audio_measurement('${id}',NULL,NULL,'{"state":"failed","reason":"${reason}"}')`), 't');
  }

  // Track 1: measured and usable.
  const r1 = track(1); const m1 = master(r1); measured(m1, -142, -13);
  // Track 2: an older measured master and a newer upload not inspected yet.
  const r2 = track(2); const old2 = master(r2, { age: 10 }); measured(old2, -100, 5);
  const new2 = master(r2, { state: 'uploaded', immutable: false });
  // Track 3: no master at all.
  const r3 = track(3);
  // Track 4: measured, then measured again and failed: only the open row counts.
  const r4 = track(4); const m4 = master(r4); measured(m4, -150, -20); failed(m4, 'timeout');
  // Track 5: the newest master was deleted; the previous one is the file.
  const r5 = track(5); const kept5 = master(r5, { age: 10 }); measured(kept5, -139, -11);
  master(r5, { state: 'deleted', immutable: false });
  // Track 6: an attempt in progress.
  const r6 = track(6); const m6 = master(r6);
  sql(`SELECT music_begin_audio_measurement('${m6.id}','${m6.sha}','loudness','ffmpeg-ebur128-v1',NULL,NULL)`);
  // Track 7: the same recording in another version has its own measured file.
  const r7 = track(7); const m7 = master(r7, { state: 'uploaded', immutable: false });
  sql(`INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist)
    VALUES ('${otherVersion}','${r7}',1,'Synthetic')`);
  const other7 = master(r7, { targetVersion: otherVersion }); measured(other7, -141, -12);

  const rows = sql(handlerSql().replace('?', `'${version}'`)).split('\n').map((line) => line.split('|'));
  assert.equal(rows.length, 7, 'one row per track, never more');
  const expected = [
    [r1, m1.id, m1.sha, 't', m1.id, m1.sha, 'measured', '-142', '-13', ''],
    [r2, new2.id, new2.sha, 'f', '', '', '', '', '', ''],
    [r3, '', '', 'f', '', '', '', '', '', ''],
    [r4, m4.id, m4.sha, 't', m4.id, m4.sha, 'failed', '', '', 'timeout'],
    [r5, kept5.id, kept5.sha, 't', kept5.id, kept5.sha, 'measured', '-139', '-11', ''],
    [r6, m6.id, m6.sha, 't', m6.id, m6.sha, 'pending', '', '', ''],
    [r7, m7.id, m7.sha, 'f', '', '', '', '', '', ''],
  ];
  assert.deepEqual(rows, expected);
  console.log('PASS the handler query returns the newest undeleted master and only its open measurement, per track and version');

  const others = sql(handlerSql().replace('?', `'${otherVersion}'`)).split('\n').map((line) => line.split('|'));
  assert.deepEqual(others, [[r7, other7.id, other7.sha, 't', other7.id, other7.sha, 'measured', '-141', '-12', '']]);
  console.log('PASS a measurement of another version of the recording is not read');

  assert.equal(sql(`SELECT count(*) FROM music_audio_measurement WHERE asset_id='${m4.id}'`), '2');
  assert.equal(sql(`SELECT string_agg(state,',' ORDER BY created_at,id) FROM music_audio_measurement WHERE asset_id='${m4.id}'`),
    'superseded,failed');
  assert.equal(sql(`SELECT count(*) FROM (SELECT asset_id FROM music_audio_measurement WHERE state<>'superseded'
    GROUP BY asset_id,kind HAVING count(*)>1) duplicated`), '0');
  console.log('PASS at most one open row per asset and kind');

  command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f',
    join(root, 'tdf-hq/sql/2026-11-21_music_audio_measurement_rollback.sql')]);
  assert.equal(sql("SELECT to_regclass('music_audio_measurement') IS NULL"), 't');
  assert.equal(sql("SELECT count(*) FROM revenue_feature_flag WHERE flag_key='music_releases.loudness_check'"), '0');
  assert.equal(sql("SELECT count(*) FROM pg_proc WHERE proname LIKE 'music_%audio_measurement%' OR proname='music_asset_supersede_measurements'"), '0');
  command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f',
    join(root, 'tdf-hq/sql/2026-11-21_music_audio_measurement.sql')]);
  assert.equal(sql("SELECT count(*) FROM revenue_feature_flag WHERE flag_key='music_releases.loudness_check' AND NOT enabled"), '2');
  console.log('PASS the migration applies twice, reverses cleanly and applies again with the flag off');
} finally {
  try { await testDatabase.cleanup(); } finally { rmSync(runtime, { recursive: true, force: true }); }
}
