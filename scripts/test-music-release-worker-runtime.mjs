// Real PostgreSQL migrations, worker, FFmpeg and files; only curl is replaced
// with an explicit fault injector. No provider credentials or network S3 calls.
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { spawn, spawnSync } from 'node:child_process';
import { setTimeout } from 'node:timers/promises';
import { chmodSync, copyFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync,
  rmSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { tmpdir, userInfo } from 'node:os';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase } from './lib/music-test-database.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const runtime = mkdtempSync(join(tmpdir(), 'tdf-music-worker-test-'));
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const env = {
  PATH: process.env.PATH,
  LANG: 'C', LC_ALL: 'C', TMPDIR: runtime,
  PGHOST: '127.0.0.1', PGPORT: '5432', PGUSER: userInfo().username,
};
let passed = 0;
const concurrencyOnly = process.argv.includes('--concurrency-only');
const only = process.argv.find((argument) => argument.startsWith('--only='))?.slice(7);
const children = new Set();
function startWorker(extra = {}, script = 'scripts/run-music-release-worker-once.sh') {
  const child = spawn('bash', [join(root, script)], {
    env: { ...workerEnv, ...extra }, stdio: ['ignore', 'pipe', 'pipe'],
  });
  children.add(child);
  let stdout = '', stderr = '';
  child.stdout.on('data', (data) => { stdout += data; });
  child.stderr.on('data', (data) => { stderr += data; });
  const done = new Promise((resolve, reject) => {
    child.on('error', reject);
    child.on('close', (status, signal) => {
      children.delete(child);
      resolve({ status, signal, stdout, stderr });
    });
  });
  return { child, done, snapshot: () => ({ stdout, stderr }) };
}
async function until(predicate, label, milliseconds = 15000) {
  const deadline = Date.now() + milliseconds;
  while (!predicate()) {
    assert.ok(Date.now() < deadline, `Timed out: ${label}`);
    await setTimeout(100);
  }
}
async function completed(workerProcess) {
  let timer;
  try {
    return await Promise.race([workerProcess.done, new Promise((_, reject) => {
      timer = globalThis.setTimeout(() => reject(new Error('Worker did not stop within 30s')), 30000);
    })]);
  } finally { clearTimeout(timer); }
}
async function waitForGate(workerProcess, gate, label, milliseconds = 45000) {
  try {
    await until(() => {
      assert.equal(workerProcess.child.exitCode, null, 'Worker exited before reaching the gate');
      assert.equal(workerProcess.child.signalCode, null, 'Worker was signalled before reaching the gate');
      return existsSync(`${gate}.started`);
    }, label, milliseconds);
  } catch (error) {
    const { stdout, stderr } = workerProcess.snapshot();
    throw new Error(`${label}: ${error.message}\n${stdout}\n${stderr}`, { cause: error });
  }
}
async function checkAsync(label, body) {
  if (only && !label.includes(only)) return;
  await body();
  passed += 1;
  console.log(`PASS ${label}`);
}
function command(program, args, options = {}) {
  const result = spawnSync(program, args, { env, encoding: 'utf8', timeout: 120000,
    maxBuffer: 8 * 1024 * 1024, ...options });
  assert.equal(result.error, undefined, `${program}: ${result.error}`);
  assert.equal(result.status, 0, `${program}: ${result.stderr}\n${result.stdout}`);
  return result.stdout.trim();
}
function sql(query) {
  return command('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-c', query]);
}
const sha = (bytes) => createHash('sha256').update(bytes).digest('hex');
const objects = join(runtime, 'objects');
const trace = join(runtime, 'trace');
const bin = join(runtime, 'bin');
mkdirSync(bin);
mkdirSync(join(objects, 'music-test-quarantine'), { recursive: true });
copyFileSync(join(root, 'test/fixtures/music-worker/curl.mjs'), join(bin, 'curl'));
chmodSync(join(bin, 'curl'), 0o755);
const workerEnv = {
  ...env, PATH: `${bin}:${env.PATH}`,
  DATABASE_URL: `postgresql://127.0.0.1:5432/${database}?user=${encodeURIComponent(env.PGUSER)}`,
  MUSIC_S3_ENDPOINT: 'https://music-worker.test.invalid',
  MUSIC_S3_ACCESS_KEY_ID: 'synthetic-worker-access',
  MUSIC_S3_SECRET_ACCESS_KEY: 'synthetic-worker-secret',
  MUSIC_S3_REGION: 'us-east-1', MUSIC_S3_MASTER_BUCKET: 'music-test-master',
  MUSIC_S3_DERIVATIVE_BUCKET: 'music-test-derivatives', MUSIC_S3_DDEX_BUCKET: 'music-test-ddex',
  MUSIC_TEST_OBJECTS: objects, MUSIC_TEST_TRACE: trace,
  MUSIC_WORKER_DIAGNOSTICS: process.env.MUSIC_WORKER_DIAGNOSTICS ?? 'false',
  // Quotes and psql metacharacters must remain data through -v substitution.
  MUSIC_WORKER_ID: "worker's :quoted \\ identity",
};
function worker(extra = {}) {
  const started = performance.now();
  const result = spawnSync('bash', [join(root, 'scripts/run-music-release-worker-once.sh')], {
    env: { ...workerEnv, ...extra }, encoding: 'utf8', timeout: 120000, maxBuffer: 1024 * 1024,
  });
  if (workerEnv.MUSIC_WORKER_DIAGNOSTICS === 'true') {
    console.log(`Worker elapsed ${Math.round(performance.now() - started)} ms`);
    console.log(result.stderr?.split('\n').filter((line) => line.startsWith('{"event":"music_worker_timing"')).join('\n'));
  }
  assert.equal(result.error, undefined, `${String(result.error)}\nWorker stdout: ${result.stdout?.slice(-4000)}\nWorker stderr: ${result.stderr?.slice(-4000)}`);
  return result;
}
function check(label, body) {
  if (concurrencyOnly) return;
  if (only && !label.includes(only)) return;
  body();
  passed += 1;
  console.log(`PASS ${label}`);
}
let artist;
function fixture(kind = 'inspect_audio', bytes = readFileSync(join(runtime, 'master.wav')), checksum = sha(bytes), existingVersion) {
  const release = randomUUID(), version = existingVersion ?? randomUUID(), recording = randomUUID(), asset = randomUUID(), job = randomUUID();
  const key = `${asset}/original`;
  const isArt = kind === 'inspect_artwork';
  mkdirSync(join(objects, 'music-test-quarantine', asset));
  writeFileSync(join(objects, 'music-test-quarantine', key), bytes);
  if (!existingVersion) sql(`INSERT INTO music_release(id,artist_party_id,canonical_slug,release_kind,created_by)
    VALUES ('${release}',${artist},'worker-${release}','single',${artist});
    INSERT INTO music_release_version(id,release_id,version_number,title,display_artist,created_by)
    VALUES ('${version}','${release}',1,'Synthetic worker release','Synthetic artist',${artist});`);
  sql(`INSERT INTO music_recording(id,canonical_title,created_by) VALUES ('${recording}','Synthetic recording',${artist});
    INSERT INTO music_asset(id,release_version_id,recording_id,asset_role,storage_provider,storage_class,
      bucket_name,object_key,media_type,byte_size,sha256,processing_state,created_by)
    VALUES ('${asset}','${version}',${isArt ? 'NULL' : `'${recording}'`},'${isArt ? 'cover_original' : 'master_audio'}',
      's3_compatible','quarantine','music-test-quarantine','${key}','${isArt ? 'image/png' : 'audio/wav'}',
      ${bytes.length},'${checksum}','uploaded',${artist});
    INSERT INTO music_processing_job(id,release_version_id,source_asset_id,job_kind,job_key,max_attempts)
    VALUES ('${job}','${version}','${asset}','${kind}','${job}',2);
    UPDATE music_release_version SET state='processing' WHERE id='${version}';`);
  writeFileSync(trace, '');
  return { version, recording, asset, job, key, bytes, checksum };
}
function jobState(f) {
  return sql(`SELECT status||':'||attempt_count||':'||(locked_by IS NULL)::text
    FROM music_processing_job WHERE id='${f.job}'`);
}
function retryNow(f) {
  sql(`UPDATE music_processing_job SET run_after=NOW()-INTERVAL '1 second' WHERE id='${f.job}'`);
}
function cancel(f) {
  sql(`UPDATE music_processing_job SET status='cancelled' WHERE id='${f.job}'`);
}
function assertFailure(f, extra) {
  const result = worker(extra);
  assert.notEqual(result.status, 0, result.stdout);
  assert.doesNotMatch(result.stdout, /job .* succeeded/);
  assert.equal(jobState(f), 'retry:1:true');
  if (extra?.MUSIC_TEST_FAULT) {
    if (['master_put', 'derivative_put'].includes(extra.MUSIC_TEST_FAULT)) {
      // The new uploader intentionally withholds untrusted provider stderr.
      assert.match(result.stderr, /Storage command failed/);
      assert.doesNotMatch(result.stderr, /Injected storage failure/);
    } else assert.match(result.stderr, new RegExp(`Injected storage failure: ${extra.MUSIC_TEST_FAULT}`));
  }
  return result;
}
const testDatabase = disposableMusicDatabase({ database, command, env });
try {
  testDatabase.create();
  for (const file of ['init_schema.sql', '2026-07-12_notification_table.sql',
    '2026-08-05_artist_enrichment.sql', '2026-08-13_unified_checkout_core.sql',
    '2026-09-04_access_request_notification_types.sql', '2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql', '2026-09-16_music_ddex_operations.sql']) {
    command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', join(root, 'tdf-hq/sql', file)]);
  }
  artist = sql("INSERT INTO party(display_name) VALUES ('Synthetic worker artist') RETURNING id");
  command('ffmpeg', ['-nostdin', '-v', 'error', '-f', 'lavfi', '-i',
    'sine=frequency=440:sample_rate=48000:duration=2', '-c:a', 'pcm_s24le', join(runtime, 'master.wav')]);
  check('idle worker and scheduler execute real SQL', () => {
    const result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.match(result.stdout, /No due music-release processing job/);
  });
  check('preview ranges, automatic reprocessing, immutable history and migration rollback', () => {
    const migration = join(root, 'tdf-hq/sql/2026-09-15_music_preview_ranges.sql');
    command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', migration]);
    assert.equal(sql("SELECT music_preview_spec(2000,1500,1000) IS NULL"), 't');
    assert.equal(sql("SELECT music_preview_spec(2000,9223372036854775807,1000) IS NULL"), 't');
    const f = fixture();
    sql(`INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist,preview_start_ms,preview_duration_ms)
      VALUES ('${f.version}','${f.recording}',1,'Synthetic artist',250,750)`);
    let result = worker();
    assert.equal(result.status, 0, result.stderr);
    const first = JSON.parse(sql(`SELECT json_build_object('id',id,'bucket',bucket_name,'key',object_key,'sha',sha256)
      FROM music_asset WHERE release_version_id='${f.version}' AND asset_role='preview_audio'`));
    assert.equal(sql(`SELECT music_preview_matches('${first.id}')`), 't');
    const duration = (path) => Number(command('ffprobe', ['-v', 'error', '-show_entries', 'format=duration', '-of', 'default=nw=1:nk=1', path]));
    assert.ok(Math.abs(duration(join(objects, first.bucket, first.key)) - 0.75) < 0.04);
    sql(`UPDATE music_release_track SET preview_start_ms=1000,preview_duration_ms=500 WHERE release_version_id='${f.version}'`);
    assert.equal(sql(`SELECT music_preview_matches('${first.id}')`), 'f');
    assert.equal(sql(`SELECT count(*) FROM music_check_submission('${f.version}') WHERE error_code='preview_processing_required'`), '1');
    assert.equal(sql('SELECT music_queue_preview_jobs(100)'), '1');
    assert.equal(sql('SELECT music_queue_preview_jobs(100)'), '0');
    const rollback = spawnSync('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database,
      '-f', join(root, 'tdf-hq/sql/2026-09-15_music_preview_ranges_rollback.sql')], { env, encoding: 'utf8' });
    assert.notEqual(rollback.status, 0, 'Rollback must reject pending preview work');
    assert.match(rollback.stderr, /Drain\/cancel preview jobs/);
    result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${f.version}' AND asset_role='preview_audio'`), '2');
    const second = JSON.parse(sql(`SELECT json_build_object('id',id,'bucket',bucket_name,'key',object_key,'sha',sha256)
      FROM music_asset WHERE release_version_id='${f.version}' AND music_preview_matches(id)`));
    assert.notEqual(first.id, second.id);
    assert.ok(Math.abs(duration(join(objects, second.bucket, second.key)) - 0.5) < 0.04);
    assert.equal(sha(readFileSync(join(objects, first.bucket, first.key))), first.sha);
    assert.equal(sql('SELECT music_queue_preview_jobs(100)'), '0');
    // A stale snapshot finishing after another edit cannot satisfy publication.
    sql(`UPDATE music_release_track SET preview_start_ms=250,preview_duration_ms=600 WHERE release_version_id='${f.version}'`);
    assert.equal(sql('SELECT music_queue_preview_jobs(100)'), '1');
    sql(`UPDATE music_release_track SET preview_start_ms=800,preview_duration_ms=600 WHERE release_version_id='${f.version}'`);
    result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${f.version}' AND music_preview_matches(id)`), '0');
    result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${f.version}' AND music_preview_matches(id)`), '1');
    // Identical bounds with different selection modes need distinct provenance;
    // otherwise content-address deduplication could keep only the explicit spec.
    for (const [start, durationMs] of [[0, 2000], [null, null], [0, 2000]]) {
      sql(`UPDATE music_release_track SET preview_start_ms=${start ?? 'NULL'},preview_duration_ms=${durationMs ?? 'NULL'} WHERE release_version_id='${f.version}'`);
      result = worker();
      assert.equal(result.status, 0, result.stderr);
      assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${f.version}' AND music_preview_matches(id)`), '1');
    }
    sql(`UPDATE music_release_track SET preview_start_ms=1900,preview_duration_ms=500 WHERE release_version_id='${f.version}'`);
    assert.equal(sql('SELECT music_queue_preview_jobs(100)'), '0');
    assert.equal(sql(`SELECT count(*) FROM music_check_submission('${f.version}') WHERE error_code='preview_range_invalid'`), '1');
    command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', join(root, 'tdf-hq/sql/2026-09-15_music_preview_ranges_rollback.sql')]);
    command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', migration]);
    assert.equal(sha(readFileSync(join(objects, first.bucket, first.key))), first.sha);
  });
  check('GET failure stops processing, preserves original and schedules retry/dead letter', () => {
    const f = fixture();
    assertFailure(f, { MUSIC_TEST_FAULT: 'get' });
    assert.equal(readFileSync(trace, 'utf8').trim(), `GET music-test-quarantine/${f.key}`);
    assert.equal(sql(`SELECT immutable FROM music_asset WHERE id='${f.asset}'`), 'f');
    const early = worker();
    assert.equal(early.status, 0, early.stderr);
    assert.equal(jobState(f), 'retry:1:true');
    retryNow(f);
    assert.notEqual(worker({ MUSIC_TEST_FAULT: 'get' }).status, 0);
    assert.equal(jobState(f), 'dead_letter:2:true');
    assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${f.version}'`), 'validation_failed');
  });
  check('checksum mismatch never promotes unverified original', () => {
    const f = fixture('inspect_audio', Buffer.from('corrupt synthetic bytes'), 'a'.repeat(64));
    assert.match(assertFailure(f).stderr, /checksum/);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /PUT|DELETE/);
    cancel(f);
  });
  check('failed audio decoding cannot produce success or delete quarantine', () => {
    const f = fixture('inspect_audio', Buffer.from('not an audio file'));
    assertFailure(f);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /PUT|DELETE/);
    cancel(f);
  });
  check('failed master PUT stops before DB promotion or quarantine deletion', () => {
    const f = fixture();
    assertFailure(f, { MUSIC_TEST_FAULT: 'master_put' });
    assert.equal(sql(`SELECT bucket_name||':'||immutable FROM music_asset WHERE id='${f.asset}'`), 'music-test-quarantine:false');
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /DELETE|PUT music-test-derivatives/);
    cancel(f);
  });
  check('failed derivative PUT retries from intact promoted master and publishes no missing assets', () => {
    const f = fixture();
    assertFailure(f, { MUSIC_TEST_FAULT: 'derivative_put' });
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '0');
    assert.equal(sql(`SELECT bucket_name||':'||immutable FROM music_asset WHERE id='${f.asset}'`), 'music-test-master:true');
    retryNow(f);
    const result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(jobState(f), 'succeeded:2:true');
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '5');
    const rows = JSON.parse(sql(`SELECT json_agg(json_build_object('bucket',bucket_name,'key',object_key,'sha',sha256))
      FROM music_asset WHERE release_version_id='${f.version}'`));
    for (const row of rows) assert.equal(sha(readFileSync(join(objects, row.bucket, row.key))), row.sha);
    assert.equal(sha(readFileSync(join(objects, 'music-test-master', `masters/${f.version}/${f.asset}/original`))), f.checksum);
    assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${f.version}'`), 'validation_failed');
    assert.equal(worker().status, 0);
    assert.equal(jobState(f), 'succeeded:2:true');
    sql(`UPDATE music_processing_job SET status='retry',run_after=NOW() WHERE id='${f.job}'`);
    const replay = worker();
    assert.equal(replay.status, 0, replay.stderr);
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '5');
    for (const row of rows) assert.equal(sha(readFileSync(join(objects, row.bucket, row.key))), row.sha);
  });
  check('quarantine cleanup failure retains recoverable bytes and still finishes derivatives', () => {
    const f = fixture();
    const result = worker({ MUSIC_TEST_FAULT: 'delete' });
    assert.equal(result.status, 0, result.stderr);
    assert.match(result.stderr, /quarantine source could not be deleted/);
    assert.equal(jobState(f), 'succeeded:1:true');
    assert.ok(existsSync(join(objects, 'music-test-quarantine', f.key)));
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '5');
  });
  check('metadata failure rolls back promotion and keeps quarantine recoverable', () => {
    const f = fixture();
    sql(`CREATE FUNCTION music_test_reject_promotion() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
      IF NEW.id='${f.asset}'::uuid AND NEW.immutable THEN RAISE EXCEPTION 'injected metadata failure'; END IF;
      RETURN NEW; END $$;
      CREATE TRIGGER music_test_reject_promotion BEFORE UPDATE ON music_asset
      FOR EACH ROW EXECUTE FUNCTION music_test_reject_promotion();`);
    assert.match(assertFailure(f).stderr, /injected metadata failure/);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /DELETE|PUT music-test-derivatives/);
    assert.equal(sql(`SELECT immutable FROM music_asset WHERE id='${f.asset}'`), 'f');
    sql('DROP TRIGGER music_test_reject_promotion ON music_asset; DROP FUNCTION music_test_reject_promotion();');
    retryNow(f);
    const result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(jobState(f), 'succeeded:2:true');
  });
  check('validation refresh failure rolls back succeeded state and remains retryable', () => {
    const f = fixture('validate_release');
    sql(`CREATE FUNCTION music_test_reject_completion() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
      IF NEW.id='${f.version}'::uuid THEN RAISE EXCEPTION 'injected validation failure'; END IF;
      RETURN NEW; END $$;
      CREATE TRIGGER music_test_reject_completion BEFORE UPDATE ON music_release_version
      FOR EACH ROW EXECUTE FUNCTION music_test_reject_completion();`);
    assert.match(assertFailure(f).stderr, /injected validation failure/);
    sql('DROP TRIGGER music_test_reject_completion ON music_release_version; DROP FUNCTION music_test_reject_completion();');
    retryNow(f);
    assert.equal(worker().status, 0);
    assert.equal(jobState(f), 'succeeded:2:true');
  });
  check('failed artwork decoding blocks all uploads', () => {
    const f = fixture('inspect_artwork', Buffer.from('not an image'));
    assertFailure(f);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /PUT|DELETE/);
    cancel(f);
  });
  check('real artwork processing promotes original and registers matching derivative hashes', () => {
    const cover = join(runtime, 'cover.png');
    command('ffmpeg', ['-nostdin', '-v', 'error', '-f', 'lavfi', '-i',
      'color=c=navy:s=3000x3000', '-frames:v', '1', '-update', '1', cover]);
    const f = fixture('inspect_artwork', readFileSync(cover));
    const result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(jobState(f), 'succeeded:1:true');
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '3');
    const rows = JSON.parse(sql(`SELECT json_agg(json_build_object('bucket',bucket_name,'key',object_key,'sha',sha256))
      FROM music_asset WHERE release_version_id='${f.version}'`));
    for (const row of rows) assert.equal(sha(readFileSync(join(objects, row.bucket, row.key))), row.sha);
    assert.equal(sha(readFileSync(join(objects, 'music-test-master', `artwork-originals/${f.version}/${f.asset}/original`))), f.checksum);
  });
  check('complete audio release becomes ready for human review exactly once', () => {
    const audio = fixture();
    const first = worker();
    assert.equal(first.status, 0, first.stderr);
    if (!existsSync(join(runtime, 'cover.png'))) command('ffmpeg', ['-nostdin', '-v', 'error', '-f', 'lavfi', '-i',
      'color=c=navy:s=3000x3000', '-frames:v', '1', '-update', '1', join(runtime, 'cover.png')]);
    const coverBytes = readFileSync(join(runtime, 'cover.png'));
    const cover = fixture('inspect_artwork', coverBytes, sha(coverBytes), audio.version);
    const party = randomUUID();
    sql(`UPDATE music_release_version SET primary_genre_id='ea0ee25a-326a-4174-9682-72d063caf6f9',
        explicit_content='not_explicit',recording_copyright_text='Synthetic master owner',
        work_copyright_text='Synthetic composition owner' WHERE id='${audio.version}';
      UPDATE music_recording SET explicit_content='not_explicit' WHERE id='${audio.recording}';
      INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist)
        VALUES ('${audio.version}','${audio.recording}',1,'Synthetic artist');
      INSERT INTO music_party(id,display_name,created_by) VALUES ('${party}','Synthetic collaborator',${artist});
      INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role)
        VALUES ('${audio.version}','${audio.recording}','${party}','main_artist'),
          ('${audio.version}','${audio.recording}','${party}','composer');
      INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
        VALUES ('${audio.version}','publication_authority','synthetic-worker-test',${artist},'{"synthetic":true}');
      INSERT INTO music_availability_rule(release_version_id,territories,listening_policy,download_policy)
        VALUES ('${audio.version}',ARRAY['Worldwide'],'full','none');`);
    for (const scope of ['master', 'composition']) {
      const declaration = randomUUID();
      sql(`BEGIN;
        INSERT INTO music_rights_declaration(id,release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by)
          VALUES ('${declaration}','${audio.version}','${audio.recording}','${scope}','owned',ARRAY['Worldwide'],'2026-01-01',${artist});
        INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on)
          VALUES ('${declaration}','${party}',10000,ARRAY['Worldwide'],'2026-01-01');
        COMMIT;`);
    }
    const result = worker();
    assert.equal(result.status, 0, result.stderr);
    assert.equal(jobState(cover), 'succeeded:1:true');
    assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${audio.version}'`), 'ready_for_review');
    assert.equal(sql(`SELECT count(*) FROM music_check_submission('${audio.version}')`), '0');
    const audits = sql(`SELECT count(*) FROM music_release_audit_event WHERE release_version_id='${audio.version}' AND next_state='ready_for_review'`);
    assert.equal(audits, '1');
    assert.equal(worker().status, 0);
    assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_version_id='${audio.version}' AND next_state='ready_for_review'`), audits);
    // Real worker/SQL/bytes with explicit DDEX renderer/package doubles. This
    // exercises the commit gate, not ERN conformance or a complete export.
    sql(`UPDATE music_release_version SET label_name='Synthetic label' WHERE id='${audio.version}';
      INSERT INTO music_identifier(release_version_id,identifier_type,identifier_value,verification_status)
        VALUES ('${audio.version}','upc','SYNTHETIC-NOT-OFFICIAL','syntax_valid');
      INSERT INTO music_identifier(recording_id,identifier_type,identifier_value,verification_status)
        VALUES ('${audio.recording}','isrc','SYNTHETIC-NOT-OFFICIAL','syntax_valid');
      UPDATE music_release_version SET state='in_review' WHERE id='${audio.version}';
      UPDATE music_release_version SET state='approved',approved_by=${artist},approved_at=NOW(),
        immutable_snapshot='{"synthetic":true}',snapshot_sha256='${'b'.repeat(64)}' WHERE id='${audio.version}';`);
    assert.equal(sql(`SELECT count(*) FROM music_check_ddex_export('${audio.version}')`), '0');
    const renderer = join(bin, 'synthetic-ddex-render'), packager = join(bin, 'synthetic-ddex-package');
    for (const [source, destination] of [['ddex-render.mjs', renderer], ['ddex-package.mjs', packager]]) {
      copyFileSync(join(root, 'test/fixtures/music-worker', source), destination);
      chmodSync(destination, 0o755);
    }
    const schema = join(runtime, 'synthetic-schema'); mkdirSync(schema);
    writeFileSync(join(schema, 'release-notification.xsd'), 'Synthetic existence marker, NOT an XSD');
    for (const revoked of [false, true]) {
      const exportId = randomUUID(), sender = randomUUID(), recipient = randomUUID(), job = randomUUID();
      sql(`INSERT INTO music_ddex_party_registry(id,party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at)
        VALUES ('${sender}','Synthetic sender','${sender.replaceAll('-', '').slice(0, 16)}','sender','Synthetic only','{"fixture":true}',${artist},NOW()),
          ('${recipient}','Synthetic recipient','${recipient.replaceAll('-', '').slice(0, 16)}','recipient','Synthetic only','{"fixture":true}',${artist},NOW());
        INSERT INTO music_ddex_export(id,release_version_id,operation,standard,ern_version,release_profile,release_profile_version,
          avs_version,structural_dictionary_version,choreography,choreography_version,sender_registry_id,recipient_registry_id,
          sender_dpid,recipient_dpid,message_id,canonical_snapshot_sha256,idempotency_key,generated_by)
        SELECT '${exportId}','${audio.version}','new_release','ERN','4.3.2','Audio','2.3.1','011','DD-ERN-432','Cloud Storage','1.8.1',
          sender.id,recipient.id,sender.dpid,recipient.dpid,'${exportId}','${'b'.repeat(64)}','${exportId}',${artist}
        FROM music_ddex_party_registry sender,music_ddex_party_registry recipient
        WHERE sender.id='${sender}' AND recipient.id='${recipient}';
        INSERT INTO music_processing_job(id,release_version_id,job_kind,job_key,output)
          VALUES ('${job}','${audio.version}','generate_ddex','${job}',jsonb_build_object('export_id','${exportId}'));`);
      const generated = worker({ MUSIC_DDEX_RENDER_BIN: renderer, MUSIC_DDEX_PACKAGE_BUILDER: packager,
        MUSIC_DDEX_SCHEMA_DIR: schema, MUSIC_TEST_REVOKE_RECIPIENT: String(revoked) });
      const exported = JSON.parse(sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exportId}'`));
      if (revoked) {
        assert.notEqual(generated.status, 0, generated.stderr);
        assert.equal(exported.status, 'validation_failed');
        assert.equal(exported.package_asset_id, null);
        assert.equal(exported.generated_at, null);
        assert.ok(exported.validation_report.errors.some(issue => issue.code === 'recipient_registry_unavailable'));
        assert.equal(jobState({ job }), 'retry:1:true');
        cancel({ job });
      } else {
        assert.equal(generated.status, 0, generated.stderr);
        assert.equal(exported.status, 'valid');
        assert.equal(jobState({ job }), 'succeeded:1:true');
        const stored = JSON.parse(sql(`SELECT row_to_json(a) FROM music_asset a WHERE id='${exported.package_asset_id}'`));
        assert.equal(sha(readFileSync(join(objects, stored.bucket_name, stored.object_key))), exported.package_sha256);
      }
    }
    console.log('PASS DDEX commit gate: synthetic package succeeds; recipient revoked during rendering blocks final validation');
  });
  check('valid DDEX retry preserves the existing package without rendering or uploading again', () => {
    // This seeds a validation STATE to test crash recovery, NOT a conformant
    // DDEX package, real DPID registry or schema validation. Everything is local.
    const f = fixture('validate_release');
    const exportId = randomUUID();
    const sender = randomUUID(), recipient = randomUUID();
    const artifacts = ['ddex_xml', 'ddex_manifest', 'ddex_package'].map((role) => {
      const id = randomUUID(), bytes = Buffer.from(`Synthetic recovery artifact: ${role}`);
      const key = `synthetic-recovery/${exportId}/${role}`;
      mkdirSync(dirname(join(objects, 'music-test-ddex', key)), { recursive: true });
      writeFileSync(join(objects, 'music-test-ddex', key), bytes);
      sql(`INSERT INTO music_asset(id,release_version_id,asset_role,storage_provider,storage_class,
        bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)
        VALUES ('${id}','${f.version}','${role}','s3_compatible','standard','music-test-ddex',
          '${key}','application/octet-stream',${bytes.length},'${sha(bytes)}','ready',true,${artist},NOW())`);
      return { id, key, bytes, hash: sha(bytes) };
    });
    sql(`INSERT INTO music_ddex_party_registry(id,party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at)
      VALUES ('${sender}','Synthetic sender','SYNTHETICSENDER','sender','Synthetic test only','{"fixture":true}',${artist},NOW()),
        ('${recipient}','Synthetic recipient','SYNTHETICRECEIVER','recipient','Synthetic test only','{"fixture":true}',${artist},NOW());
      INSERT INTO music_ddex_export(id,release_version_id,operation,standard,ern_version,release_profile,
        release_profile_version,avs_version,structural_dictionary_version,choreography,choreography_version,
        sender_registry_id,recipient_registry_id,sender_dpid,recipient_dpid,message_id,canonical_snapshot_sha256,
        idempotency_key,generated_by,status,xml_asset_id,manifest_asset_id,package_asset_id,package_sha256,generated_at,validation_report)
      VALUES ('${exportId}','${f.version}','new_release','ERN','4.3.2','Audio','2.3.1','011','DD-ERN-432','Cloud Storage','1.8.1',
        '${sender}','${recipient}','SYNTHETICSENDER','SYNTHETICRECEIVER','synthetic-recovery','${'a'.repeat(64)}',
        'synthetic-recovery',${artist},'valid','${artifacts[0].id}','${artifacts[1].id}','${artifacts[2].id}',
        '${artifacts[2].hash}',NOW(),'{"synthetic":true,"notASchemaValidation":true}');
      UPDATE music_processing_job SET job_kind='generate_ddex',output=jsonb_build_object('export_id','${exportId}') WHERE id='${f.job}';`);
    const before = sql(`SELECT row_to_json(e)::text FROM music_ddex_export e WHERE id='${exportId}'`);
    for (const attempt of [1, 2]) {
      if (attempt === 2) sql(`UPDATE music_processing_job SET status='retry',run_after=NOW() WHERE id='${f.job}'`);
      const result = worker({ MUSIC_DDEX_RENDER_BIN: '/not-a-renderer', MUSIC_DDEX_SCHEMA_DIR: '/not-a-schema' });
      assert.equal(result.status, 0, result.stderr);
      assert.equal(jobState(f), `succeeded:${attempt}:true`);
      assert.equal(sql(`SELECT row_to_json(e)::text FROM music_ddex_export e WHERE id='${exportId}'`), before);
    }
    assert.equal(readFileSync(trace, 'utf8'), '');
    for (const artifact of artifacts) assert.equal(sha(readFileSync(join(objects, 'music-test-ddex', artifact.key))), artifact.hash);
    // Even an export not yet marked valid must not receive a failure from a
    // different version's job (the error handler is a write boundary too).
    sql(`UPDATE music_ddex_export SET status='queued' WHERE id='${exportId}'`);
    const mismatchBefore = sql(`SELECT row_to_json(e)::text FROM music_ddex_export e WHERE id='${exportId}'`);
    const wrongVersion = fixture('generate_ddex');
    sql(`UPDATE music_processing_job SET output=jsonb_build_object('export_id','${exportId}') WHERE id='${wrongVersion.job}'`);
    assert.match(assertFailure(wrongVersion).stderr, /does not belong to this release version/);
    assert.equal(sql(`SELECT row_to_json(e)::text FROM music_ddex_export e WHERE id='${exportId}'`), mismatchBefore);
    assert.equal(readFileSync(trace, 'utf8'), '');
    cancel(wrongVersion);
  });
  await checkAsync('two sibling jobs close a release once without losing the final validation', async () => {
    const first = fixture('validate_release');
    const second = { ...first, job: randomUUID() };
    sql(`INSERT INTO music_processing_job(id,release_version_id,source_asset_id,job_kind,job_key)
      VALUES ('${second.job}','${first.version}','${first.asset}','validate_release','${second.job}')`);
    const workers = [startWorker(), startWorker()];
    for (const result of await Promise.all(workers.map(completed))) {
      // Bounded lock contention is retryable, not a lost publication. Do not
      // accept deadlocks or unrelated failures as a substitute for success.
      assert.doesNotMatch(result.stderr, /deadlock detected/);
      if (result.status !== 0) assert.match(result.stderr, /canceling statement due to lock timeout/);
    }
    for (const f of [first, second]) {
      const state = jobState(f);
      if (state === 'retry:1:true') {
        assert.match(sql(`SELECT error_summary FROM music_processing_job WHERE id='${f.job}'`), /lock timeout/);
        retryNow(f);
        const retry = worker();
        assert.equal(retry.status, 0, retry.stderr);
        assert.equal(jobState(f), 'succeeded:2:true');
      } else assert.equal(state, 'succeeded:1:true');
    }
    assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${first.version}'`), 'validation_failed');
    assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_version_id='${first.version}' AND next_state='validation_failed'`), '1');
  });
  await checkAsync('heartbeat keeps a long-running job exclusive across multiple lease intervals', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const running = startWorker({ MUSIC_TEST_GATE: gate, MUSIC_TEST_FAULT: 'get',
      MUSIC_WORKER_LEASE_SECONDS: '30', MUSIC_WORKER_HEARTBEAT_SECONDS: '2' });
    await waitForGate(running, gate, 'GET gate');
    const gateObservedAt = Date.now();
    const firstLease = sql(`SELECT locked_at FROM music_processing_job WHERE id='${f.job}'`);
    await setTimeout(65000);
    assert.notEqual(sql(`SELECT locked_at FROM music_processing_job WHERE id='${f.job}'`), firstLease,
      `Heartbeat did not renew: ${jobState(f)}\n${JSON.stringify(running.snapshot())}`);
    const contender = worker({ MUSIC_WORKER_LEASE_SECONDS: '30', MUSIC_WORKER_HEARTBEAT_SECONDS: '2' });
    assert.equal(contender.status, 0, contender.stderr);
    assert.match(contender.stdout, /No due music-release processing job/,
      `Lease exclusivity failed after ${Date.now() - gateObservedAt}ms since gate observation; ` +
      `state=${jobState(f)}; firstWorker=${JSON.stringify(running.snapshot())}; contender=${JSON.stringify(contender)}`);
    assert.equal(jobState(f), 'running:1:false');
    writeFileSync(`${gate}.release`, '');
    assert.notEqual((await completed(running)).status, 0);
    assert.equal(jobState(f), 'retry:1:true');
    cancel(f);
  });
  await checkAsync('expired attempt cannot fail a replacement attempt even with the same worker name', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const stale = startWorker({ MUSIC_TEST_GATE: gate, MUSIC_TEST_FAULT: 'get' });
    await waitForGate(stale, gate, 'stale GET');
    sql(`UPDATE music_processing_job SET locked_at=NOW()-INTERVAL '16 minutes' WHERE id='${f.job}'`);
    const replacement = worker({ MUSIC_TEST_FAULT: 'get' });
    assert.notEqual(replacement.status, 0);
    assert.equal(jobState(f), 'dead_letter:2:true');
    const after = sql(`SELECT row_to_json(j)::text FROM music_processing_job j WHERE id='${f.job}'`);
    writeFileSync(`${gate}.release`, '');
    const result = await completed(stale);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /lease lost|ownership lost/);
    assert.equal(sql(`SELECT row_to_json(j)::text FROM music_processing_job j WHERE id='${f.job}'`), after);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /PUT|DELETE/);
  });
  await checkAsync('stale transfer cannot promote metadata or overwrite replacement state', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const stale = startWorker({ MUSIC_TEST_GATE: gate, MUSIC_TEST_GATE_METHOD: 'PUT' });
    await waitForGate(stale, gate, 'stale PUT', 90000);
    sql(`UPDATE music_processing_job SET locked_at=NOW()-INTERVAL '16 minutes' WHERE id='${f.job}'`);
    assert.notEqual(worker({ MUSIC_TEST_FAULT: 'get' }).status, 0);
    assert.equal(jobState(f), 'dead_letter:2:true');
    writeFileSync(`${gate}.release`, '');
    const result = await completed(stale);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /lease lost/);
    assert.equal(jobState(f), 'dead_letter:2:true');
    assert.equal(sql(`SELECT immutable FROM music_asset WHERE id='${f.asset}'`), 'f');
    assert.equal(sql(`SELECT count(*) FROM music_asset WHERE parent_asset_id='${f.asset}'`), '0');
    assert.equal(sha(readFileSync(join(objects, 'music-test-quarantine', f.key))), f.checksum);
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /DELETE|PUT music-test-derivatives/);
  });
  await checkAsync('cancellation stops a blocked transfer and its child without changing the cancelled job', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const running = startWorker({ MUSIC_TEST_GATE: gate,
      MUSIC_WORKER_LEASE_SECONDS: '30', MUSIC_WORKER_HEARTBEAT_SECONDS: '2' });
    await waitForGate(running, gate, 'cancelled GET');
    const transferPid = Number(readFileSync(`${gate}.started`, 'utf8'));
    cancel(f);
    const after = sql(`SELECT row_to_json(j)::text FROM music_processing_job j WHERE id='${f.job}'`);
    const result = await completed(running);
    assert.equal(result.status, 75, result.stderr);
    assert.equal(sql(`SELECT row_to_json(j)::text FROM music_processing_job j WHERE id='${f.job}'`), after);
    assert.throws(() => process.kill(transferPid, 0), { code: 'ESRCH' });
    assert.doesNotMatch(readFileSync(trace, 'utf8'), /PUT|DELETE/);
  });
  await checkAsync('TERM interrupts an active transfer, reaps its child and records one retry', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const running = startWorker({ MUSIC_TEST_GATE: gate });
    await waitForGate(running, gate, 'TERM GET');
    const transferPid = Number(readFileSync(`${gate}.started`, 'utf8'));
    running.child.kill('SIGTERM');
    const result = await completed(running);
    assert.equal(result.status, 143, result.stderr);
    assert.equal(jobState(f), 'retry:1:true');
    assert.throws(() => process.kill(transferPid, 0), { code: 'ESRCH' });
    assert.equal(sha(readFileSync(join(objects, 'music-test-quarantine', f.key))), f.checksum);
    cancel(f);
  });
  await checkAsync('polling supervisor forwards TERM to the active iteration and waits for cleanup', async () => {
    const f = fixture();
    const gate = join(runtime, randomUUID());
    const running = startWorker({ MUSIC_TEST_GATE: gate }, 'scripts/run-music-release-worker.sh');
    await waitForGate(running, gate, 'supervisor GET');
    const transferPid = Number(readFileSync(`${gate}.started`, 'utf8'));
    running.child.kill('SIGTERM');
    const result = await completed(running);
    assert.equal(result.status, 0, result.stderr);
    assert.equal(jobState(f), 'retry:1:true');
    assert.throws(() => process.kill(transferPid, 0), { code: 'ESRCH' });
    cancel(f);
  });
  check('queued DDEX revalidates upgraded graphs and persists safe field errors on every retry', () => {
    // A pre-upgrade job, not a conformant package or verified real-world DPID.
    const f = fixture('generate_ddex');
    const exportId = randomUUID(), sender = randomUUID(), recipient = randomUUID();
    sql(`INSERT INTO music_ddex_party_registry(id,party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at)
      VALUES ('${sender}','Synthetic sender','QUEUEDSENDER','sender','Synthetic test only','{"fixture":true}',${artist},NOW()),
        ('${recipient}','Synthetic recipient','QUEUEDRECEIVER','recipient','Synthetic test only','{"fixture":true}',${artist},NOW());
      INSERT INTO music_ddex_export(id,release_version_id,operation,standard,ern_version,release_profile,
        release_profile_version,avs_version,structural_dictionary_version,choreography,choreography_version,
        sender_registry_id,recipient_registry_id,sender_dpid,recipient_dpid,message_id,canonical_snapshot_sha256,
        idempotency_key,generated_by)
      VALUES ('${exportId}','${f.version}','new_release','ERN','4.3.2','Audio','2.3.1','011','DD-ERN-432','Cloud Storage','1.8.1',
        '${sender}','${recipient}','QUEUEDSENDER','QUEUEDRECEIVER','queued-upgrade','${'a'.repeat(64)}','queued-upgrade',${artist});
      UPDATE music_asset SET asset_role='preview_audio',parent_asset_id=id WHERE id='${f.asset}';
      UPDATE music_processing_job SET output=jsonb_build_object('export_id','${exportId}') WHERE id='${f.job}';`);
    for (const file of ['2026-09-15_music_version_parties.sql', '2026-09-15_music_party_details.sql',
      '2026-09-16_music_correction_asset_graph.sql', '2026-09-16_music_correction_concurrency.sql',
      '2026-09-16_music_resource_graph_validation.sql']) {
      command('psql', ['-Xq', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', join(root, 'tdf-hq/sql', file)]);
    }
    const original = sql(`SELECT row_to_json(a)::text FROM music_asset a WHERE id='${f.asset}'`);
    for (const attempt of [1, 2]) {
      if (attempt === 2) {
        // Registry revocation after queueing must also be seen on retry.
        sql(`UPDATE music_ddex_party_registry SET active=false WHERE id='${sender}'`);
        retryNow(f);
      }
      const result = worker({ MUSIC_DDEX_RENDER_BIN: '/not-a-renderer', MUSIC_DDEX_SCHEMA_DIR: '/not-a-schema' });
      assert.notEqual(result.status, 0);
      assert.equal(jobState(f), `${attempt === 1 ? 'retry' : 'dead_letter'}:${attempt}:true`);
      assert.equal(sql(`SELECT error_code FROM music_processing_job WHERE id='${f.job}'`), 'ddex_preconditions_failed');
      const report = JSON.parse(sql(`SELECT validation_report FROM music_ddex_export WHERE id='${exportId}'`));
      assert.equal(report.valid, false);
      assert.equal(report.code, 'ddex_preconditions_failed');
      assert.ok(report.errors.some(issue => issue.code === 'resource_graph_unrooted'
        && issue.fieldPath === `assets.${f.asset}.parentAssetId`));
      assert.ok(report.errors.some(issue => issue.code === 'snapshot_mismatch'));
      if (attempt === 2) assert.ok(report.errors.some(issue => issue.code === 'sender_registry_unavailable'));
      assert.doesNotMatch(JSON.stringify(report), /music-test-quarantine|SELECT |UPDATE /);
      assert.ok(!JSON.stringify(report).includes(f.key));
      assert.doesNotMatch(result.stderr, /Pinned official|not-a-renderer|music-test-quarantine/);
      assert.equal(readFileSync(trace, 'utf8'), '', 'no GET, PUT or DELETE before validation');
      assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${f.version}' AND asset_role LIKE 'ddex_%'`), '0');
      assert.equal(sql(`SELECT row_to_json(a)::text FROM music_asset a WHERE id='${f.asset}'`), original);
    }
    assert.equal(sql(`SELECT status FROM music_ddex_export WHERE id='${exportId}'`), 'failed');
  });
  console.log(`Music worker runtime: ${passed} scenarios passed (transport fault injector; no S3 compatibility claim).`);
} finally {
  try {
    for (const child of children) child.kill('SIGTERM');
    await until(() => children.size === 0, 'test worker cleanup', 35000);
    await testDatabase.cleanup();
  } finally { rmSync(runtime, { recursive: true, force: true }); }
}
