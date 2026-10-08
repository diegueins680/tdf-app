// Real HTTP/transaction probes. Fault injection is installed only in the
// disposable E2E database and targets one synthetic idempotency key.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { setTimeout as delay } from 'node:timers/promises';

export async function probeDdexEnqueue({ request, sql, database, path, payload, versionId }) {
  assert.match(database, /^tdf_music_(?:s3_[a-f0-9]{32}|release_api_e2e)$/);
  assert.match(versionId, /^[a-f0-9-]{36}$/);
  assert.equal(payload.idempotencyKey, 'music-e2e-ddex-export');
  const key = payload.idempotencyKey;
  const counts = () => sql(`SELECT json_build_array(
    (SELECT count(*) FROM music_ddex_export WHERE idempotency_key='${key}'),
    (SELECT count(*) FROM music_processing_job WHERE job_kind='generate_ddex'
      AND release_version_id='${versionId}'))`);
  const before = counts();
  sql(`CREATE FUNCTION music_e2e_fail_ddex_job() RETURNS trigger LANGUAGE plpgsql AS $$
    BEGIN
      IF NEW.job_kind='generate_ddex' AND EXISTS(SELECT 1 FROM music_ddex_export
        WHERE id::text=NEW.job_key AND idempotency_key='${key}') THEN
        RAISE EXCEPTION 'synthetic_ddex_enqueue_private_marker';
      END IF;
      RETURN NEW;
    END $$;
    CREATE TRIGGER music_e2e_fail_ddex_job BEFORE INSERT ON music_processing_job
      FOR EACH ROW EXECUTE FUNCTION music_e2e_fail_ddex_job();`);
  try {
    const failure = await request(path, { ...payload, expected: 500 });
    assert.doesNotMatch(String(failure), /synthetic_ddex_enqueue_private_marker|INSERT|SELECT/);
    assert.equal(counts(), before, 'Job insert failure must roll back the export too');
    // A controlled application abort AFTER both inserts must roll back too;
    // returning Left from inside runSqlPool would incorrectly commit them.
    sql(`CREATE OR REPLACE FUNCTION music_e2e_fail_ddex_job() RETURNS trigger LANGUAGE plpgsql AS $$
      BEGIN
        IF NEW.job_kind='generate_ddex' AND EXISTS(SELECT 1 FROM music_ddex_export
          WHERE id::text=NEW.job_key AND idempotency_key='${key}') THEN
          NEW.output=jsonb_build_object('export_id',gen_random_uuid());
        END IF;
        RETURN NEW;
      END $$;`);
    const rejectedLink = await request(path, { ...payload, expected: 409 });
    assert.match(String(rejectedLink), /linkage is inconsistent/);
    assert.equal(counts(), before, 'Controlled error must roll back both inserted rows');
  } finally {
    sql(`DROP TRIGGER music_e2e_fail_ddex_job ON music_processing_job;
      DROP FUNCTION music_e2e_fail_ddex_job();`);
  }

  // Hold the version in another connection until four HTTP calls demonstrably
  // overlap: one waits on the version, three on the actor/key advisory lock.
  const name = `music_ddex_barrier_${randomUUID().replaceAll('-', '')}`;
  const child = spawn('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database], {
    env: { ...process.env, PGAPPNAME: name,
      PGOPTIONS: '-c statement_timeout=30000 -c idle_in_transaction_session_timeout=30000' },
    stdio: ['pipe', 'pipe', 'pipe'],
  });
  let output = '', error = '', done = false;
  child.stdout.on('data', bytes => { output += bytes; });
  child.stderr.on('data', bytes => { error += bytes; });
  const closed = new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('close', code => { done = true; resolve(code); });
  });
  async function until(predicate) {
    const deadline = Date.now() + 15000;
    while (!predicate()) {
      assert(!done, `Barrier connection exited: ${error}`);
      assert(Date.now() < deadline, 'DDEX concurrency barrier timed out');
      await delay(50);
    }
  }
  let pending;
  let results;
  try {
    child.stdin.write(`BEGIN; SELECT id FROM music_release_version
      WHERE id='${versionId}' FOR UPDATE; SELECT 'READY';\n`);
    await until(() => output.includes('READY'));
    // allSettled prevents unhandled rejections while inspecting the barrier.
    pending = Promise.allSettled(Array.from({ length: 4 }, () => request(path, payload)));
    await until(() => sql(`SELECT EXISTS(SELECT 1 FROM pg_stat_activity b
        WHERE b.datname=current_database() AND EXISTS(SELECT 1 FROM pg_stat_activity a
          WHERE a.application_name='${name}' AND a.pid=ANY(pg_blocking_pids(b.pid))))
      AND (SELECT count(*) FROM pg_stat_activity WHERE datname=current_database()
        AND wait_event_type='Lock' AND wait_event='advisory') >= 3`) === 't');
    child.stdin.end('COMMIT;\n');
    assert.equal(await closed, 0, error);
    results = await pending;
    for (const result of results) assert.equal(result.status, 'fulfilled', String(result.reason));
  } finally {
    if (!done) {
      sql(`SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE application_name='${name}'`);
      child.stdin.destroy();
      await closed;
    }
    if (pending) await pending;
  }
  const exported = results[0].value;
  assert.equal(new Set(results.map(result => result.value.id)).size, 1);
  assert.equal(sql(`SELECT count(*) FROM music_ddex_export WHERE idempotency_key='${key}'`), '1');
  const jobCount = () => sql(`SELECT count(*) FROM music_processing_job
    WHERE job_kind='generate_ddex' AND job_key='${exported.id}'`);
  assert.equal(jobCount(), '1');
  await request(path, { ...payload, expected: 409, idempotencyKey: `${key}-another-key` });
  assert.equal(sql(`SELECT count(*) FROM music_ddex_export WHERE idempotency_key='${key}-another-key'`), '0');

  // Recreate only a stranded queued legacy job, preserving the export itself.
  const exportBefore = sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exported.id}'`);
  sql(`DELETE FROM music_processing_job WHERE job_kind='generate_ddex' AND job_key='${exported.id}'`);
  const recovered = await Promise.all(Array.from({ length: 4 }, () => request(path, payload)));
  assert(recovered.every(value => value.id === exported.id));
  assert.equal(jobCount(), '1');
  assert.equal(sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exported.id}'`), exportBefore);
  sql(`UPDATE music_processing_job SET status='retry',attempt_count=2,
    output=output || '{"retainedFixture":true}'::jsonb
    WHERE job_kind='generate_ddex' AND job_key='${exported.id}'`);
  const jobBefore = sql(`SELECT row_to_json(j) FROM music_processing_job j
    WHERE job_kind='generate_ddex' AND job_key='${exported.id}'`);
  await request(path, payload);
  assert.equal(sql(`SELECT row_to_json(j) FROM music_processing_job j
    WHERE job_kind='generate_ddex' AND job_key='${exported.id}'`), jobBefore);
  console.log('PASS API DDEX enqueue → SQL and controlled-error rollback, observed lock barrier, four concurrent replays, natural-key conflict, queued orphan recovery and preserved attempts');
  return exported;
}
