// Real two-session barrier against the disposable migration-test container.
import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { setTimeout as delay } from 'node:timers/promises';

const [container, database, release, sourceA, sourceB, actor, mode] = process.argv.slice(2);
assert.match(container ?? '', /^tdf-music-release-migration-test-[0-9]+$/);
assert.equal(database, 'tdf_music_release_test');
for (const id of [release, sourceA, sourceB]) assert.match(id ?? '', /^[0-9a-f-]{36}$/);
assert.match(actor ?? '', /^[0-9]+$/);
assert(['legacy', 'fixed'].includes(mode));
assert.notEqual(sourceA, sourceB);
const prefix = `tdf_correction_${randomUUID().replaceAll('-', '')}`;
const args = (name) => ['exec', '-i', '-e', `PGAPPNAME=${name}`,
  '-e', 'PGOPTIONS=-c statement_timeout=30000', container,
  'psql', '-XAtq', '-v', 'ON_ERROR_STOP=1', '-v', 'VERBOSITY=verbose',
  '-U', 'postgres', '-d', database];
const sql = (query) => execFileSync('docker', [...args(prefix), '-c', query],
  { encoding: 'utf8', timeout: 35000 }).trim();
function session(name) {
  const child = spawn('docker', args(name), { stdio: ['pipe', 'pipe', 'pipe'] });
  const state = { child, output: '', error: '', done: false };
  child.stdout.on('data', (chunk) => { state.output += chunk; });
  child.stderr.on('data', (chunk) => { state.error += chunk; });
  state.closed = new Promise((resolve, reject) => {
    child.on('error', reject);
    child.on('close', (code) => { state.done = true; resolve(code); });
  });
  return state;
}
async function until(predicate, label) {
  const end = Date.now() + 15000;
  while (!predicate()) {
    assert(Date.now() < end, `Timed out: ${label}`);
    await delay(50);
  }
}
const countBefore = Number(sql(`SELECT count(*) FROM music_release_version WHERE release_id='${release}'`));
const maxBefore = Number(sql(`SELECT max(version_number) FROM music_release_version WHERE release_id='${release}'`));
let a, b;
try {
  a = session(`${prefix}_a`);
  a.child.stdin.write(`BEGIN; SELECT music_create_release_correction('${release}','${sourceA}',${actor}); SELECT 'READY';\n`);
  await until(() => a.output.includes('READY') || a.done, 'first transaction ready');
  assert(a.output.includes('READY'), a.error);
  b = session(`${prefix}_b`);
  b.child.stdin.end(`SELECT music_create_release_correction('${release}','${sourceB}',${actor});\n`);
  let waitEvent;
  await until(() => {
    waitEvent = sql(`SELECT b.wait_event FROM pg_stat_activity b
      WHERE b.application_name='${prefix}_b' AND EXISTS (
        SELECT 1 FROM pg_stat_activity a WHERE a.application_name='${prefix}_a'
          AND a.pid=ANY(pg_blocking_pids(b.pid)))`);
    return waitEvent !== '' || b.done;
  }, 'second transaction demonstrably blocked by the first');
  assert.equal(waitEvent, mode === 'fixed' ? 'advisory' : 'transactionid');
  a.child.stdin.end('COMMIT;\n');
  assert.equal(await a.closed, 0, a.error);
  const secondExit = await b.closed;
  if (mode === 'legacy') {
    assert.notEqual(secondExit, 0);
    assert.match(b.error, /23505/);
    assert.match(b.error, /music_release_version_release_id_version_number_key/);
  } else {
    assert.equal(secondExit, 0, b.error);
    const firstId = a.output.split('\n').find((line) => /^[0-9a-f-]{36}$/.test(line));
    const secondId = b.output.trim();
    assert.match(secondId, /^[0-9a-f-]{36}$/);
    assert.notEqual(firstId, secondId);
    assert.equal(sql(`SELECT string_agg(version_number::text,',' ORDER BY version_number)
      FROM music_release_version WHERE id IN ('${firstId}','${secondId}')`),
    `${maxBefore + 1},${maxBefore + 2}`);
  }
  assert.equal(Number(sql(`SELECT count(*) FROM music_release_version WHERE release_id='${release}'`)),
    countBefore + (mode === 'fixed' ? 2 : 1));
  console.log(`PASS correction concurrency ${mode}: observed ${waitEvent} barrier, ${mode === 'fixed' ? 'distinct consecutive versions' : 'original unique-key race reproduced'}`);
} finally {
  // Only this fixture's unique application names; no other connections touched.
  sql(`SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE application_name IN ('${prefix}_a','${prefix}_b')`);
  for (const state of [a, b].filter(Boolean)) {
    if (!state.done) state.child.stdin.destroy();
    await state.closed;
  }
}
