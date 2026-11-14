import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { completedTestChild, observeTestChild } from '../lib/test-child-completion.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const source = readFileSync(join(root, 'scripts/run-music-release-worker-once.sh'), 'utf8');
const extract = name => source.match(new RegExp(`^${name}\\(\\) \\{[\\s\\S]*?^\\}`, 'm'))?.[0];
for (const name of ['psql_db', 'stop_heartbeat', 'heartbeat']) assert.ok(extract(name), name);
const helpers = ['psql_db', 'stop_heartbeat', 'request_heartbeat_stop', 'run_heartbeat_child', 'heartbeat']
  .map(extract).filter(Boolean).join('\n');
const guard = source.slice(source.indexOf("lease_guard_sql='"), source.indexOf('\npsql_db()'));
assert.ok(guard.startsWith("lease_guard_sql='"));
const quote = text => `'${text.replaceAll("'", "'\\''")}'`;
const signalGroup = (pid, signal = 'SIGTERM') => {
  if (!Number.isSafeInteger(pid) || pid <= 0) return;
  try { process.kill(-pid, signal); } catch (error) { if (error.code !== 'ESRCH') throw error; }
};
const isAlive = pid => {
  try { process.kill(pid, 0); return true; } catch (error) { if (error.code === 'ESRCH') return false; throw error; }
};
function fixture() {
  const directory = mkdtempSync(join(tmpdir(), 'tdf-owned-heartbeat-test-'));
  const executable = join(directory, 'native-fixture');
  writeFileSync(executable, `#!${process.execPath}
const fs = require('node:fs');
const path = require('node:path');
const kind = path.basename(process.argv[1]);
if (kind === 'sleep' && process.env.TDF_HEARTBEAT_PHASE === 'psql') process.exit(0);
const sql = kind === 'psql' ? fs.readFileSync(0, 'utf8') : null;
fs.writeFileSync(process.env.TDF_HEARTBEAT_READY, JSON.stringify({pid:process.pid,kind,sql,args:process.argv.slice(2)}));
if (process.env.TDF_HEARTBEAT_CAPTURE_ONLY === 'true') process.exit(Number(process.env.TDF_HEARTBEAT_STATUS));
const termDelay = Number(process.env.TDF_HEARTBEAT_TERM_DELAY_MS || 0);
if (termDelay > 0) {
  let stopping = false;
  process.on('SIGTERM', () => { if (!stopping) { stopping = true; setTimeout(() => process.exit(0), termDelay); } });
}
setInterval(()=>{},1000);
`);
  for (const name of ['sleep', 'psql']) {
    writeFileSync(join(directory, name), readFileSync(executable));
    chmodSync(join(directory, name), 0o755);
  }
  const ready = join(directory, 'ready.json');
  const prelude = `
set -euo pipefail
set -m
exec 4>&2
working_dir=${quote(directory)}
DATABASE_URL=postgresql://synthetic.invalid/never-contacted
job_id=11111111-1111-4111-8111-111111111111
worker_id="synthetic worker's identity"
attempt=1
lease_seconds=60
lease_active=true
heartbeat_supervised=false
timing() { :; }
${guard}
${helpers}
heartbeat_pid=''
heartbeat_seconds=60
`;
  return { directory, ready, prelude, env: { PATH: `${directory}:/usr/bin:/bin`, TDF_HEARTBEAT_READY: ready } };
}
for (const phase of ['sleep', 'psql']) {
  for (const missedDelivery of [false, true]) {
    test(`heartbeat reaps ${phase} when first group signal ${missedDelivery ? 'misses the native child' : 'reaches the whole group'}`,
      { timeout: 10_000 }, async () => {
        const f = fixture();
        // Controlled first-delivery miss models a fork/signal race; live CI
        // did not identify its descriptor holder.
        const fault = missedDelivery ? `
kill() {
  if [ "$#" = 3 ] && [ "$1" = -TERM ] && [ "$2" = -- ]; then
    builtin kill -TERM "$heartbeat_pid"
  else builtin kill "$@"; fi
}` : '';
        const observed = observeTestChild(spawn('bash', ['-c', `${f.prelude}
${fault}
heartbeat &
heartbeat_pid=$!
printf 'HEARTBEAT_PID=%s\\n' "$heartbeat_pid"
for ((i=0; i<300; i++)); do
  [ ! -f "$TDF_HEARTBEAT_READY" ] || break
  /bin/sleep 0.01
done
[ -f "$TDF_HEARTBEAT_READY" ] || exit 91
stop_heartbeat
printf 'WORKER_DONE\\n'
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'], env: { ...f.env, TDF_HEARTBEAT_PHASE: phase } }));
        try {
          const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
          assert.equal(outcome.status, 0, outcome.stderr);
          assert.match(outcome.stdout, /WORKER_DONE/);
          const native = JSON.parse(readFileSync(f.ready, 'utf8'));
          assert.equal(native.kind, phase);
          if (phase === 'psql') {
            assert.match(native.sql, /SELECT pg_temp\.music_worker_guard\(/);
            assert.ok(native.sql.endsWith('SELECT 1;\n'));
          }
          assert.equal(isAlive(native.pid), false, 'native heartbeat child must already be reaped');
          assert.equal(observed.snapshot().processState, 'closed');
        } finally {
          const heartbeatPid = Number(observed.snapshot().stdout.match(/HEARTBEAT_PID=(\d+)/)?.[1]);
          const native = existsSync(f.ready) ? JSON.parse(readFileSync(f.ready, 'utf8')) : null;
          if (observed.snapshot().processState !== 'closed' || (native && isAlive(native.pid))) {
            signalGroup(heartbeatPid); signalGroup(observed.child.pid);
            try { await completedTestChild(observed, { timeoutMs: 1000 }); }
            catch { signalGroup(heartbeatPid, 'SIGKILL'); signalGroup(observed.child.pid, 'SIGKILL'); await completedTestChild(observed, { timeoutMs: 1000 }); }
          }
          rmSync(f.directory, { recursive: true, force: true });
        }
      });
  }
}
test('heartbeat stops within moments when the first group signal is lost entirely', { timeout: 10_000 }, async () => {
  const f = fixture();
  // A TERM delivered while the heartbeat or its sleep is still being forked
  // reaches nobody. Model it by dropping the first group signal: without a
  // repeated signal the worker waits out the whole 60 s heartbeat interval.
  const observed = observeTestChild(spawn('bash', ['-c', `${f.prelude}
kill() {
  if [ "$#" = 3 ] && [ "$1" = -TERM ] && [ "$2" = -- ] && [ ! -e "$working_dir/first-term-lost" ]; then
    : > "$working_dir/first-term-lost"
    return 0
  fi
  builtin kill "$@"
}
heartbeat &
heartbeat_pid=$!
printf 'HEARTBEAT_PID=%s\\n' "$heartbeat_pid"
for ((i=0; i<300; i++)); do
  [ ! -f "$TDF_HEARTBEAT_READY" ] || break
  /bin/sleep 0.01
done
[ -f "$TDF_HEARTBEAT_READY" ] || exit 91
stop_heartbeat
[ -e "$working_dir/first-term-lost" ] || exit 92
printf 'WORKER_DONE\\n'
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'], env: { ...f.env, TDF_HEARTBEAT_PHASE: 'sleep' } }));
  try {
    const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
    assert.equal(outcome.status, 0, outcome.stderr);
    assert.match(outcome.stdout, /WORKER_DONE/);
    const native = JSON.parse(readFileSync(f.ready, 'utf8'));
    assert.equal(native.kind, 'sleep');
    assert.equal(isAlive(native.pid), false, 'native heartbeat child must already be reaped');
    assert.equal(observed.snapshot().processState, 'closed');
  } finally {
    const heartbeatPid = Number(observed.snapshot().stdout.match(/HEARTBEAT_PID=(\d+)/)?.[1]);
    if (observed.snapshot().processState !== 'closed') {
      signalGroup(heartbeatPid, 'SIGKILL'); signalGroup(observed.child.pid, 'SIGKILL');
      await completedTestChild(observed, { timeoutMs: 1000 }).catch(() => {});
    }
    rmSync(f.directory, { recursive: true, force: true });
  }
});
test('repeated stop signals do not let the worker return before a slow native child is reaped', { timeout: 10_000 }, async () => {
  const f = fixture();
  // The native child takes 1.2 s to exit after TERM, far longer than the gap
  // between repeated signals. The heartbeat must keep waiting for it.
  const started = Date.now();
  const observed = observeTestChild(spawn('bash', ['-c', `${f.prelude}
heartbeat &
heartbeat_pid=$!
printf 'HEARTBEAT_PID=%s\\n' "$heartbeat_pid"
for ((i=0; i<300; i++)); do
  [ ! -f "$TDF_HEARTBEAT_READY" ] || break
  /bin/sleep 0.01
done
[ -f "$TDF_HEARTBEAT_READY" ] || exit 91
stop_heartbeat
printf 'WORKER_DONE\\n'
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'],
    env: { ...f.env, TDF_HEARTBEAT_PHASE: 'sleep', TDF_HEARTBEAT_TERM_DELAY_MS: '1200' } }));
  try {
    const outcome = await completedTestChild(observed, { timeoutMs: 6000 });
    assert.equal(outcome.status, 0, outcome.stderr);
    assert.match(outcome.stdout, /WORKER_DONE/);
    const native = JSON.parse(readFileSync(f.ready, 'utf8'));
    assert.equal(isAlive(native.pid), false, 'slow native heartbeat child must be reaped before the worker returns');
    assert.ok(Date.now() - started >= 1100, 'the worker must have waited for the slow child');
    assert.equal(observed.snapshot().processState, 'closed');
  } finally {
    const heartbeatPid = Number(observed.snapshot().stdout.match(/HEARTBEAT_PID=(\d+)/)?.[1]);
    const native = existsSync(f.ready) ? JSON.parse(readFileSync(f.ready, 'utf8')) : null;
    if (observed.snapshot().processState !== 'closed' || (native && isAlive(native.pid))) {
      signalGroup(heartbeatPid, 'SIGKILL'); signalGroup(observed.child.pid, 'SIGKILL');
      if (native && isAlive(native.pid)) { try { process.kill(native.pid, 'SIGKILL'); } catch { /* already gone */ } }
      await completedTestChild(observed, { timeoutMs: 1000 }).catch(() => {});
    }
    rmSync(f.directory, { recursive: true, force: true });
  }
});
for (const status of [0, 7]) {
  test(`normal and supervised SQL preserve guard, input, options and status ${status}`, { timeout: 10_000 }, async () => {
    const captures = [];
    for (const supervised of [false, true]) {
      const f = fixture();
      const observed = observeTestChild(spawn('bash', ['-c', `${f.prelude}
heartbeat_stopping=false
heartbeat_child=''
heartbeat_supervised=${supervised}
if [ "$heartbeat_supervised" = true ]; then set +m; fi
result=0
psql_db -c 'SELECT 1;' || result=$?
exit "$result"
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'], env: {
        ...f.env, TDF_HEARTBEAT_PHASE: 'psql', TDF_HEARTBEAT_CAPTURE_ONLY: 'true', TDF_HEARTBEAT_STATUS: String(status),
      } }));
      try {
        const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
        assert.equal(outcome.status, status, outcome.stderr);
        const { pid, ...capture } = JSON.parse(readFileSync(f.ready, 'utf8'));
        assert.equal(isAlive(pid), false);
        assert.match(capture.sql, /SELECT pg_temp\.music_worker_guard\(/);
        assert.ok(capture.sql.endsWith('SELECT 1;\n'));
        assert.ok(capture.args.includes("lease_owner=synthetic worker's identity"));
        captures.push(capture);
      } finally {
        if (observed.snapshot().processState !== 'closed') {
          signalGroup(observed.child.pid, 'SIGKILL');
          await completedTestChild(observed, { timeoutMs: 1000 });
        }
        rmSync(f.directory, { recursive: true, force: true });
      }
    }
    assert.deepEqual(captures[0], captures[1]);
  });
}
