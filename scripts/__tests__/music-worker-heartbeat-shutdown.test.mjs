import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { completedTestChild, observeTestChild, createTestStopTraceForwarder, completedTestChildWithStopTrace } from '../lib/test-child-completion.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const source = readFileSync(join(root, 'scripts/run-music-release-worker-once.sh'), 'utf8');
const extract = name => source.match(new RegExp(`^${name}\\(\\) \\{[\\s\\S]*?^\\}`, 'm'))?.[0];
for (const name of ['psql_db', 'stop_heartbeat', 'heartbeat']) assert.ok(extract(name), name);
const helpers = ['psql_db', 'trace_heartbeat_stop', 'stop_heartbeat', 'request_heartbeat_stop', 'run_heartbeat_child', 'heartbeat']
  .map(extract).filter(Boolean).join('\n');
const guard = source.slice(source.indexOf("lease_guard_sql='"), source.indexOf('\npsql_db()'));
assert.ok(guard.startsWith("lease_guard_sql='"));
const quote = text => `'${text.replaceAll("'", "'\\''")}'`;

const stopLine = (stage = 'wait', heartbeatPid = 123, status = 0) =>
  JSON.stringify({ event: 'music_worker_stop', stage, heartbeatPid, status });
test('diagnostic forwarding retains successful worker stop records with canonical fields only', () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  assert.deepEqual(forward(`unrelated stderr\n${stopLine('start')}\n${stopLine('term', 123, 7)}\n${stopLine('wait', 123, 143)}\n`),
    { forwarded: 3, truncated: false });
  assert.deepEqual(output, [stopLine('start'), stopLine('term', 123, 7), stopLine('wait', 123, 143)]);
});
test('disabled forwarding stays silent even with oversized or nonstring stderr', () => {
  for (const enabled of [false, undefined, 'true']) {
    const output = [], forward = createTestStopTraceForwarder({ enabled, emit: line => output.push(line) });
    assert.deepEqual(forward('private-payload'.repeat(10000) + stopLine()), { forwarded: 0, truncated: false });
    assert.deepEqual(forward({ toString() { throw new Error('must not inspect'); } }), { forwarded: 0, truncated: false });
    assert.deepEqual(output, []);
  }
});
test('forwarding rejects extra fields, secret payloads and noncanonical protocol values', () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  const invalid = [stopLine('unknown'), stopLine('wait', 0), stopLine('wait', -1), stopLine('wait', 1.5),
    stopLine('wait', 123, 256), stopLine('wait', 123, -1),
    stopLine().replace('"status":0', '"status":0,"secret":"private-payload"'),
    stopLine().replace('"heartbeatPid":123', '"heartbeatPid":"private-payload"'),
    stopLine().replace('"status":0', '"status":00'),
    stopLine().replace('"stage":"wait"', '"stage":"wait","stage":"private-payload"'),
    `SQL private-payload ${stopLine()}`];
  assert.deepEqual(forward(invalid.join('\n')), { forwarded: 0, truncated: false });
  assert.deepEqual(output, []);
});
test('forwarding caps per-worker/global output and reports truncation without payloads', () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  for (let i = 0; i < 129; i += 1) forward(`${stopLine('start')}\n${stopLine('term')}\n${stopLine()}\n`);
  const records = output.filter(line => line.startsWith('{'));
  assert.equal(records.length, 384);
  assert.equal(output.length, 385);
  assert.equal(output.at(-1), 'Music worker stop diagnostics truncated by fixed capture limits.');
  const perWorker = [], limited = createTestStopTraceForwarder({ enabled: true, emit: line => perWorker.push(line) });
  assert.deepEqual(limited(Array(5).fill(stopLine()).join('\n')), { forwarded: 3, truncated: true });
  assert.equal(perWorker.length, 4);
});
test('oversized capture keeps complete bounded tail records without leaking discarded text', () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  assert.deepEqual(forward('private-payload'.repeat(6000) + '\n' + stopLine()), { forwarded: 1, truncated: true });
  assert.deepEqual(output, [stopLine(), 'Music worker stop diagnostics truncated by fixed capture limits.']);
  const overlong = [], bounded = createTestStopTraceForwarder({ enabled: true, emit: line => overlong.push(line) });
  bounded('{"event":"music_worker_stop","secret":"' + 'private-payload'.repeat(100) + '"}\n');
  assert.deepEqual(overlong, ['Music worker stop diagnostics truncated by fixed capture limits.']);
});
test('async completion forwards successful and failed snapshots without changing result or error identity', async () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  const result = { status: 17, stdout: 'unchanged', stderr: stopLine() };
  const fake = { snapshot: () => ({ stderr: stopLine('term', 124, 1) }) };
  assert.equal(await completedTestChildWithStopTrace(fake, forward, { complete: async () => result }), result);
  const original = new Error('original failure');
  await assert.rejects(completedTestChildWithStopTrace(fake, forward, { complete: async () => { throw original; } }), error => error === original);
  assert.deepEqual(output, [stopLine(), stopLine('term', 124, 1)]);
});
test('diagnostic sink and snapshot failures cannot replace completion success or rejection', async () => {
  const forward = createTestStopTraceForwarder({ enabled: true, emit: () => { throw new Error('sink private-payload'); } });
  const result = { status: 0, stderr: stopLine() }, fake = { snapshot() { throw new Error('snapshot failure'); } };
  assert.equal(await completedTestChildWithStopTrace(fake, forward, { complete: async () => result }), result);
  const original = new Error('original failure');
  await assert.rejects(completedTestChildWithStopTrace(fake, forward, { complete: async () => { throw original; } }), error => error === original);
});
test('real sibling child completions expose validated stderr while retaining zero/nonzero exits', { timeout: 10_000 }, async () => {
  const output = [], forward = createTestStopTraceForwarder({ enabled: true, emit: line => output.push(line) });
  const children = [0, 17].map(status => observeTestChild(spawn(process.execPath, ['-e',
    `process.stderr.write('private-payload\\n'+JSON.stringify({event:'music_worker_stop',stage:'wait',heartbeatPid:process.pid,status:${status}})+'\\n');process.exitCode=${status};`,
  ], { stdio: ['ignore', 'pipe', 'pipe'], env: { PATH: '/usr/bin:/bin' } })));
  try {
    const results = await Promise.all(children.map(child => completedTestChildWithStopTrace(child, forward)));
    assert.deepEqual(results.map(result => result.status), [0, 17]);
    assert.deepEqual(output.map(line => JSON.parse(line)).sort((a, b) => a.status - b.status),
      children.map((child, index) => ({ event: 'music_worker_stop', stage: 'wait', heartbeatPid: child.child.pid, status: index === 0 ? 0 : 17 })));
    assert.doesNotMatch(output.join('\n'), /private-payload/);
  } finally {
    for (const child of children) if (child.snapshot().processState !== 'closed') {
      child.child.kill('SIGKILL');
      await completedTestChild(child, { timeoutMs: 1000 });
    }
  }
});
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

async function stopTraceFixture({ flag, immediate = false, fd = 'stderr', exitStatus = 0,
  commandStatus = false } = {}) {
  const f = fixture();
  const release = join(f.directory, 'diagnostic-sink-ready');
  const prelude = fd === 'broken-pipe' ? f.prelude.replace('exec 4>&2', ':') : f.prelude;
  const diagnosticSink = fd === 'closed' ? 'exec 4>&-' : fd === 'broken-pipe'
    ? `for ((i=0;i<300;i++)); do [ ! -f ${quote(release)} ] || break; /bin/sleep 0.01; done\n[ -f ${quote(release)} ] || exit 92` : '';
  const fault = commandStatus ? `
kill() {
  if [ "$#" = 3 ] && [ "$1" = -TERM ] && [ "$2" = -- ]; then
    builtin kill "$@"; return 7
  fi
  builtin kill "$@"
}
wait() { builtin wait "$@"; return 9; }
` : '';
  const waitNative = immediate ? '' : `
for ((i=0;i<300;i++)); do [ ! -f "$TDF_HEARTBEAT_READY" ] || break; /bin/sleep 0.01; done
[ -f "$TDF_HEARTBEAT_READY" ] || exit 91
`;
  const observed = observeTestChild(spawn('bash', ['-c', `${prelude}
trap 'printf "OUTER_EXIT\\n" >&2' EXIT
${diagnosticSink}
${fault}
heartbeat &
heartbeat_pid=$!
printf 'HEARTBEAT_PID=%s\\n' "$heartbeat_pid"
${waitNative}
stop_heartbeat
[ -z "$heartbeat_pid" ] || exit 94
printf 'WORKER_DONE\\n'
exit ${exitStatus}
`], { detached: true, stdio: fd === 'broken-pipe'
    ? ['ignore', 'pipe', 'pipe', 'ignore', 'pipe'] : ['ignore', 'pipe', 'pipe'],
    env: { ...f.env, TDF_HEARTBEAT_PHASE: 'sleep',
      ...(flag === undefined ? {} : { MUSIC_WORKER_STOP_DIAGNOSTICS: flag }) } }));
  if (fd === 'broken-pipe') {
    observed.child.stdio[4].destroy();
    writeFileSync(release, 'closed-owned-reader');
  }
  try {
    const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
    assert.equal(outcome.status, exitStatus, outcome.stderr);
    assert.match(outcome.stdout, /WORKER_DONE/);
    assert.equal(outcome.stderr.match(/OUTER_EXIT/g)?.length, 1, 'writer cannot run the caller EXIT trap');
    const native = existsSync(f.ready) ? JSON.parse(readFileSync(f.ready, 'utf8')) : null;
    if (!immediate) assert.ok(native, 'native heartbeat must start before the controlled stop');
    if (native) assert.equal(isAlive(native.pid), false, 'native heartbeat must be reaped');
    assert.equal(observed.snapshot().processState, 'closed');
    const events = outcome.stderr.split('\n').filter(line => line.startsWith('{"event":"music_worker_stop"')).map(line => JSON.parse(line));
    return { outcome, events, pid: Number(outcome.stdout.match(/HEARTBEAT_PID=(\d+)/)?.[1]) };
  } finally {
    const heartbeatPid = Number(observed.snapshot().stdout.match(/HEARTBEAT_PID=(\d+)/)?.[1]);
    if (observed.snapshot().processState !== 'closed') {
      signalGroup(heartbeatPid); signalGroup(observed.child.pid);
      try { await completedTestChild(observed, { timeoutMs: 1000 }); }
      catch { signalGroup(heartbeatPid, 'SIGKILL'); signalGroup(observed.child.pid, 'SIGKILL'); await completedTestChild(observed, { timeoutMs: 1000 }); }
    }
    rmSync(f.directory, { recursive: true, force: true });
  }
}

for (const immediate of [false, true]) {
  test(`optional shutdown trace observes native stop (immediate=${immediate})`, { timeout: 10_000 }, async () => {
    const { events, pid } = await stopTraceFixture({ flag: 'true', immediate });
    assert.deepEqual(events.map(({ status, ...event }) => event), ['start', 'term', 'wait'].map(stage => ({
      event: 'music_worker_stop', stage, heartbeatPid: pid,
    })));
    assert.equal(events[0].status, 0);
    assert.equal(events[1].status, 0);
    // A startup TERM may precede handler installation. The original stop
    // ignores that child status; diagnostics must expose it without a failure.
    assert.ok((immediate ? [0, 143] : [0]).includes(events[2].status));
  });
}
for (const flag of [undefined, 'false', 'TRUE', '1']) {
  test(`shutdown diagnostics remain disabled for ${flag}`, { timeout: 10_000 }, async () => {
    const { events } = await stopTraceFixture({ flag });
    assert.deepEqual(events, []);
  });
}
test('shutdown trace records ignored command failures without replacing worker status', { timeout: 10_000 }, async () => {
  const { events, pid } = await stopTraceFixture({ flag: 'true', commandStatus: true, exitStatus: 23 });
  assert.deepEqual(events, [0, 7, 9].map((status, index) => ({
    event: 'music_worker_stop', stage: ['start', 'term', 'wait'][index], heartbeatPid: pid, status,
  })));
});
for (const fd of ['closed', 'broken-pipe']) for (const exitStatus of [0, 23]) {
  test(`unavailable diagnostic sink preserves worker status ${exitStatus} (${fd})`, { timeout: 10_000 }, async () => {
    const { events } = await stopTraceFixture({ flag: 'true', fd, exitStatus });
    assert.deepEqual(events, []);
  });
}
test('shutdown trace rejects malformed protocol fields without exposing payloads', { timeout: 10_000 }, async () => {
  const f = fixture();
  const observed = observeTestChild(spawn('bash', ['-c', `${f.prelude}
if declare -F trace_heartbeat_stop >/dev/null; then
  trace_heartbeat_stop
  trace_heartbeat_stop 'private-payload' 123 0
  trace_heartbeat_stop start 'private-payload' 0
  trace_heartbeat_stop start 0 0
  trace_heartbeat_stop start 0123 0
  trace_heartbeat_stop start 12345678901 0
  trace_heartbeat_stop start 123 'private-payload'
  trace_heartbeat_stop start 123 256
  trace_heartbeat_stop start 123 00
  trace_heartbeat_stop start 123 0 'private-payload'
fi
exit 23
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'],
    env: { ...f.env, MUSIC_WORKER_STOP_DIAGNOSTICS: 'true' } }));
  try {
    const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
    assert.equal(outcome.status, 23);
    assert.equal(outcome.stdout, '');
    assert.equal(outcome.stderr, '');
  } finally {
    if (observed.snapshot().processState !== 'closed') {
      signalGroup(observed.child.pid, 'SIGKILL');
      await completedTestChild(observed, { timeoutMs: 1000 });
    }
    rmSync(f.directory, { recursive: true, force: true });
  }
});
for (const [baseline, diagnostic] of [[0, 19], [17, 0], [17, 19]]) {
  test(`actual CI runtime gate preserves baseline ${baseline} despite diagnostic ${diagnostic}`, { timeout: 10_000 }, async () => {
    const workflow = readFileSync(join(root, '.github/workflows/ci.yml'), 'utf8');
    const match = workflow.match(/^          run_music_worker_runtime_gate\(\) \{\n[\s\S]*?^          \}/m);
    assert.ok(match, 'actual hosted invocation must retain the guarded function');
    const driver = match[0].replace(/^          /gm, '');
    const observed = observeTestChild(spawn('bash', ['-c', `set -euo pipefail
npm() {
  [ "$#" = 2 ] && [ "$1" = run ] && [ "$2" = test:music-worker-runtime ] || return 95
  printf 'MODE=%s\\n' "$MUSIC_WORKER_STOP_DIAGNOSTICS"
  case "$MUSIC_WORKER_STOP_DIAGNOSTICS" in
    false) return ${baseline} ;;
    true) return ${diagnostic} ;;
    *) return 96 ;;
  esac
}
${driver}
run_music_worker_runtime_gate
`], { detached: true, stdio: ['ignore', 'pipe', 'pipe'],
      env: { PATH: '/usr/bin:/bin', MUSIC_WORKER_STOP_DIAGNOSTICS: 'true' } }));
    try {
      const outcome = await completedTestChild(observed, { timeoutMs: 4000 });
      assert.equal(outcome.status, baseline, outcome.stderr);
      const modes = outcome.stdout.split('\n').filter(line => line.startsWith('MODE='));
      assert.deepEqual(modes, baseline === 0 ? ['MODE=false'] : ['MODE=false', 'MODE=true']);
    } finally {
      if (observed.snapshot().processState !== 'closed') {
        signalGroup(observed.child.pid, 'SIGKILL');
        await completedTestChild(observed, { timeoutMs: 1000 });
      }
    }
  });
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
