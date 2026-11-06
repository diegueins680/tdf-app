import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import test from 'node:test';
import { observeTestChild, completedTestChild, testChildProcessMetadata } from '../lib/test-child-completion.mjs';
import { basename } from 'node:path';

const observeNode = source => observeTestChild(spawn(process.execPath, ['-e', source], {
  stdio: ['ignore', 'pipe', 'pipe', 'ipc'], env: { PATH: process.env.PATH },
  // Own a separate group on the tested macOS/Linux hosts, including before IPC.
  detached: process.platform !== 'win32',
}));
const nextEvent = (child, event, timeout = 2000) => once(child, event, { signal: AbortSignal.timeout(timeout) });
const stopPid = (pid, signal = 'SIGTERM') => {
  if (!pid) return;
  try { process.kill(pid, signal); }
  catch (error) { if (error.code !== 'ESRCH') throw error; }
};
async function stopFixture(observed, descendantPid) {
  if (observed.child.pid && process.platform !== 'win32') stopPid(-observed.child.pid);
  observed.child.kill('SIGTERM');
  stopPid(descendantPid);
  try { await completedTestChild(observed, { timeoutMs: 1500 }); }
  catch (error) {
    if (observed.child.pid && process.platform !== 'win32') stopPid(-observed.child.pid, 'SIGKILL');
    observed.child.kill('SIGKILL');
    stopPid(descendantPid, 'SIGKILL');
    await completedTestChild(observed, { timeoutMs: 1500 });
    throw error;
  }
}
function parentWithDescendant(sendReady = true) {
  const descendant = `process.stdout.write('descendant-output'); process.stderr.write('descendant-error');
    ${sendReady ? "process.send('ready');" : ''} setInterval(()=>{},1000);`;
  return observeNode(`const {spawn}=require('node:child_process');
    const child=spawn(process.execPath,['-e',${JSON.stringify(descendant)}],{stdio:['ignore',1,2,'ipc']});
    let ready=false, reported=false, stopping=false;
    const exitWhenReady=()=>{if(ready&&reported&&!stopping){child.disconnect();process.disconnect();process.exit(0);}};
    child.once('message',()=>{ready=true;exitWhenReady();});
    child.once('close',()=>{if(stopping)process.exit(143);});
    process.on('SIGTERM',()=>{stopping=true;child.kill('SIGTERM');
      if(child.exitCode!==null||child.signalCode!==null)process.exit(143);});
    process.send({pid:child.pid},()=>{reported=true;exitWhenReady();});`);
}

test('normal completion retains full output and observes exit before close', { timeout: 8000 }, async () => {
  const observed = observeNode("process.stdout.write('fixture-output'); process.stderr.write('fixture-error'); process.disconnect();");
  try {
    const result = await completedTestChild(observed, { timeoutMs: 3000 });
    assert.deepEqual(result, { status: 0, signal: null, stdout: 'fixture-output', stderr: 'fixture-error' });
    const snapshot = observed.snapshot();
    assert.equal(snapshot.processState, 'closed');
    assert.ok(snapshot.spawnedAt >= snapshot.startedAt);
    assert.ok(snapshot.exitedAt >= snapshot.spawnedAt);
    assert.ok(snapshot.closedAt >= snapshot.exitedAt);
    assert.equal(snapshot.stdoutStream.ended, true);
    assert.equal(snapshot.stderrStream.ended, true);
  } finally { await stopFixture(observed); }
});

test('the default completion deadline remains 30 seconds', async t => {
  const delays = [];
  const original = globalThis.setTimeout;
  t.mock.method(globalThis, 'setTimeout', (callback, delay) => {
    delays.push(delay);
    return original(callback, delay);
  });
  const result = { status: 0 };
  assert.equal(await completedTestChild({ done: Promise.resolve(result) }), result);
  assert.deepEqual(delays, [30000]);
});

test('an early spawn error remains rejected when completion is awaited later', { timeout: 5000 }, async () => {
  const observed = observeTestChild(spawn('/nonexistent/tdf-synthetic-child', [], { stdio: ['ignore', 'pipe', 'pipe'] }));
  await new Promise(resolve => setImmediate(resolve));
  await assert.rejects(completedTestChild(observed, { timeoutMs: 2000 }), { code: 'ENOENT' });
  assert.match(observed.snapshot().spawnError, /ENOENT/);
});

test('a live child timeout retains bounded output and remains a failure', { timeout: 8000 }, async () => {
  const observed = observeNode("process.stdout.write('x'.repeat(9000)+'stdout-end'); process.stderr.write('y'.repeat(9000)+'stderr-end'); process.send('ready'); setInterval(()=>{},1000);");
  try {
    await nextEvent(observed.child, 'message');
    await assert.rejects(completedTestChild(observed, { timeoutMs: 40 }), error => {
      const snapshot = error.workerSnapshot;
      assert.match(error.message, /^Worker did not stop within 0\.04s/);
      assert.match(error.message, /Worker completion snapshot:/);
      assert.equal(snapshot.pid, observed.child.pid);
      assert.equal(snapshot.processState, 'awaiting-exit');
      assert.equal(snapshot.exitedAt, null);
      assert.equal(snapshot.closedAt, null);
      assert.equal(snapshot.exitCode, null);
      assert.ok(snapshot.stdoutCharacters > 9000);
      assert.equal(snapshot.stdoutTail.length, 4000);
      assert.equal(snapshot.stderrTail.length, 4000);
      assert.ok(snapshot.stdoutTail.endsWith('stdout-end'));
      assert.ok(snapshot.stderrTail.endsWith('stderr-end'));
      return true;
    });
  } finally {
    await stopFixture(observed);
  }
});

test('exited parent with descendant-held output cannot complete before close', { timeout: 8000 }, async () => {
  const observed = parentWithDescendant();
  const exited = nextEvent(observed.child, 'exit');
  void exited.catch(() => {});
  let descendantPid;
  try {
    const [message] = await nextEvent(observed.child, 'message');
    descendantPid = message.pid;
    assert.ok(Number.isSafeInteger(descendantPid) && descendantPid > 0);
    await exited;
    await assert.rejects(completedTestChild(observed, { timeoutMs: 40 }), error => {
      const snapshot = error.workerSnapshot;
      assert.equal(snapshot.processState, 'exited-awaiting-close');
      assert.equal(snapshot.exitCode, 0);
      assert.ok(snapshot.exitedAt >= snapshot.spawnedAt);
      assert.equal(snapshot.closedAt, null);
      assert.equal(snapshot.stdoutStream.ended, false);
      assert.equal(snapshot.stderrStream.ended, false);
      assert.equal(snapshot.stdoutTail, 'descendant-output');
      assert.equal(snapshot.stderrTail, 'descendant-error');
      return true;
    });
  } finally {
    await stopFixture(observed, descendantPid);
  }
  assert.equal(observed.snapshot().processState, 'closed');
});

test('missing descendant readiness rejects within the bound and cleans the fixture', { timeout: 8000 }, async () => {
  const observed = parentWithDescendant(false);
  let descendantPid;
  try {
    const [message] = await nextEvent(observed.child, 'message');
    descendantPid = message.pid;
    assert.ok(Number.isSafeInteger(descendantPid) && descendantPid > 0);
    await assert.rejects(nextEvent(observed.child, 'exit', 40), { name: 'AbortError' });
  } finally {
    // The parent also owns a TERM handler for failure before the PID handshake.
    await stopFixture(observed, descendantPid);
  }
  assert.throws(() => process.kill(descendantPid, 0), { code: 'ESRCH' });
});

test('timeout metadata contains a live owned parent and descendant but no unrelated child or argv',
  { timeout: 8000, skip: process.platform === 'win32' }, async t => {
    const observed = parentWithDescendant(false);
    const unrelated = observeNode("process.send('unrelated-ready'); setInterval(()=>{},1000);");
    const unrelatedReady = nextEvent(unrelated.child, 'message');
    void unrelatedReady.catch(() => {});
    let descendantPid;
    try {
      const [message] = await nextEvent(observed.child, 'message');
      descendantPid = message.pid;
      await unrelatedReady;
      await assert.rejects(completedTestChild(observed, { timeoutMs: 40 }), error => {
        assert.equal(error.workerSnapshot.processState, 'awaiting-exit');
        const metadata = error.workerProcessMetadata;
        if (metadata.reason === 'ps-unavailable') {
          t.skip('POSIX ps absent; synthetic parser and missing-ps controls still run');
          return true;
        }
        assert.equal(metadata.status, 'observed');
        assert.equal(metadata.rootPid, observed.child.pid);
        assert.ok(metadata.processes.some(row => row.pid === observed.child.pid && row.ppid === process.pid));
        assert.ok(metadata.processes.some(row => row.pid === descendantPid && row.ppid === observed.child.pid));
        assert.equal(metadata.processes.some(row => row.pid === unrelated.child.pid || row.pid === process.pid), false);
        assert.ok(metadata.processes.every(row => row.comm === basename(process.execPath)));
        assert.doesNotMatch(JSON.stringify(metadata), /descendant-output|descendant-error|setInterval|unrelated-ready/);
        return true;
      });
    } finally {
      await stopFixture(observed, descendantPid);
      await stopFixture(unrelated);
    }
  });

test('Linux metadata retains numeric session/wait state and excludes unrelated rows', () => {
  const pid = process.pid + 100000;
  const stdout = `${pid} ${process.pid} ${pid} ${pid} S do_wait /usr/bin/bash\n`
    + `${pid + 1} ${pid} ${pid + 1} ${pid} S hrtimer_nanosleep /bin/sleep\n`
    + `${pid + 2} 1 1 1 S poll_schedule_timeout /unrelated/PRIVATE_NAME\n`;
  const metadata = testChildProcessMetadata(pid, { platform: 'linux', run: () => ({ status: 0, stdout }) });
  assert.equal(metadata.status, 'observed');
  assert.deepEqual(metadata.processes.map(row => row.pid), [pid, pid + 1]);
  assert.deepEqual(metadata.processes.map(row => row.comm), ['bash', 'sleep']);
  assert.equal(metadata.processes[1].sid, pid);
  assert.equal(metadata.processes[1].wchan, 'hrtimer_nanosleep');
  assert.doesNotMatch(JSON.stringify(metadata), /PRIVATE_NAME|\/usr\/|\/bin\//);
});

test('process diagnostics are bounded and never request arguments or environment', () => {
  const pid = process.pid + 100000;
  const stdout = `${pid} ${process.pid} ${pid} S /bin/bash\n`
    + Array.from({ length: 40 }, (_, index) => `${pid + index + 1} ${pid} ${pid} S /bin/sleep`).join('\n');
  const metadata = testChildProcessMetadata(pid, { platform: 'darwin', run: (program, args, options) => {
    assert.equal(program, 'ps');
    assert.deepEqual(args, ['-A', '-o', 'pid=,ppid=,pgid=,stat=,comm=']);
    assert.equal(options.timeout, 200);
    assert.equal(options.killSignal, 'SIGKILL');
    assert.equal(options.maxBuffer, 512 * 1024);
    assert.deepEqual(Object.keys(options.env).sort(), ['LANG', 'LC_ALL', 'PATH']);
    return { status: 0, stdout };
  } });
  assert.equal(metadata.processes.length, 32);
  assert.equal(metadata.truncated, true);
});

test('missing or failed ps is graceful and never exposes diagnostic stderr', () => {
  const pid = process.pid + 100000;
  const missing = testChildProcessMetadata(pid, { run: () => ({ error: { code: 'ENOENT' }, stderr: 'PRIVATE_ERROR' }) });
  assert.deepEqual(missing, { status: 'unavailable', reason: 'ps-unavailable' });
  const failed = testChildProcessMetadata(pid, { run: () => { throw new Error('PRIVATE_ERROR'); } });
  assert.deepEqual(failed, { status: 'unavailable', reason: 'ps-failed' });
  assert.deepEqual(testChildProcessMetadata(pid, { run: () => ({ status: 0, stdout: '' }) }),
    { status: 'unavailable', reason: 'owned-child-not-present' });
  assert.doesNotMatch(JSON.stringify([missing, failed]), /PRIVATE_ERROR/);
});

test('a late completion during diagnostics cannot replace the original timeout failure', async () => {
  let finish;
  const done = new Promise(resolve => { finish = resolve; });
  const snapshot = () => ({ stdout: '', stderr: '', pid: null, processState: 'awaiting-exit' });
  await assert.rejects(completedTestChild({ done, snapshot }, { timeoutMs: 5,
    collectProcessMetadata: () => { finish({ status: 0 }); return { status: 'observed' }; },
  }), error => {
    assert.match(error.message, /^Worker did not stop within 0\.005s\nWorker completion snapshot:/);
    assert.equal(error.workerProcessMetadata.status, 'observed');
    return true;
  });
});

test('diagnostic collector failure cannot mask the unchanged primary timeout', async () => {
  const snapshot = () => ({ stdout: '', stderr: '', pid: null, processState: 'awaiting-exit' });
  await assert.rejects(completedTestChild({ done: new Promise(() => {}), snapshot }, { timeoutMs: 5,
    collectProcessMetadata: () => { throw new Error('PRIVATE_DIAGNOSTIC_ERROR'); },
  }), error => {
    assert.match(error.message, /^Worker did not stop within 0\.005s\nWorker completion snapshot:/);
    assert.equal(error.workerSnapshot.processState, 'awaiting-exit');
    assert.deepEqual(error.workerProcessMetadata, { status: 'unavailable', reason: 'diagnostic-collector-failed' });
    assert.doesNotMatch(error.message, /PRIVATE_DIAGNOSTIC_ERROR/);
    return true;
  });
});
