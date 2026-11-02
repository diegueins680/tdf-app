import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import test from 'node:test';
import { observeTestChild, completedTestChild } from '../lib/test-child-completion.mjs';

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
