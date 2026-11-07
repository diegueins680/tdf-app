// Synthetic test children only. A process exit and drained output are separate
// milestones: descendants can retain the parent's stdout/stderr descriptors.
import { spawnSync } from 'node:child_process';
import { basename } from 'node:path';

// Failure diagnostics only: never request argv, environment, users or SQL text.
// Capture a bounded table internally, then expose only this direct child's tree.
export function testChildProcessMetadata(pid, { run = spawnSync, platform = process.platform } = {}) {
  if (!Number.isSafeInteger(pid) || pid <= 0) return { status: 'unavailable', reason: 'invalid-child-pid' };
  if (platform === 'win32') return { status: 'unavailable', reason: 'unsupported-platform' };
  const linux = platform === 'linux';
  const columns = linux ? 'pid=,ppid=,pgid=,sid=,stat=,wchan=,comm=' : 'pid=,ppid=,pgid=,stat=,comm=';
  let result;
  try {
    result = run('ps', ['-A', '-o', columns], {
      encoding: 'utf8', timeout: 200, killSignal: 'SIGKILL', maxBuffer: 512 * 1024,
      env: { PATH: process.env.PATH, LANG: 'C', LC_ALL: 'C' },
    });
  } catch { return { status: 'unavailable', reason: 'ps-failed' }; }
  if (result.error || result.status !== 0) {
    return { status: 'unavailable', reason: result.error?.code === 'ENOENT' ? 'ps-unavailable' : 'ps-failed' };
  }
  const table = new Map();
  const pattern = linux
    ? /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+([A-Za-z+<]+)\s+(\S+)\s+(.+?)\s*$/
    : /^\s*(\d+)\s+(\d+)\s+(\d+)\s+([A-Za-z+<]+)\s+(.+?)\s*$/;
  for (const line of String(result.stdout).split('\n')) {
    const match = line.match(pattern);
    if (!match) continue;
    const numbers = match.slice(1, linux ? 5 : 4).map(Number);
    if (!numbers.every(value => Number.isSafeInteger(value) && value >= 0)) continue;
    const [processPid, ppid, pgid, sid] = numbers;
    if (table.has(processPid)) return { status: 'unavailable', reason: 'ambiguous-process-table' };
    const comm = basename(match[linux ? 7 : 5]).replace(/[^\x20-\x7e]/g, '?').slice(0, 64);
    table.set(processPid, { pid: processPid, ppid, pgid, stat: match[linux ? 5 : 4].slice(0, 16), comm,
      ...(linux ? { sid, wchan: match[6].replace(/[^A-Za-z0-9_.:-]/g, '?').slice(0, 64) } : {}) });
  }
  if (table.get(pid)?.ppid !== process.pid) return { status: 'unavailable', reason: 'owned-child-not-present' };
  const processes = [], pending = [pid], seen = new Set();
  while (pending.length && processes.length < 32) {
    const current = pending.shift();
    if (seen.has(current)) continue;
    seen.add(current);
    processes.push(table.get(current));
    for (const row of table.values()) if (row.ppid === current && !seen.has(row.pid)) pending.push(row.pid);
  }
  return { status: 'observed', rootPid: pid, processes, truncated: pending.length > 0,
    scope: 'Best-effort direct-child/descendant metadata; PID lifetime and process-table races remain possible.' };
}

export function observeTestChild(child) {
  const startedAt = performance.now();
  let spawnedAt = null, exitedAt = null, closedAt = null;
  let stdout = '', stderr = '', spawnError = null;
  child.once('spawn', () => { spawnedAt = performance.now(); });
  child.once('exit', () => { exitedAt = performance.now(); });
  child.stdout.on('data', data => { stdout += data; });
  child.stderr.on('data', data => { stderr += data; });
  const done = new Promise((resolve, reject) => {
    child.once('error', error => { spawnError = error.message; reject(error); });
    child.once('close', (status, signal) => {
      closedAt = performance.now();
      resolve({ status, signal, stdout, stderr });
    });
  });
  // Readiness may be awaited before completion. Preserve the rejected promise
  // for its caller without an unhandled rejection on an early spawn failure.
  void done.catch(() => {});
  const stream = readable => ({ ended: readable.readableEnded,
    closed: readable.closed, destroyed: readable.destroyed });
  return { child, done, snapshot: () => ({ stdout, stderr, pid: child.pid ?? null,
    startedAt, spawnedAt, exitedAt, closedAt, observedAt: performance.now(),
    exitCode: child.exitCode, signalCode: child.signalCode, spawnError,
    processState: closedAt !== null ? 'closed' : exitedAt !== null ? 'exited-awaiting-close'
      : spawnError !== null ? 'spawn-error' : 'awaiting-exit',
    stdoutStream: stream(child.stdout), stderrStream: stream(child.stderr) }) };
}

export async function completedTestChild(workerProcess, {
  timeoutMs = 30000, collectProcessMetadata = testChildProcessMetadata,
} = {}) {
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) throw new Error('Positive test-child deadline required');
  let timer;
  try {
    return await Promise.race([workerProcess.done, new Promise((_, reject) => {
      timer = setTimeout(() => {
        const { stdout, stderr, ...lifecycle } = workerProcess.snapshot();
        const snapshot = { ...lifecycle, stdoutCharacters: stdout.length, stderrCharacters: stderr.length,
          stdoutTail: stdout.slice(-4000), stderrTail: stderr.slice(-4000) };
        const error = new Error(`Worker did not stop within ${timeoutMs / 1000}s\nWorker completion snapshot: ${JSON.stringify(snapshot)}`);
        error.workerSnapshot = snapshot;
        // Lock in failure at the original deadline FIRST. Bounded synchronous
        // diagnostics finish before the caller's cleanup, but no late child
        // completion can win the race or turn this timeout into success.
        reject(error);
        try { error.workerProcessMetadata = collectProcessMetadata(snapshot.pid); }
        catch { error.workerProcessMetadata = { status: 'unavailable', reason: 'diagnostic-collector-failed' }; }
      }, timeoutMs);
    })]);
  } finally { clearTimeout(timer); }
}
