// Synthetic test children only. A process exit and drained output are separate
// milestones: descendants can retain the parent's stdout/stderr descriptors.
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

export async function completedTestChild(workerProcess, { timeoutMs = 30000 } = {}) {
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
        reject(error);
      }, timeoutMs);
    })]);
  } finally { clearTimeout(timer); }
}
