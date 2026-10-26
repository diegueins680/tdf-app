import { spawn } from 'node:child_process';

// Interactive test processes must fail readiness when they die or stop making
// progress. `closed` always resolves; callers can await readiness first without
// an unhandled rejection from a second promise on spawn failure.
export function testProcessSession(command, args, { timeoutMs = 15_000 } = {}) {
  const child = spawn(command, args, { stdio: ['pipe', 'pipe', 'pipe'] });
  let output = '', errors = '', failure = null, ended = false, exitCode = null;
  const waiters = new Set();
  const notify = () => { for (const check of [...waiters]) check(); };
  child.stdout.on('data', data => { output += data; notify(); });
  child.stderr.on('data', data => { errors += data; });
  child.stdin.on('error', error => { failure = error; notify(); });
  const closed = new Promise(resolve => {
    child.on('error', error => {
      failure = error;
      ended = true;
      notify();
      resolve(null);
    });
    child.on('close', code => {
      ended = true;
      exitCode = code;
      notify();
      resolve(code);
    });
  });
  const until = marker => new Promise((resolve, reject) => {
    const finish = error => {
      clearTimeout(timer);
      waiters.delete(check);
      if (error) reject(error); else resolve();
    };
    const check = () => {
      if (output.includes(marker)) finish();
      else if (failure || ended) finish(new Error(
        `Test process ended before ${marker}: ${failure?.message ?? (errors.trim() || `exit ${exitCode}`)}`,
      ));
    };
    const timer = setTimeout(() => {
      child.kill('SIGKILL');
      finish(new Error(`Test process timed out waiting for ${marker}`));
    }, timeoutMs);
    waiters.add(check);
    check();
  });
  return { child, closed, until, errors: () => errors };
}
