import assert from 'node:assert/strict';
import test from 'node:test';
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

// Execute the real fixture's finalizer, without its database/application setup.
const source = readFileSync(new URL('./experiment-http-runtime.mjs', import.meta.url), 'utf8');
const marker = '\n} finally {';
const start = source.lastIndexOf(marker);
assert.ok(start >= 0, 'the HTTP fixture must retain its top-level finalizer');
const cleanup = source.slice(start + marker.length, source.lastIndexOf('}'));

function run(mode, finalizer = cleanup) {
  const program = `
    import assert from 'node:assert/strict';
    import { spawn } from 'node:child_process';
    import { once } from 'node:events';
    import { mkdtempSync, openSync, closeSync, fstatSync } from 'node:fs';
    import { tmpdir } from 'node:os';
    import { join } from 'node:path';
    const children = [], sessions = [];
    const fd = openSync(join(mkdtempSync(join(tmpdir(), 'tdf-experiment-cleanup-')), 'fixture.log'), 'w', 0o600);
    const files = [fd];
    async function launch(kind) {
      const code = kind === 'normal'
        ? "process.stdout.write('READY'); process.exit(7);"
        : (kind === 'slow' ? "process.on('SIGTERM', () => setTimeout(() => process.exit(0), 80));" : '')
          + "process.stdout.write('READY'); setTimeout(() => process.exit(0), 1500);";
      const child = spawn(process.execPath, ['-e', code], { stdio: ['ignore', 'pipe', 'pipe'] });
      const closed = once(child, 'exit');
      children.push(child); sessions.push(closed);
      await once(child.stdout, 'data');
      return { child, closed };
    }
    const mode = ${JSON.stringify(mode)};
    if (mode === 'live') {
      await launch('slow');
      await launch('fast');
    } else {
      const session = await launch(mode === 'normal' ? 'normal' : 'fast');
      if (mode !== 'normal') session.child.kill('SIGTERM');
      await session.closed;
      if (mode !== 'normal') {
        assert.equal(session.child.exitCode, null);
        assert.equal(session.child.signalCode, 'SIGTERM');
      }
    }
    try {
      if (mode === 'failure') throw new Error('original synthetic HTTP assertion');
    } finally {
      ${finalizer}
    }
    await Promise.all(sessions);
    for (const child of children) assert.ok(child.exitCode !== null || child.signalCode !== null);
    assert.throws(() => fstatSync(fd), { code: 'EBADF' });
    console.log('CLEANUP_COMPLETE');
  `;
  return spawnSync(process.execPath, ['--input-type=module', '--eval', program], {
    encoding: 'utf8', timeout: 10_000,
  });
}

function completed(result) {
  assert.equal(result.error, undefined);
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /CLEANUP_COMPLETE/);
}

test('already signalled fixtures finish cleanup and close their log descriptor', () => {
  completed(run('signalled'));
});

test('already normally exited fixtures finish cleanup', () => {
  completed(run('normal'));
});

test('concurrent fixture exits do not lose a later child exit event', () => {
  completed(run('live'));
});

test('cleanup preserves an existing functional assertion failure', () => {
  const result = run('failure');
  assert.equal(result.error, undefined);
  assert.equal(result.status, 1, result.stderr);
  assert.match(result.stderr, /original synthetic HTTP assertion/);
  assert.doesNotMatch(result.stdout, /CLEANUP_COMPLETE/);
});

test('the historical exit-code-only guard reproduces unresolved-await exit 13', () => {
  const historical = cleanup.replaceAll(' && child.signalCode === null', '');
  const result = run('signalled', historical);
  assert.equal(result.error, undefined);
  assert.equal(result.status, 13, result.stderr);
  assert.doesNotMatch(result.stdout, /CLEANUP_COMPLETE/);
});
