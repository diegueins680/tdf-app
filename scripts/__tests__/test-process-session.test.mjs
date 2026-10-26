import assert from 'node:assert/strict';
import test from 'node:test';
import { testProcessSession } from '../lib/test-process-session.mjs';

const nodeSession = source => testProcessSession(process.execPath, ['-e', source]);

test('readiness accepts output split across chunks and already emitted output', async () => {
  const session = nodeSession("process.stdout.write('hea'); setTimeout(() => process.stdout.write('der_ready'), 10);");
  await session.until('header_ready');
  assert.equal(await session.closed, 0);
  await session.until('header_ready');
});

test('an early SQL-style error rejects readiness with its diagnostic', async () => {
  const session = nodeSession("process.stderr.write('synthetic SQL failure'); process.exitCode = 3;");
  await assert.rejects(session.until('header_ready'), /synthetic SQL failure/);
  assert.equal(await session.closed, 3);
});

test('successful exit without a marker also rejects readiness', async () => {
  const session = nodeSession('process.exitCode = 0;');
  await assert.rejects(session.until('header_ready'), /ended before header_ready.*exit 0/);
  assert.equal(await session.closed, 0);
});

test('spawn errors reject readiness without an unhandled closed promise', async () => {
  const session = testProcessSession('/nonexistent/tdf-test-process', []);
  await assert.rejects(session.until('header_ready'), /ENOENT/);
  assert.equal(await session.closed, null);
});

test('a stalled child has a bounded readiness wait and is terminated', async () => {
  const session = testProcessSession(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { timeoutMs: 200 });
  await assert.rejects(session.until('header_ready'), /timed out/);
  assert.equal(await session.closed, null);
});
