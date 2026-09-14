import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { test } from 'node:test';

import {
  assertTokenValid,
  checkTokenStatus,
} from '../refresh-instagram-token.mjs';
import { checkToken as checkMessagingToken } from '../check-messaging-token.mjs';

function response({ ok, status, data, retryAfter = null }) {
  return {
    ok,
    status,
    statusText: ok ? 'OK' : 'Bad Request',
    headers: { get: name => name === 'retry-after' ? retryAfter : null },
    json: async () => data,
  };
}

test('token checks retry transient Meta errors and ultimately fail closed', async () => {
  let calls = 0;
  const delays = [];
  const fetchImpl = async () => {
    calls += 1;
    return response({
      ok: false,
      status: 400,
      data: { error: { code: 2, message: 'Temporary system error' } },
    });
  };

  await assert.rejects(
    checkTokenStatus('redacted', {
      fetchImpl,
      maxAttempts: 3,
      random: () => 0.5,
      sleep: async delay => delays.push(delay),
    }),
    /Temporary system error/
  );

  assert.equal(calls, 3);
  assert.deepEqual(delays, [1000, 2000]);
});

test('token checks recover after a bounded transient failure', async () => {
  const responses = [
    response({
      ok: false,
      status: 503,
      retryAfter: '0',
      data: { error: { message: 'Service unavailable' } },
    }),
    response({
      ok: true,
      status: 200,
      data: { data: { is_valid: true, scopes: ['instagram_basic'] } },
    }),
  ];

  const status = await checkTokenStatus('redacted', {
    fetchImpl: async () => responses.shift(),
    sleep: async () => {},
  });

  assert.equal(status.isValid, true);
  assert.equal(status.daysUntilExpiry, Infinity);
});

test('an invalid token can never be classified as healthy', async () => {
  const status = await checkTokenStatus('redacted', {
    fetchImpl: async () => response({
      ok: true,
      status: 200,
      data: { data: { is_valid: false } },
    }),
  });

  assert.throws(() => assertTokenValid(status), /access token is invalid/);
});

test('an invalid messaging token fails even when Meta reports no expiration', async () => {
  const result = await checkMessagingToken('redacted', 'Test Token', {
    appId: 'app-id',
    appSecret: 'app-secret',
    fetchImpl: async () => ({
      json: async () => ({ data: { is_valid: false } }),
    }),
  });

  assert.deepEqual(result, { ok: false, error: 'invalid' });
});

test('token maintenance scripts neither invoke a shell nor log token prefixes', async () => {
  const scripts = await Promise.all([
    readFile(new URL('../refresh-instagram-token.mjs', import.meta.url), 'utf8'),
    readFile(new URL('../check-messaging-token.mjs', import.meta.url), 'utf8'),
  ]);

  for (const source of scripts) {
    assert.doesNotMatch(source, /execSync\s*\(/);
    assert.doesNotMatch(source, /token (?:prefix|substring)/i);
  }
});
