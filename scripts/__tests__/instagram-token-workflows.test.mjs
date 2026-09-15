import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { test } from 'node:test';

import {
  assertCheckConfiguration,
  assertTokenValid,
  checkInstagramToken as checkInstagramTokenImpl,
  checkTokenStatus as checkTokenStatusImpl,
} from '../refresh-instagram-token.mjs';
import { checkToken as checkMessagingToken } from '../check-messaging-token.mjs';

const inspector = { appId: 'parent-app', appSecret: 'parent-secret', expectedAppId: 'instagram-app' };
const checkTokenStatus = (token, options) => checkTokenStatusImpl(token, { ...inspector, ...options });
const checkInstagramToken = (token, options) => checkInstagramTokenImpl(token, { ...inspector, ...options });

function response({ ok, status, data, retryAfter = null }) {
  return {
    ok,
    status,
    statusText: ok ? 'OK' : 'Bad Request',
    headers: { get: name => name === 'retry-after' ? retryAfter : null },
    json: async () => data,
  };
}

test('Instagram account validation reports an expired token before attempting app authentication', async () => {
  const requests = [];
  await assert.rejects(checkInstagramToken('redacted', {
    fetchImpl: async url => {
      requests.push(new URL(url));
      return response({ ok: false, status: 400, data: {
        error: { code: 190, message: 'Error validating access token: Session has expired.' },
      } });
    },
    sleep: async () => assert.fail('Expired tokens must not be retried'),
  }), /Session has expired/);
  assert.equal(requests.length, 1);
  assert.equal(requests[0].origin, 'https://graph.instagram.com');
  assert.equal(requests[0].pathname, '/v26.0/me');
  assert.equal(requests[0].searchParams.get('fields'), 'user_id');
});

test('successful Instagram account access still requires valid token metadata', async () => {
  const hosts = [];
  const status = await checkInstagramToken('redacted', {
    fetchImpl: async url => {
      hosts.push(new URL(url).hostname);
      return response({ ok: true, status: 200, data: hosts.length === 1
        ? { user_id: '123' }
        : { data: { is_valid: false } },
      });
    },
  });
  assert.deepEqual(hosts, ['graph.instagram.com', 'graph.facebook.com']);
  assert.throws(() => assertTokenValid(status), /access token is invalid/);
});

test('an empty or ambiguous Instagram account response fails closed', async () => {
  for (const data of [{}, { data: [] }, { data: [{ user_id: '1' }, { user_id: '2' }] }]) {
    let calls = 0;
    await assert.rejects(checkInstagramToken('redacted', {
      fetchImpl: async () => { calls++; return response({ ok: true, status: 200, data }); },
    }), /no valid user ID/);
    assert.equal(calls, 1);
  }
});

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

test('Meta system errors without an API code still receive bounded retries', async () => {
  let calls = 0;
  const fetchImpl = async () => {
    calls += 1;
    return response({
      ok: false,
      status: 400,
      data: { error: { message: 'Cannot get application info due to a system error.' } },
    });
  };

  await assert.rejects(
    checkTokenStatus('redacted', {
      fetchImpl,
      maxAttempts: 2,
      sleep: async () => {},
    }),
    /system error.*HTTP 400/
  );
  assert.equal(calls, 2);
});

test('Meta authentication errors fail immediately even when their wording mentions a system error', async () => {
  let calls = 0;
  const delays = [];

  await assert.rejects(
    checkTokenStatus('redacted', {
      fetchImpl: async () => {
        calls += 1;
        return response({
          ok: false,
          status: 400,
          data: {
            error: {
              code: 190,
              message: 'Error validating application. Cannot get application info due to a system error.',
            },
          },
        });
      },
      maxAttempts: 3,
      sleep: async delay => delays.push(delay),
    }),
    /Error validating application.*HTTP 400, API code 190/
  );

  assert.equal(calls, 1);
  assert.deepEqual(delays, []);
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
      data: { data: { is_valid: true, app_id: 'instagram-app', expires_at: 0, scopes: ['instagram_basic'] } },
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

test('metadata inspection authenticates the parent app but verifies the Instagram child owner', async () => {
  const status = await checkTokenStatus('subject+token&value', {
    fetchImpl: async rawUrl => {
      const url = new URL(rawUrl);
      assert.equal(url.origin, 'https://graph.facebook.com');
      assert.equal(url.pathname, '/v26.0/debug_token');
      assert.equal(url.searchParams.get('input_token'), 'subject+token&value');
      assert.equal(url.searchParams.get('access_token'), 'parent-app|parent-secret');
      return response({ ok: true, status: 200, data: { data: {
        is_valid: true, app_id: 'instagram-app', expires_at: 0,
      } } });
    },
  });
  assert.equal(status.isValid, true);
  for (const metadata of [
    { is_valid: true, app_id: 'unrelated-app', expires_at: 0 },
    { is_valid: true, app_id: 'instagram-app' },
  ]) {
    await assert.rejects(checkTokenStatus('redacted', {
      fetchImpl: async () => response({ ok: true, status: 200, data: { data: metadata } }),
    }), /different application|no authoritative expiration/);
  }
});

test('metadata cannot mark an expired token or expired data grant healthy', async () => {
  const now = Math.floor(Date.now() / 1000);
  for (const deadlines of [
    { expires_at: now - 60 },
    { expires_at: now + 3600, data_access_expires_at: now - 60 },
  ]) {
    const status = await checkTokenStatus('redacted', {
      fetchImpl: async () => response({ ok: true, status: 200, data: { data: {
        is_valid: true, app_id: 'instagram-app', ...deadlines,
      } } }),
    });
    assert.throws(() => assertTokenValid(status), /access token is invalid/);
  }
});

test('token validation requires an explicit matching Meta application ID', () => {
  assert.throws(
    () => assertCheckConfiguration({ token: 'token', appSecret: 'secret' }),
    /INSTAGRAM_APP_ID environment variable is required/
  );
  assert.doesNotThrow(() => assertCheckConfiguration({
    token: 'token',
    appId: 'app-id',
    appSecret: 'secret',
  }));
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

test('an absent optional Slack webhook cannot mask the original workflow failure', async () => {
  const workflow = await readFile(
    new URL('../../.github/workflows/refresh-instagram-token.yml', import.meta.url),
    'utf8'
  );

  assert.match(workflow, /if: failure\(\) && steps\.notification-config\.outputs\.enabled == 'true'/);
  assert.match(workflow, /if: failure\(\) && steps\.notification-config\.outputs\.enabled == 'false'/);
  assert.match(workflow, /refresh-token:\n    runs-on: ubuntu-latest\n    steps:/);
  assert.match(workflow, /uses: slackapi\/slack-github-action@v1\.24\.0\n        env:\n          SLACK_WEBHOOK_URL:/);
  assert.match(workflow, /INSTAGRAM_APP_ID: \$\{\{ secrets\.INSTAGRAM_APP_ID \}\}/);
  assert.doesNotMatch(workflow, /INSTAGRAM_APP_ID[^\n]*\|\|/);
});
