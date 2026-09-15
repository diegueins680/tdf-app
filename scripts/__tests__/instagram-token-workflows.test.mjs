import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { test } from 'node:test';
import { spawnSync } from 'node:child_process';

import {
  assertCheckConfiguration,
  assertTokenValid,
  checkInstagramToken as checkInstagramTokenImpl,
  checkTokenStatus as checkTokenStatusImpl,
} from '../refresh-instagram-token.mjs';
import {
  checkToken as checkMessagingToken,
  parseReadOnly,
  runMessagingTokenCli,
  runMessagingTokenMaintenance,
} from '../check-messaging-token.mjs';

test('manual messaging validation invokes an explicit read-only command', async () => {
  const workflow = await readFile(
    new URL('../../.github/workflows/check-messaging-token.yml', import.meta.url), 'utf8'
  );
  assert.match(workflow, /node scripts\/check-messaging-token\.mjs --check/);
});

test('messaging CLI preserves legacy maintenance and rejects ambiguous arguments', () => {
  assert.equal(parseReadOnly([]), false);
  assert.equal(parseReadOnly(['--check']), true);
  for (const args of [['--chek'], ['--check', '--refresh'], ['--check', '--check']]) {
    assert.throws(() => parseReadOnly(args), /Usage:/);
  }
});

test('read-only messaging checks both tokens but never refreshes unhealthy tokens', async () => {
  const healthy = { ok: true, expiringSoon: false };
  for (const unhealthy of [
    { ok: false, error: 'missing' },
    { ok: false, error: 'invalid' },
    { ok: false, error: 'missing_credentials' },
    { ok: false, error: 'provider_failure' },
    { ok: false, expired: true },
    { ok: true, expiringSoon: true },
  ]) {
    for (const failedIndex of [0, 1]) {
      const checked = [];
      let refreshes = 0;
      const code = await runMessagingTokenMaintenance({
        readOnly: true,
        instagramToken: 'test-instagram',
        facebookToken: 'test-facebook',
        check: async token => {
          checked.push(token);
          return checked.length - 1 === failedIndex ? unhealthy : healthy;
        },
        refresh: async () => { refreshes++; throw new Error('Unexpected credential mutation'); },
      });
      assert.equal(code, 1);
      assert.deepEqual(checked, ['test-instagram', 'test-facebook']);
      assert.equal(refreshes, 0);
    }
  }
});

test('healthy messaging tokens pass without refresh in either mode', async () => {
  for (const readOnly of [true, false]) {
    let checks = 0;
    let refreshes = 0;
    assert.equal(await runMessagingTokenMaintenance({
      readOnly,
      check: async () => { checks++; return { ok: true, expiringSoon: false }; },
      refresh: async () => { refreshes++; },
    }), 0);
    assert.equal(checks, 2);
    assert.equal(refreshes, 0);
  }
});

test('read-only invariant holds for every two-token validity/expiry combination', async () => {
  for (let state = 0; state < 16; state++) {
    const instagram = { ok: Boolean(state & 1), expiringSoon: Boolean(state & 2) };
    const facebook = { ok: Boolean(state & 4), expiringSoon: Boolean(state & 8) };
    let checks = 0;
    let refreshes = 0;
    const code = await runMessagingTokenMaintenance({
      readOnly: true,
      check: async () => ++checks === 1 ? instagram : facebook,
      refresh: async () => { refreshes++; },
    });
    assert.equal(code, instagram.ok && !instagram.expiringSoon && facebook.ok && !facebook.expiringSoon ? 0 : 1);
    assert.equal(checks, 2);
    assert.equal(refreshes, 0, `Forbidden refresh in state ${state}`);
  }
});

test('legacy messaging maintenance still refreshes when needed and verifies the result', async () => {
  const checked = [];
  const refreshed = [];
  assert.equal(await runMessagingTokenMaintenance({
    instagramToken: 'test-instagram',
    facebookToken: 'test-facebook',
    check: async token => {
      checked.push(token);
      return { ok: true, expiringSoon: token === 'test-instagram' };
    },
    refresh: async token => { refreshed.push(token); return 'test-replacement'; },
  }), 0);
  assert.deepEqual(refreshed, ['test-instagram']);
  assert.deepEqual(checked, ['test-instagram', 'test-facebook', 'test-replacement']);
});

test('legacy messaging maintenance retains missing-token and refresh failure exits', async () => {
  assert.equal(await runMessagingTokenMaintenance({
    instagramToken: '',
    check: async () => ({ ok: false }),
    refresh: async () => assert.fail('Cannot refresh a missing token'),
  }), 1);
  for (const failRefresh of [true, false]) {
    assert.equal(await runMessagingTokenMaintenance({
      instagramToken: 'test-instagram',
      check: async () => ({ ok: false }),
      refresh: async () => {
        if (failRefresh) throw new Error('Simulated refresh failure');
        return 'test-replacement';
      },
    }), 1);
  }
});

test('read-only CLI fails missing credentials without entering maintenance', async () => {
  // Exercise the actual CLI entry with the real checker and explicitly absent
  // tokens. No real credentials or external requests are used by this test.
  let refreshes = 0;
  assert.equal(await runMessagingTokenCli(['--check'], {
    readOnly: false, // The CLI flag must override even a conflicting option.
    instagramToken: '',
    facebookToken: '',
    refresh: async () => { refreshes++; },
  }), 1);
  assert.equal(refreshes, 0);
  await assert.rejects(runMessagingTokenCli(['--check', '--refresh'], {
    check: async () => assert.fail('Invalid CLI arguments must fail before token checks'),
    refresh: async () => assert.fail('Invalid CLI arguments must fail before refresh'),
  }), /Usage:/);
  const script = await readFile(new URL('../check-messaging-token.mjs', import.meta.url), 'utf8');
  assert.match(script, /runMessagingTokenCli\(process\.argv\.slice\(2\)\)\.then\(code => \{\s*process\.exitCode = code;/);
});

test('messaging workflow separates read-only credentials from unchanged maintenance', async () => {
  const workflow = await readFile(
    new URL('../../.github/workflows/check-messaging-token.yml', import.meta.url), 'utf8'
  );
  const step = name => workflow.split(`      - name: ${name}\n`)[1]?.split('\n      - name:')[0];
  const readOnly = step('Check Messaging Token (read-only)');
  const maintenance = step('Check/Refresh Messaging Token');
  assert.match(workflow, /permissions:\n  contents: read/);
  assert.match(workflow, /cron: '0 \* \* \* \*'/);
  assert.match(readOnly, /if: github.event_name == 'workflow_dispatch' && inputs.action == 'check'/);
  assert.match(readOnly, /run: node scripts\/check-messaging-token\.mjs --check/);
  assert.doesNotMatch(readOnly, /FLY_|flyctl/);
  assert.match(maintenance, /if: github.event_name == 'schedule' \|\| inputs.action == 'refresh'/);
  assert.match(maintenance, /FLY_API_TOKEN: \$\{\{ secrets.FLY_API_TOKEN \}\}/);
  assert.match(maintenance, /run: node scripts\/check-messaging-token\.mjs\s*$/);
  assert.match(step('Install Fly CLI'), /if: github.event_name == 'schedule' \|\| inputs.action == 'refresh'/);
  assert.match(step('Validate requested action'), /Unsupported messaging-token action'; exit 1/);
  // The Fly credential must occur only on the guarded maintenance step.
  assert.equal(workflow.match(/FLY_API_TOKEN:/g)?.length, 1);
  assert.doesNotMatch(workflow.split('    steps:')[0], /\benv:/);
});

test('messaging workflow rejects unsupported actions before any credential step', async () => {
  const workflow = await readFile(
    new URL('../../.github/workflows/check-messaging-token.yml', import.meta.url), 'utf8'
  );
  const firstStep = workflow.split('      - name: Validate requested action\n')[1].split('\n      - name:')[0];
  const guard = firstStep.split('        run: |\n')[1];
  assert.ok(guard);
  for (const [event, action, status] of [
    ['schedule', '', 0],
    ['workflow_dispatch', 'check', 0],
    ['workflow_dispatch', 'refresh', 0],
    ['workflow_dispatch', '', 1],
    ['workflow_dispatch', 'unknown', 1],
    ['pull_request', 'refresh', 1],
  ]) {
    const result = spawnSync('/bin/bash', ['-e', '-c', guard], {
      env: { WORKFLOW_EVENT: event, REQUESTED_ACTION: action }, encoding: 'utf8', timeout: 10000,
    });
    assert.equal(result.error, undefined);
    assert.equal(result.status, status, `${event}:${action}`);
  }
});

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
