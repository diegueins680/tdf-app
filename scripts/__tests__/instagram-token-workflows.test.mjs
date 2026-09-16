import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtemp, readFile, rm, stat } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';
import {
  bootstrapLifecycle, checkLifecycle, loadBundle, openBundle, refreshLifecycle,
  parseMetaResponse, requestMeta, retryDelay, saveBundle, sealBundle, validateBundle,
} from '../lib/instagram-token-lifecycle.mjs';
import { runLifecycle } from '../refresh-instagram-token.mjs';
import { findLifecycleRun, readLifecycleMetadata } from '../instagram-lifecycle-artifact.mjs';
import { spawnSync } from 'node:child_process';
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

const NOW = 2000000000;
const DAY = 86400;
const config = { appId: '123', appSecret: 'unit-test-secret-not-a-live-credential', context: 'example/repo', expectedUserId: '456' };
const permissions = ['instagram_business_basic', 'instagram_business_manage_messages'];
const response = (data, status = 200, retryAfter = null) => ({
  ok: status >= 200 && status < 300, status,
  headers: { get: () => retryAfter }, text: async () => JSON.stringify(data), json: async () => data,
});
const grant = () => ({ data: [{ access_token: 'test-short', user_id: '456', permissions: permissions.join(',') }] });
const profile = () => ({ id: '456', user_id: '456' });
const long = (extra = {}) => ({ access_token: 'test-long', token_type: 'bearer', expires_in: 5183944, ...extra });
const sequence = values => async () => {
  assert.ok(values.length, 'Unexpected network request');
  return response(values.shift());
};
const setupOptions = { ...config, code: 'test-single-use-code', redirectUri: 'https://example.test/callback' };

test('numeric provider account IDs retain exact source digits beyond MAX_SAFE_INTEGER', () => {
  const result = parseMetaResponse('{"user_id":17841445628242005,"expires_in":5183944}');
  assert.equal(result.user_id, '17841445628242005');
  assert.equal(result.expires_in, 5183944);
  assert.equal(parseMetaResponse('{"data":[{"user_id":456}]}').data[0].user_id, '456');
  assert.equal(parseMetaResponse('{"user_id":"17841445628242005"}').user_id, '17841445628242005');
});

test('numeric account IDs reject fractional, negative and exponent encodings', () => {
  assert.throws(() => parseMetaResponse('{"user_id":1.5}'), /non-canonical/);
  assert.throws(() => parseMetaResponse('{"user_id":-1}'), /non-canonical/);
  assert.throws(() => parseMetaResponse('{"user_id":1e3}'), /non-canonical/);
});

test('bootstrap preserves numeric wire identity through grant, checkpoint and live verification', async () => {
  let calls = 0;
  const wireGrant = '{"access_token":"test-short","user_id":17841445628242005,"permissions":"instagram_business_basic"}';
  const result = await bootstrapLifecycle({ ...setupOptions, expectedUserId: '17841445628242005' }, {
    clock: () => NOW,
    fetchImpl: async () => {
      calls++;
      if (calls === 3) return response(long());
      return { ok: true, status: 200, text: async () => calls === 1 ? wireGrant : '{"id":17841445628242005,"user_id":17841445628242005}' };
    },
  });
  assert.equal(calls, 4);
  assert.equal(result.userId, '17841445628242005');
  assert.equal(result.authorization.userId, result.userId);
});
function bundle() {
  return {
    version: 1, token: 'test-long', tokenSha256: createHash('sha256').update('test-long').digest('hex'),
    appId: '123', userId: '456', permissions, issuedAt: NOW - DAY, receivedAt: NOW - DAY,
    expiresIn: 5183944, expiresAt: NOW - DAY + 5183944, dataAccessExpiresAt: null,
    authorization: { method: 'instagram_authorization_code', appId: '123', userId: '456', scopedUserId: '456', authorizedAt: NOW - DAY },
  };
}

test('OAuth bootstrap binds app ID, app secret, callback, provider user and permissions before recording expiry', async () => {
  const calls = [];
  const answers = [grant(), profile(), long(), { user_id: '456' }];
  const result = await bootstrapLifecycle(setupOptions, { clock: () => NOW, fetchImpl: async (url, init) => {
    calls.push(new URL(url));
    assert.equal(init.redirect, 'error');
    if (calls.length === 1) {
      assert.equal(url, 'https://api.instagram.com/oauth/access_token');
      assert.equal(init.method, 'POST');
      assert.equal(init.body.get('client_id'), '123');
      assert.equal(init.body.get('client_secret'), config.appSecret);
      assert.equal(init.body.get('redirect_uri'), setupOptions.redirectUri);
      assert.equal(init.body.get('grant_type'), 'authorization_code');
      assert.equal(init.body.get('code'), setupOptions.code);
    }
    return response(answers.shift());
  } });
  assert.equal(result.expiresAt, NOW + 5183944);
  assert.equal(result.authorization.appId, '123');
  assert.deepEqual(result.permissions, permissions);
  assert.deepEqual(calls.map(x => x.hostname), ['api.instagram.com', 'graph.instagram.com', 'graph.instagram.com', 'graph.instagram.com']);
  assert.equal(calls[1].searchParams.get('fields'), 'id,user_id');
  assert.equal(calls[2].searchParams.get('grant_type'), 'ig_exchange_token');
  assert.equal(calls[2].searchParams.get('client_secret'), config.appSecret);
  assert.equal(calls[3].searchParams.get('fields'), 'user_id');
});

test('expiry starts before the exchange request, never after network latency', async () => {
  const ticks = [NOW, NOW + 1, NOW + 19, NOW + 20];
  const result = await bootstrapLifecycle(setupOptions, { clock: () => ticks.shift(), fetchImpl: sequence([grant(), profile(), long(), { user_id: '456' }]) });
  assert.equal(result.expiresAt, NOW + 1 + 5183944);
  assert.equal(result.receivedAt, NOW + 19);
});

for (const [name, changed] of [
  ['missing code', { code: '' }], ['missing account pin', { expectedUserId: undefined }],
  ['non-HTTPS callback', { redirectUri: 'http://example.test/callback' }],
  ['callback userinfo', { redirectUri: 'https://user:pass@example.test/callback' }],
  ['missing app', { appId: '' }], ['missing secret', { appSecret: '' }],
]) test('bootstrap rejects ' + name + ' before network access', async () => {
  await assert.rejects(bootstrapLifecycle({ ...setupOptions, ...changed }, { fetchImpl: async () => assert.fail('Must not call Meta') }));
});

for (const [name, data] of [
  ['missing identity', {}], ['empty account array', { data: [] }],
  ['ambiguous accounts', { data: [{ user_id: '456' }, { user_id: '456' }] }],
  ['missing token', { ...grant().data[0], access_token: '' }],
  ['missing permissions', { ...grant().data[0], permissions: undefined }],
  ['missing basic grant', { ...grant().data[0], permissions: 'instagram_business_manage_messages' }],
]) test('bootstrap rejects provider ' + name, async () => {
  let calls = 0;
  await assert.rejects(bootstrapLifecycle(setupOptions, { clock: () => NOW, fetchImpl: async () => { calls++; return response(data); } }));
  assert.equal(calls, 1);
});

for (const expires_in of [undefined, 0, -1, '5183944', 1.5, Infinity, 5184001]) {
  test('provider lifetime fails closed: ' + String(expires_in), async () => {
    await assert.rejects(bootstrapLifecycle(setupOptions, { clock: () => NOW, fetchImpl: sequence([grant(), profile(), long({ expires_in })]) }), /authoritative token lifetime/);
  });
}

test('failed code exchange is not retried and cannot leak provider-echoed credentials', async () => {
  let calls = 0;
  await assert.rejects(bootstrapLifecycle(setupOptions, { fetchImpl: async () => {
    calls++; return response({ error: { code: 2, message: setupOptions.code + config.appSecret } }, 400);
  }, sleep: async () => assert.fail('Single-use grant must not retry') }), error => {
    assert.equal(error.message, 'Meta request failed (HTTP 400, API code 2)');
    return true;
  });
  assert.equal(calls, 1);
});

test('bootstrap binds distinct app-scoped and professional IDs using the same short-token profile', async () => {
  const result = await bootstrapLifecycle(setupOptions, { clock: () => NOW, fetchImpl: sequence([
    { ...grant().data[0], user_id: '789' }, { id: '789', user_id: '456' }, long(), { user_id: '456' },
  ]) });
  assert.equal(result.userId, '456');
  assert.equal(result.authorization.scopedUserId, '789');
  assert.equal(result.authorization.userId, '456');
});

test('a professional account mismatch fails before long-lived exchange despite a valid scoped mapping', async () => {
  const answers = [grant(), { id: '456', user_id: '789' }];
  await assert.rejects(bootstrapLifecycle(setupOptions, { clock: () => NOW, fetchImpl: sequence(answers) }), /different Instagram professional account/);
  assert.equal(answers.length, 0);
});

test('the expected professional account cannot authorize a different or missing grant-scoped identity', async () => {
  await assert.rejects(bootstrapLifecycle(setupOptions, { clock: () => NOW,
    fetchImpl: sequence([grant(), { id: '789', user_id: '456' }]),
  }), /app-scoped identity/);
  await assert.rejects(bootstrapLifecycle(setupOptions, { clock: () => NOW,
    fetchImpl: sequence([grant(), { user_id: '456' }]),
  }), /app-scoped identity/);
});

const invalidBundles = [
  ['missing evidence', () => null],
  ['raw-token mismatch', b => ({ ...b, token: 'different-test-token' })],
  ['wrong app', b => ({ ...b, appId: '789' })],
  ['wrong approved account', b => ({ ...b, userId: '789' })],
  ['missing ownership provenance', b => ({ ...b, authorization: null })],
  ['missing scoped identity evidence', b => ({ ...b, authorization: { ...b.authorization, scopedUserId: undefined } })],
  ['mismatched grant owner', b => ({ ...b, authorization: { ...b.authorization, appId: '789' } })],
  ['future issuance', b => ({ ...b, issuedAt: NOW + 1 })],
  ['future response', b => ({ ...b, receivedAt: NOW + 1 })],
  ['response before issuance', b => ({ ...b, receivedAt: b.issuedAt - 1 })],
  ['invalid grant timestamp', b => ({ ...b, authorization: { ...b.authorization, authorizedAt: NOW + 1 } })],
  ['missing authoritative expiry', b => ({ ...b, expiresIn: undefined })],
  ['fabricated expiry', b => ({ ...b, expiresAt: b.expiresAt + 1 })],
  ['expired token', b => ({ ...b, expiresIn: 1, expiresAt: b.issuedAt + 1 })],
  ['expired data grant', b => ({ ...b, dataAccessExpiresAt: NOW - 1 })],
  ['missing data evidence field', b => { delete b.dataAccessExpiresAt; return b; }],
  ['invalid scopes', b => ({ ...b, permissions: ['unrelated'] })],
];
for (const [name, mutate] of invalidBundles) test('read-only check rejects ' + name + ' without a provider request', async () => {
  await assert.rejects(checkLifecycle(mutate(bundle()), config, { now: NOW, fetchImpl: async () => assert.fail('Invalid evidence must fail before network') }));
});

test('read-only check validates current account access without debugger, refresh, or expiry extension', async () => {
  const input = bundle();
  const before = JSON.stringify(input);
  const status = await checkLifecycle(input, config, { now: NOW, fetchImpl: async (url, init) => {
    assert.equal(new URL(url).pathname, '/v26.0/me');
    assert.equal(new URL(url).origin, 'https://graph.instagram.com');
    assert.equal(init.method, undefined);
    return response({ data: [{ user_id: '456' }] });
  } });
  assert.equal(status.isValid, true);
  assert.equal(JSON.stringify(input), before);
});

for (const data of [{}, { data: [] }, { data: [{ user_id: '456' }, { user_id: '789' }] }, { user_id: '789' }]) {
  test('live invalid, ambiguous or switched account fails closed: ' + JSON.stringify(data), async () => {
    await assert.rejects(checkLifecycle(bundle(), config, { now: NOW, fetchImpl: async () => response(data) }));
  });
}

test('expired/revoked live tokens fail without retries and never report healthy', async () => {
  let calls = 0;
  await assert.rejects(checkLifecycle(bundle(), config, { now: NOW, fetchImpl: async () => {
    calls++; return response({ error: { code: 190, is_transient: true, message: 'System error' } }, 400);
  }, sleep: async () => assert.fail('Authentication is permanent') }), /API code 190/);
  assert.equal(calls, 1);
});

test('read requests use bounded exponential jitter and ultimately fail closed', async () => {
  const delays = [];
  let calls = 0;
  await assert.rejects(requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    fetchImpl: async () => { calls++; return response({ error: { code: 2 } }, 400); },
    random: () => 0.5, sleep: async ms => delays.push(ms),
  }), /API code 2/);
  assert.equal(calls, 3);
  assert.deepEqual(delays, [1000, 2000]);
});

test('read requests recover after one infrastructure error', async () => {
  let calls = 0;
  const data = await requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    fetchImpl: async () => ++calls === 1 ? response({}, 503, '0') : response({ user_id: '456' }),
    sleep: async ms => assert.equal(ms, 0),
  });
  assert.equal(data.user_id, '456');
  assert.equal(calls, 2);
});

test('uncoded Meta system errors retain bounded retry coverage without echoing their message', async () => {
  let calls = 0;
  await assert.rejects(requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    maxAttempts: 2,
    fetchImpl: async () => { calls++; return response({ error: { message: 'Cannot get application info due to a system error: private-test-value' } }, 400); },
    sleep: async () => {},
  }), error => error.message === 'Meta request failed (HTTP 400, API code unknown)');
  assert.equal(calls, 2);
});

test('Retry-After supports seconds/date and never retries sooner than a long server delay', () => {
  assert.equal(retryDelay(1, '2'), 2000);
  assert.equal(retryDelay(1, new Date((NOW + 3) * 1000).toUTCString(), NOW * 1000), 3000);
  assert.throws(() => retryDelay(1, '60'), /longer retry delay/);
});

test('network failures and malformed responses redact underlying sensitive details', async () => {
  await assert.rejects(requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    maxAttempts: 1, fetchImpl: async () => { throw new TypeError('url?access_token=private-test-value'); },
  }), error => error.message === 'Meta network request failed or timed out');
  await assert.rejects(requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    fetchImpl: async () => ({ ok: true, status: 200, text: async () => { throw new Error('private-test-response'); } }),
  }), error => error.message === 'Meta returned malformed JSON');
});

test('refresh retains OAuth ownership and the earliest known data-access deadline', async () => {
  const prior = { ...bundle(), dataAccessExpiresAt: NOW + 1000 };
  const before = JSON.stringify(prior);
  const next = await refreshLifecycle(prior, config, { clock: () => NOW,
    fetchImpl: sequence([{ user_id: '456' }, long({ access_token: 'test-refreshed', data_access_expires_at: NOW + 2000 }), { user_id: '456' }]),
  });
  assert.equal(next.dataAccessExpiresAt, NOW + 1000);
  assert.equal(next.expiresAt, NOW + 5183944);
  assert.deepEqual(next.authorization, prior.authorization);
  assert.notEqual(next.tokenSha256, prior.tokenSha256);
  assert.equal(JSON.stringify(prior), before);
});

test('refresh carries forward a known data-access deadline when response omits it', async () => {
  const next = await refreshLifecycle({ ...bundle(), dataAccessExpiresAt: NOW + 1000 }, config, { clock: () => NOW,
    fetchImpl: sequence([{ user_id: '456' }, long(), { user_id: '456' }]),
  });
  assert.equal(next.dataAccessExpiresAt, NOW + 1000);
});

test('a token younger than 24 hours cannot be refreshed', async () => {
  const young = bundle();
  young.issuedAt = NOW - DAY + 1;
  young.receivedAt = young.issuedAt;
  young.expiresAt = young.issuedAt + young.expiresIn;
  let calls = 0;
  await assert.rejects(refreshLifecycle(young, config, { clock: () => NOW, fetchImpl: async () => { calls++; return response({ user_id: '456' }); } }), /24 hours/);
  assert.equal(calls, 1);
});

test('network delay cannot make a not-yet-24-hour-old token eligible for refresh', async () => {
  const delayed = { ...bundle(), receivedAt: NOW - DAY + 10 };
  await assert.rejects(refreshLifecycle(delayed, config, { clock: () => NOW,
    fetchImpl: sequence([{ user_id: '456' }]),
  }), /24 hours/);
});

test('read-only checks retry malformed gateway responses with a bounded delay', async () => {
  let calls = 0;
  const data = await requestMeta('https://graph.instagram.com/v26.0/me', {}, {
    fetchImpl: async () => ++calls === 1
      ? { status: 503, text: async () => { throw new Error('HTML gateway body'); } }
      : response({ user_id: '456' }),
    sleep: async () => {},
  });
  assert.equal(data.user_id, '456');
  assert.equal(calls, 2);
});

test('failed refresh is not retried and leaves the existing bundle unchanged', async () => {
  const prior = bundle();
  const before = JSON.stringify(prior);
  let calls = 0;
  await assert.rejects(refreshLifecycle(prior, config, { clock: () => NOW, fetchImpl: async () => {
    calls++; return calls === 1 ? response({ user_id: '456' }) : response({ error: { code: 2 } }, 400);
  }, sleep: async () => assert.fail('Mutation must not retry') }));
  assert.equal(calls, 2);
  assert.equal(JSON.stringify(prior), before);
});

test('encrypted state authenticates token/evidence and repository/application context', () => {
  const encrypted = sealBundle(bundle(), config);
  assert.ok(!encrypted.includes('test-long'));
  assert.ok(!encrypted.includes('tokenSha256'));
  assert.deepEqual(openBundle(encrypted, config), bundle());
  for (const changed of [{ context: 'other/repo' }, { appId: '789' }, { appSecret: 'different-unit-test-secret' }]) {
    assert.throws(() => openBundle(encrypted, { ...config, ...changed }), /authentication/);
  }
  const corrupt = JSON.parse(encrypted);
  corrupt.ciphertext = 'AAAA' + corrupt.ciphertext.slice(4);
  assert.throws(() => openBundle(JSON.stringify(corrupt), config), /authentication/);
  assert.throws(() => openBundle(JSON.stringify(bundle()), config), /authentication/);
  assert.notEqual(encrypted, sealBundle(bundle(), config), 'Each write needs a fresh nonce');
});

test('atomic persistence writes only ciphertext with private permissions; check performs no writes', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'tdf-instagram-lifecycle-test-'));
  try {
    const path = join(directory, 'state.enc.json');
    await saveBundle(path, bundle(), config);
    assert.equal((await stat(path)).mode & 0o777, 0o600);
    const before = await readFile(path, 'utf8');
    assert.deepEqual(await loadBundle(path, config), bundle());
    await runLifecycle('--check', {
      INSTAGRAM_APP_ID: config.appId, INSTAGRAM_APP_SECRET: config.appSecret,
      GITHUB_REPOSITORY: config.context, INSTAGRAM_USER_ID: config.expectedUserId,
      INSTAGRAM_LIFECYCLE_STATE_FILE: path,
    }, { now: NOW, fetchImpl: async () => response({ user_id: '456' }) });
    assert.equal(await readFile(path, 'utf8'), before);
    await assert.rejects(loadBundle(join(directory, 'missing'), config), /bootstrap/);
  } finally { await rm(directory, { recursive: true, force: true }); }
});

const producer = (extra = {}) => ({
  id: 20, display_title: 'Instagram lifecycle: setup',
  path: '.github/workflows/refresh-instagram-token.yml', head_sha: 'a'.repeat(40),
  head_repository: { full_name: 'example/repo' }, status: 'completed', conclusion: 'success', ...extra,
});
const lookupConfig = { repository: 'example/repo', sha: 'b'.repeat(40), currentRunId: '30' };
test('checkpoint ancestry lookup projects large comparison responses before the subprocess buffer', () => {
  const endpoint = `repos/example/repo/compare/${'a'.repeat(40)}...${'b'.repeat(40)}`;
  const metadata = readLifecycleMetadata(endpoint, (command, args, options) => {
    assert.equal(command, 'gh');
    assert.equal(options.timeout, 30000);
    assert.equal(options.maxBuffer, 1024 * 1024);
    assert.deepEqual(options.stdio, ['ignore', 'pipe', 'pipe']);
    // Model gh's output projection: an unrelated multi-megabyte diff must never
    // reach execFileSync's bounded stdout capture just to establish ancestry.
    if (args.at(-2) !== '--jq' || args.at(-1) !== '{status}') {
      throw Object.assign(new Error('comparison output exceeded buffer'), { code: 'ENOBUFS' });
    }
    assert.deepEqual(args.slice(0, 2), ['api', endpoint]);
    return JSON.stringify({ status: 'ahead' });
  });
  assert.deepEqual(metadata, { status: 'ahead' });
});
test('checkpoint run inventory projection retains every provenance field', () => {
  const expected = { total_count: 1, workflow_runs: [producer()] };
  assert.deepEqual(readLifecycleMetadata('repos/example/repo/actions/workflows/refresh-instagram-token.yml/runs?per_page=100', (_command, args) => {
    assert.equal(args.at(-2), '--jq');
    assert.equal(args.at(-1), '{total_count, workflow_runs: [.workflow_runs[] | {id, display_title, path, head_sha, head_repository: {full_name: .head_repository.full_name}, status, conclusion}]}');
    return JSON.stringify(expected);
  }), expected);
});
test('checkpoint artifact projection retains identity, expiry and inventory bounds', () => {
  const expected = { total_count: 1, artifacts: [{ id: 1, name: 'instagram-lifecycle-v1', expired: false }] };
  assert.deepEqual(readLifecycleMetadata('repos/example/repo/actions/runs/20/artifacts?per_page=100', (_command, args) => {
    assert.equal(args.at(-1), '{total_count, artifacts: [.artifacts[] | {id, name, expired}]}');
    return JSON.stringify(expected);
  }), expected);
});
test('metadata execution and JSON errors fail closed without echoing response content', () => {
  const endpoint = `repos/example/repo/compare/${'a'.repeat(40)}...${'b'.repeat(40)}`;
  assert.throws(() => readLifecycleMetadata(endpoint, () => { throw new Error('sensitive upstream diagnostic'); }), { message: 'GitHub lifecycle metadata request failed' });
  assert.throws(() => readLifecycleMetadata(endpoint, () => 'not-json sensitive upstream diagnostic'), { message: 'GitHub lifecycle metadata request failed' });
  assert.throws(() => readLifecycleMetadata('repos/example/repo/unsupported', () => assert.fail('must not execute')), /Unsupported lifecycle metadata request/);
});
function artifactApi(runs, artifacts = [{ id: 1, name: 'instagram-lifecycle-v1', expired: false }], compare = 'ahead') {
  return async endpoint => {
    if (endpoint.includes('/workflows/')) return { workflow_runs: runs, total_count: runs.length };
    if (endpoint.includes('/compare/')) return { status: compare };
    return { artifacts, total_count: artifacts.length };
  };
}
test('checkpoint lookup accepts successful ancestor provenance and never follows unmerged sibling history', async () => {
  assert.equal(await findLifecycleRun(lookupConfig, artifactApi([producer()])), '20');
  await assert.rejects(findLifecycleRun(lookupConfig, artifactApi([producer()], undefined, 'diverged')), /No eligible/);
});
for (const state of ['failure', 'cancelled', 'timed_out']) test('newest failed producer blocks older state: ' + state, async () => {
  await assert.rejects(findLifecycleRun(lookupConfig, artifactApi([producer({ conclusion: state }), producer({ id: 10 })])), /did not succeed/);
});
for (const artifacts of [[], [{ id: 1, name: 'instagram-lifecycle-v1', expired: true }], [{ id: 1, name: 'instagram-lifecycle-v1' }, { id: 2, name: 'instagram-lifecycle-v1' }]]) {
  test('missing/expired/ambiguous latest checkpoint cannot fall back: ' + JSON.stringify(artifacts), async () => {
    await assert.rejects(findLifecycleRun(lookupConfig, artifactApi([producer(), producer({ id: 10 })], artifacts)), /missing, ambiguous or expired/);
  });
}
test('read-only runs and current setup are not state producers; missing bootstrap fails explicitly', async () => {
  await assert.rejects(findLifecycleRun(lookupConfig, artifactApi([producer({ id: 30 }), producer({ display_title: 'Instagram lifecycle: check' })])), /No eligible/);
});
test('foreign repository producer is rejected', async () => {
  await assert.rejects(findLifecycleRun(lookupConfig, artifactApi([producer({ head_repository: { full_name: 'attacker/repo' } })])), /producer identity/);
});

test('the existing Facebook messaging invalid-token regression remains enforced', async () => {
  const status = await checkMessagingToken('test-only', 'Test Token', { appId: 'test-app', appSecret: 'test-secret', fetchImpl: async () => response({ data: { is_valid: false } }) });
  assert.deepEqual(status, { ok: false, error: 'invalid' });
});

test('workflow retains fail-closed notifications and scopes secrets away from checkout and read-only metadata lookup', async () => {
  const workflow = await readFile(new URL('../../.github/workflows/refresh-instagram-token.yml', import.meta.url), 'utf8');
  assert.match(workflow, /contents: read/);
  assert.match(workflow, /actions: read/);
  assert.match(workflow, /cancel-in-progress: false/);
  assert.match(workflow, /persist-credentials: false/);
  assert.match(workflow, /if-no-files-found: error/);
  assert.match(workflow, /state\.enc\.json/);
  assert.doesNotMatch(workflow, /FLY_API_TOKEN|flyctl|FACEBOOK_APP_SECRET|INSTAGRAM_ACCESS_TOKEN:|continue-on-error/);
  assert.match(workflow, /if: failure\(\) && steps\.notification-config\.outputs\.enabled == 'true'/);
  assert.match(workflow, /if: failure\(\) && steps\.notification-config\.outputs\.enabled == 'false'/);
  assert.match(workflow, /INSTAGRAM_APP_ID: \$\{\{ secrets\.INSTAGRAM_APP_ID \}\}/);
  assert.doesNotMatch(workflow, /INSTAGRAM_APP_ID[^\n]*\|\|/);
  const checkStep = workflow.split('- name: Check\/Refresh Token')[1].split('- name: Persist')[0];
  assert.doesNotMatch(checkStep, /AUTHORIZATION_CODE/);
  const bootstrapStep = workflow.split('- name: Bootstrap')[1].split('- name: Check\/Refresh Token')[0];
  assert.match(bootstrapStep, /if: inputs.action == 'setup'/);
  assert.match(bootstrapStep, /INSTAGRAM_AUTHORIZATION_CODE/);
});

test('token maintenance never invokes a shell, exposes prefixes, or deploys as part of lifecycle', async () => {
  const source = await readFile(new URL('../refresh-instagram-token.mjs', import.meta.url), 'utf8');
  const library = await readFile(new URL('../lib/instagram-token-lifecycle.mjs', import.meta.url), 'utf8');
  assert.doesNotMatch(source + library, /execSync\s*\(|flyctl|token (?:prefix|substring)/i);
  assert.doesNotMatch(library, /graph\.facebook\.com|debug_token/);
});
