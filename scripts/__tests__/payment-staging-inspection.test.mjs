import assert from 'node:assert/strict';
import test from 'node:test';
import {
  assertStagingApp, classifyInspectionError, inspectKoyeb, inspectStaging, STAGING_APPS,
  summarizeConfig, summarizeSecrets, summarizeStatus,
} from '../inspect-payment-staging.mjs';

test('staging inspection refuses production and arbitrary targets', () => {
  for (const app of ['tdf-hq', 'production', 'attacker-staging', '']) {
    assert.throws(() => assertStagingApp(app));
  }
  STAGING_APPS.forEach(assertStagingApp);
});

test('projections discard credential values, digests, unknown config and machine payloads', () => {
  const app = STAGING_APPS[0];
  const marker = 'SYNTHETIC-DO-NOT-RETAIN';
  const config = summarizeConfig(app, { app, env: {
    APP_ENV: 'staging', PAYPAL_ENV: marker, DATABASE_URL: marker, UNKNOWN: marker,
  } });
  const secrets = summarizeSecrets([{ Name: 'PAYPHONE_TOKEN', Digest: marker, Value: marker }]);
  const status = summarizeStatus(app, { Name: app, Status: 'deployed', Machines: [
    { id: '12345678abcdef', state: 'started', config: { env: { SECRET: marker } } },
  ] });
  assert.equal(JSON.stringify({ config, secrets, status }).includes(marker), false);
  assert.equal(config.safetySettings.PAYPAL_ENV, 'unexpected');
  assert.deepEqual(config.sensitivePlaintextSettingNames, ['DATABASE_URL']);
  assert.equal(secrets.PAYPHONE_TOKEN, true);
  assert.equal(secrets.PLACETOPAY_SECRET_KEY, false);
  assert.throws(() => summarizeConfig(app, { app: 'tdf-hq' }));
  assert.throws(() => summarizeStatus(app, { Name: 'tdf-hq' }));
});

test('only read-only fixed-app commands run and errors cannot leak raw provider output', async () => {
  const calls = [];
  const report = await inspectStaging({
    runFly: async (args) => {
      calls.push(args);
      throw Object.assign(new Error('SYNTHETIC-PRIVATE-ERROR'), { stdout: 'SYNTHETIC-PRIVATE-STDOUT' });
    },
    health: async () => ({ statusOk: true }),
  });
  assert.equal(calls.length, 6);
  for (const args of calls) {
    assert.ok(STAGING_APPS.includes(args[args.indexOf('--app') + 1]));
    assert.ok(['status', 'config', 'secrets'].includes(args[0]));
    if (args[0] === 'config') assert.equal(args[1], 'show');
    if (args[0] === 'secrets') assert.equal(args[1], 'list');
  }
  assert.equal(JSON.stringify(report).includes('SYNTHETIC-PRIVATE'), false);
  assert.equal(report.providerQualified, false);
});

test('auth failures are actionable without returning credential-bearing diagnostics', () => {
  assert.equal(classifyInspectionError({ stderr: 'Error: authentication token expired SECRET-VALUE' }),
    'hosting_authentication_unavailable');
  assert.equal(classifyInspectionError({ stderr: 'Error: not authorized SECRET-VALUE' }),
    'hosting_authorization_denied');
  assert.equal(classifyInspectionError(new Error('Unexpected staging app identity')),
    'unexpected_app_identity');
  assert.equal(classifyInspectionError(new SyntaxError('PRIVATE-RESPONSE')), 'non_json_cli_response');
});

test('alternative hosting inspection is GET-only and omits production and secret fields', async () => {
  let requests = 0;
  const report = await inspectKoyeb({ token: 'SYNTHETIC-TOKEN', fetcher: async (url, options) => {
    requests += 1;
    assert.equal(url, 'https://app.koyeb.com/v1/apps?limit=100');
    assert.equal(options.method, 'GET');
    assert.equal(options.redirect, 'error');
    return { ok: true, status: 200, json: async () => ({ apps: [
      { name: 'production', id: 'PRIVATE-PRODUCTION-ID' },
      { name: 'tdf-payments-staging', id: '11111111-1111-4111-8111-111111111111', secret: 'SYNTHETIC-SECRET' },
    ] }) };
  } });
  assert.equal(requests, 1);
  assert.equal(report.accessible, true);
  assert.equal(report.stagingApps.length, 1);
  assert.equal(/SYNTHETIC|PRIVATE-PRODUCTION/.test(JSON.stringify(report)), false);
  assert.deepEqual(await inspectKoyeb(), { configured: false, accessible: false });
});
