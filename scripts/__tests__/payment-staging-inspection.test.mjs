import assert from 'node:assert/strict';
import test from 'node:test';
import {
  assertStagingApp, inspectStaging, STAGING_APPS, summarizeConfig, summarizeSecrets, summarizeStatus,
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
