'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { assertChannelMatchesSigning, resolveSigning } = require('../scripts/signing.cjs');
const { buildChannel } = require('../scripts/prepare-channel.cjs');

const summary = (signing) => ({ unsigned: signing.unsigned, marker: signing.marker, reason: signing.reason, macSigned: signing.macSigned, winSigned: signing.winSigned });

test('without signing credentials every host platform produces an unsigned test package', () => {
  for (const platform of ['darwin', 'win32', 'linux']) {
    const signing = resolveSigning({}, platform);
    assert.equal(signing.unsigned, true, platform);
    assert.equal(signing.marker, '-UNSIGNED-TEST', platform);
  }
  assert.equal(resolveSigning({}, 'darwin').reason, 'no-certificate');
  assert.equal(resolveSigning({}, 'win32').reason, 'no-certificate');
  assert.equal(resolveSigning({}, 'linux').reason, 'unsupported-platform');
  // The workflow passes an empty value when a secret is not configured: absent, not present.
  assert.equal(resolveSigning({ CSC_LINK: '', WIN_CSC_LINK: '  ' }, 'darwin').unsigned, true);
});

test('a certificate for the host platform signs; TDF_DESKTOP_UNSIGNED=1 still forces a test package', () => {
  assert.deepEqual(summary(resolveSigning({ CSC_LINK: 'developer-id.p12' }, 'darwin')), { unsigned: false, marker: '', reason: null, macSigned: true, winSigned: false });
  assert.deepEqual(summary(resolveSigning({ CSC_NAME: 'Developer ID Application: TDF' }, 'darwin')), { unsigned: false, marker: '', reason: null, macSigned: true, winSigned: false });
  assert.equal(resolveSigning({ CSC_LINK: 'developer-id.p12', TDF_DESKTOP_UNSIGNED: '1' }, 'darwin').reason, 'explicit');
  assert.equal(resolveSigning({ CSC_LINK: 'developer-id.p12', TDF_DESKTOP_UNSIGNED: '0' }, 'darwin').unsigned, false);
  // "0" never claims a signature that the environment cannot produce.
  assert.equal(resolveSigning({ TDF_DESKTOP_UNSIGNED: '0' }, 'darwin').unsigned, true);
  // A macOS certificate does not sign Windows packages and the reverse.
  assert.equal(resolveSigning({ CSC_LINK: 'developer-id.p12' }, 'win32').unsigned, true);
  assert.equal(resolveSigning({ WIN_CSC_LINK: 'authenticode.pfx' }, 'darwin').unsigned, true);
  assert.deepEqual(summary(resolveSigning({ WIN_CSC_LINK: 'authenticode.pfx' }, 'win32')), { unsigned: false, marker: '', reason: null, macSigned: false, winSigned: true });
  assert.equal(resolveSigning({ WIN_CSC_LINK: 'authenticode.pfx' }, 'linux').unsigned, true);
});

test('Azure Trusted Signing needs its complete configuration; notarization needs all three Apple credentials', () => {
  const azure = { AZURE_TENANT_ID: 't', AZURE_CLIENT_ID: 'c', AZURE_CLIENT_SECRET: 's', TDF_WIN_SIGN_ENDPOINT: 'e', TDF_WIN_SIGN_ACCOUNT: 'a', TDF_WIN_SIGN_PROFILE: 'p' };
  assert.equal(resolveSigning(azure, 'win32').unsigned, false);
  assert.equal(resolveSigning({ ...azure, TDF_WIN_SIGN_PROFILE: '' }, 'win32').unsigned, true);
  assert.equal(resolveSigning({ ...azure, TDF_DESKTOP_UNSIGNED: '1' }, 'win32').unsigned, true);
  assert.equal(resolveSigning({ CSC_LINK: 'x', APPLE_ID: 'a', APPLE_APP_SPECIFIC_PASSWORD: 'p', APPLE_TEAM_ID: 'T' }, 'darwin').hasNotarization, true);
  assert.equal(resolveSigning({ CSC_LINK: 'x', APPLE_ID: 'a', APPLE_TEAM_ID: 'T' }, 'darwin').hasNotarization, false);
});

test('the embedded channel carries the same unsigned decision as the package', () => {
  const base = { TDF_DESKTOP_CHANNEL: 'production' };
  assert.equal(buildChannel({ env: base, platform: 'darwin' }).channel.unsigned, true);
  assert.equal(buildChannel({ env: base, platform: 'win32' }).channel.unsigned, true);
  assert.equal(buildChannel({ env: { ...base, CSC_LINK: 'developer-id.p12' }, platform: 'darwin' }).channel.unsigned, false);
  assert.equal(buildChannel({ env: { ...base, CSC_LINK: 'developer-id.p12', TDF_DESKTOP_UNSIGNED: '1' }, platform: 'darwin' }).channel.unsigned, true);
  assert.equal(buildChannel({ env: { ...base, WIN_CSC_LINK: 'authenticode.pfx' }, platform: 'win32' }).channel.unsigned, false);
  assert.equal(buildChannel({ env: { ...base, CSC_LINK: 'developer-id.p12' }, platform: 'linux' }).channel.unsigned, true);
  const { channel, signing } = buildChannel({ env: { ...base, CSC_LINK: 'developer-id.p12' }, platform: 'darwin', sourceCommit: 'a'.repeat(40), now: new Date('2026-11-08T05:00:00Z') });
  assert.equal(channel.sourceCommit, 'a'.repeat(40));
  assert.equal(channel.builtAt, '2026-11-08T05:00:00.000Z');
  assert.equal(channel.appOrigin, 'https://www.tdfrecords.net');
  assert.equal(signing.macSigned, true);
  assert.throws(() => buildChannel({ env: { TDF_DESKTOP_CHANNEL: 'local' }, platform: 'darwin' }), /development only/);
  assert.throws(() => buildChannel({ env: { ...base, TDF_DESKTOP_APP_ORIGIN: 'http://staging.example' }, platform: 'darwin' }), /https origin/);
});

test('packaging refuses a channel.json whose unsigned flag disagrees with the environment', () => {
  const unsigned = resolveSigning({}, 'darwin');
  const signed = resolveSigning({ CSC_LINK: 'developer-id.p12' }, 'darwin');
  assert.equal(assertChannelMatchesSigning({ unsigned: true }, unsigned).unsigned, true);
  assert.equal(assertChannelMatchesSigning({ unsigned: false }, signed).unsigned, false);
  assert.throws(() => assertChannelMatchesSigning({ unsigned: false }, unsigned), /unsigned=false.*unsigned=true.*no-certificate/);
  assert.throws(() => assertChannelMatchesSigning({ unsigned: true }, signed), /prepare:channel/);
  assert.throws(() => assertChannelMatchesSigning(null, signed), /missing/);
});
