'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { classifyNavigation, deepLinkFromArgv, describeForLog, isSafeExternalUrl, parseDeepLink } = require('../src/main/links.cjs');

const origin = 'https://www.tdfrecords.net';

test('accepts deep links that resolve to safe in-app paths', () => {
  assert.deepEqual(parseDeepLink('tdf-desktop://open?path=/eventos/42', origin), { path: '/eventos/42' });
  assert.deepEqual(parseDeepLink('tdf-desktop://open/social/eventos?city=Quito#top', origin), { path: '/social/eventos?city=Quito#top' });
  assert.deepEqual(parseDeepLink('tdf-desktop://open', origin), { path: '/' });
  assert.deepEqual(parseDeepLink('https://www.tdfrecords.net/estudio/calendario?d=2026-11-08', origin), { path: '/estudio/calendario?d=2026-11-08' });
});

test('rejects deep links that would escape the hosted application', () => {
  for (const raw of [
    'tdf-desktop://open?path=//evil.example/phish',
    'tdf-desktop://open?path=https://evil.example',
    'tdf-desktop://open?path=/../../etc/passwd',
    'tdf-desktop://open?path=/a/%2e%2e/b',
    'tdf-desktop://open?path=javascript:alert(1)',
    'tdf-desktop://open?path=/ok\u0000bad',
    'tdf-desktop://internal/offline.html',
    'tdf-desktop://evil/open?path=/x',
    'https://evil.example/eventos/42',
    'https://user:pass@www.tdfrecords.net/eventos/42',
    'http://www.tdfrecords.net/eventos/42',
    'file:///etc/passwd',
    `tdf-desktop://open?path=/${'a'.repeat(5000)}`,
    '',
    42,
  ]) assert.equal(parseDeepLink(raw, origin), null, String(raw));
});

test('selects the first valid deep link from process arguments only', () => {
  assert.deepEqual(deepLinkFromArgv(['electron', '--flag', 'tdf-desktop://open?path=//evil', 'tdf-desktop://open?path=/fans'], origin), { path: '/fans' });
  assert.equal(deepLinkFromArgv(['electron', 'https://www.tdfrecords.net/fans'], origin), null);
  assert.equal(deepLinkFromArgv(undefined, origin), null);
});

test('classifies navigation targets', () => {
  assert.equal(classifyNavigation('https://www.tdfrecords.net/login', origin), 'app');
  assert.equal(classifyNavigation('tdf-desktop://internal/offline.html?code=-106', origin), 'internal');
  assert.equal(classifyNavigation('tdf-desktop://open?path=/x', origin), 'blocked');
  assert.equal(classifyNavigation('https://checkout.stripe.com/pay/cs_test', origin), 'external');
  assert.equal(classifyNavigation('http://www.tdfrecords.net/login', origin), 'external');
  assert.equal(classifyNavigation('https://user@www.tdfrecords.net/', origin), 'blocked');
  assert.equal(classifyNavigation('file:///Users/me/master.wav', origin), 'blocked');
  assert.equal(classifyNavigation('javascript:alert(1)', origin), 'blocked');
  assert.equal(classifyNavigation('data:text/html,hi', origin), 'blocked');
  assert.equal(classifyNavigation('not a url', origin), 'blocked');
});

test('only http(s) without credentials may open in the system browser', () => {
  assert.equal(isSafeExternalUrl('https://accounts.google.com/o/oauth2'), true);
  assert.equal(isSafeExternalUrl('http://example.com'), true);
  assert.equal(isSafeExternalUrl('https://user:pw@example.com'), false);
  assert.equal(isSafeExternalUrl('file:///etc/hosts'), false);
  assert.equal(isSafeExternalUrl('smb://share/x'), false);
  assert.equal(isSafeExternalUrl(''), false);
});

test('log descriptions keep scheme, host and path but never query strings, fragments or credentials', () => {
  assert.equal(describeForLog('tdf-desktop://open?path=%2Freset%3Ftoken%3Dsecret-token'), 'tdf-desktop://open?[redacted]');
  assert.equal(describeForLog('tdf-desktop://open/reset?token=secret-token#x'), 'tdf-desktop://open/reset?[redacted]#[redacted]');
  assert.equal(describeForLog('https://www.tdfrecords.net/reset?token=secret-token'), 'https://www.tdfrecords.net/reset?[redacted]');
  assert.equal(describeForLog('https://user:secret-token@www.tdfrecords.net/eventos'), 'https://[credentials]@www.tdfrecords.net/eventos');
  assert.equal(describeForLog('/reset?token=secret-token'), '/reset?[redacted]');
  assert.equal(describeForLog('/eventos/42#top'), '/eventos/42#[redacted]');
  assert.equal(describeForLog('/eventos/42'), '/eventos/42');
  assert.equal(describeForLog('file:///etc/passwd'), 'file:///etc/passwd');
  assert.equal(describeForLog(`https://www.tdfrecords.net/${'a'.repeat(400)}?t=1`).length, 160 + '?[redacted]'.length);
  assert.equal(describeForLog(''), '<empty>');
  assert.equal(describeForLog(42), '<number>');
  assert.equal(describeForLog(null), '<null>');
  for (const raw of [
    'tdf-desktop://open?path=%2Freset%3Ftoken%3Dsecret-token',
    'tdf-desktop://open?path=//evil.example/phish?token=secret-token',
    '/reset?token=secret-token',
    'not a url?token=secret-token',
    'https://user:secret-token@www.tdfrecords.net/',
  ]) assert.doesNotMatch(describeForLog(raw), /secret-token/, raw);
});


test('malformed credential-bearing links log only a generic redacted descriptor and never navigate', () => {
  for (const raw of [
    'tdf-desktop://user:SYNTHETIC_PASSWORD@[invalid]/open',
    'tdf-desktop://SYNTHETIC_USERNAME@[invalid]/open',
    'https://user:SYNTHETIC%5FPASSWORD@bad host/',
    'tdf-desktop://user:SYNTHETIC_PASSWORD@\u0000invalid/open',
  ]) {
    assert.equal(parseDeepLink(raw, origin), null);
    assert.equal(classifyNavigation(raw, origin), 'blocked');
    assert.equal(describeForLog(raw), '<invalid-url>');
    assert.doesNotMatch(describeForLog(raw), /SYNTHETIC/);
  }
});
