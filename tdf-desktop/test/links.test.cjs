'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { classifyNavigation, deepLinkFromArgv, isSafeExternalUrl, parseDeepLink } = require('../src/main/links.cjs');

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
