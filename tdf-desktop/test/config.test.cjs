'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { loadChannel, normalizeChannel, parseOrigin, sanitizeAppPath } = require('../src/main/config.cjs');

const channelsDir = path.resolve(__dirname, '..', 'channels');

test('production channel is a valid https channel with shell updates enabled', () => {
  const channel = normalizeChannel(JSON.parse(fs.readFileSync(path.join(channelsDir, 'production.json'), 'utf8')));
  assert.equal(channel.appOrigin, 'https://www.tdfrecords.net');
  assert.equal(channel.apiOrigin, 'https://api.tdfrecords.net');
  assert.equal(channel.updates.enabled, true);
  assert.match(channel.updates.url, /^https:\/\/github\.com\/diegueins680\/tdf-app\/releases\/download\/desktop-latest$/);
  assert.equal(channel.unsigned, false);
});

test('origins must be bare http(s) origins; http only on loopback or when explicitly allowed', () => {
  assert.equal(parseOrigin('https://www.tdfrecords.net'), 'https://www.tdfrecords.net');
  assert.equal(parseOrigin('https://www.tdfrecords.net/'), 'https://www.tdfrecords.net');
  assert.equal(parseOrigin('http://127.0.0.1:4173'), 'http://127.0.0.1:4173');
  assert.equal(parseOrigin('http://localhost:5173'), 'http://localhost:5173');
  assert.equal(parseOrigin('http://www.tdfrecords.net'), null);
  assert.equal(parseOrigin('http://staging.example', { allowHttp: true }), 'http://staging.example');
  assert.equal(parseOrigin('https://www.tdfrecords.net/app'), null);
  assert.equal(parseOrigin('https://www.tdfrecords.net?x=1'), null);
  assert.equal(parseOrigin('https://u:p@www.tdfrecords.net'), null);
  assert.equal(parseOrigin('ftp://www.tdfrecords.net'), null);
  assert.equal(parseOrigin(' https://www.tdfrecords.net'), null);
});

test('application paths stay inside the hosted origin', () => {
  assert.equal(sanitizeAppPath('/eventos/42?x=1#y'), '/eventos/42?x=1#y');
  assert.equal(sanitizeAppPath('/'), '/');
  for (const bad of ['//evil.example', '/..', '/a/../b', '/a/./b', 'eventos', 'https://x', '/x\\y', '/a%2fb', '/%2e%2e/x', '', `/${'a'.repeat(3000)}`]) {
    assert.equal(sanitizeAppPath(bad), null, bad);
  }
});

test('channel normalization refuses unsafe or incomplete definitions', () => {
  const base = { name: 'production', appOrigin: 'https://www.tdfrecords.net', apiOrigin: 'https://api.tdfrecords.net', startPath: '/inicio', updates: { enabled: true, provider: 'generic', url: 'https://example.com/feed/', channel: 'latest' } };
  assert.equal(normalizeChannel(base).updates.url, 'https://example.com/feed');
  assert.throws(() => normalizeChannel({ ...base, appOrigin: 'http://www.tdfrecords.net' }), /appOrigin/);
  assert.throws(() => normalizeChannel({ ...base, startPath: '//evil' }), /startPath/);
  assert.throws(() => normalizeChannel({ ...base, updates: { ...base.updates, provider: 'github' } }), /generic/);
  assert.throws(() => normalizeChannel({ ...base, updates: { ...base.updates, url: 'http://insecure/feed' } }), /https/);
  assert.throws(() => normalizeChannel({ ...base, name: 'Prod Uction' }), /name/);
  assert.equal(normalizeChannel({ ...base, sourceCommit: 'not-a-sha' }).sourceCommit, null);
  assert.equal(normalizeChannel({ ...base, unsigned: true }).unsigned, true);
});

test('packaged builds read the shipped channel and ignore development overrides', () => {
  const resources = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-desktop-resources-'));
  try {
    fs.writeFileSync(path.join(resources, 'channel.json'), JSON.stringify({ name: 'production', appOrigin: 'https://www.tdfrecords.net', apiOrigin: 'https://api.tdfrecords.net', updates: { enabled: false } }));
    const channel = loadChannel({ isPackaged: true, resourcesPath: resources, channelsDir, env: { TDF_DESKTOP_APP_URL: 'http://127.0.0.1:4173' } });
    assert.equal(channel.appOrigin, 'https://www.tdfrecords.net');
    assert.equal(channel.unsigned, false);
  } finally { fs.rmSync(resources, { recursive: true, force: true }); }
});

test('development runs are always marked unsigned and accept loopback overrides only', () => {
  const channel = loadChannel({ isPackaged: false, resourcesPath: '/nonexistent', channelsDir, env: { TDF_DESKTOP_APP_URL: 'http://127.0.0.1:4173', TDF_DESKTOP_API_URL: 'http://127.0.0.1:19099' } });
  assert.equal(channel.appOrigin, 'http://127.0.0.1:4173');
  assert.equal(channel.apiOrigin, 'http://127.0.0.1:19099');
  assert.equal(channel.unsigned, true);
  assert.throws(() => loadChannel({ isPackaged: false, resourcesPath: '/nonexistent', channelsDir, env: { TDF_DESKTOP_APP_URL: 'http://evil.example' } }), /loopback/);
  const production = loadChannel({ isPackaged: false, resourcesPath: '/nonexistent', channelsDir, env: { TDF_DESKTOP_CHANNEL: 'production' } });
  assert.equal(production.appOrigin, 'https://www.tdfrecords.net');
  assert.equal(production.unsigned, true);
});
