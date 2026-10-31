import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import crypto from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { verifyBrowserVendor, evaluateAdvisories, auditBrowserPackages, validateAdvisoryPackages } from '../check-browser-vendor.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const manifestPath = 'third_party/webtorrent/browser.json';
const inventoryPath = 'third_party/webtorrent/bundled-provenance.json';
const noticesPath = 'tdf-hq-ui/public/third-party/webtorrent-NOTICES.txt';
const assetPath = 'tdf-hq-ui/src/vendor/webtorrent/webtorrent.min.js';
const loaderPath = 'tdf-hq-ui/src/utils/torrentAudio.ts';
const json = file => JSON.parse(fs.readFileSync(file, 'utf8'));
const write = (file, value) => fs.writeFileSync(file, JSON.stringify(value));
const digest = value => crypto.createHash('sha256').update(value).digest('hex');
function fixture(t) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-browser-vendor-test-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  for (const file of [manifestPath, inventoryPath, noticesPath, assetPath, loaderPath, 'package-lock.json', 'audit-ci.jsonc']) {
    fs.mkdirSync(path.dirname(path.join(directory, file)), { recursive: true });
    fs.copyFileSync(path.join(root, file), path.join(directory, file));
  }
  return directory;
}
function edit(directory, relative, action) {
  const file = path.join(directory, relative); const value = json(file); action(value); write(file, value);
}
function rehash(directory, key, file) {
  edit(directory, manifestPath, m => { m[key].sha256 = digest(fs.readFileSync(path.join(directory, file))); });
}
const packages = [{ name: 'example', versions: ['1.0.0', '2.0.0'] }];
const advisory = { id: 123, title: 'Controlled advisory', url: 'https://github.com/advisories/test', severity: 'low', vulnerable_versions: '<2.0.0' };
const response = report => ({ ok: true, text: async () => JSON.stringify(report) });

test('the shipped browser artifact, notices and conservative inventory pass', () => {
  const result = verifyBrowserVendor(root);
  assert.equal(result.component.version, '2.8.5');
  assert.equal(result.advisoryPackages.length, 59);
  assert.equal(result.assetSha256, '98f65d27e64f5872427544e53566bba011ace28af867ac44fc8fc08c2f92bcae');
});

test('corrupt or missing browser bytes reject', t => {
  const directory = fixture(t);
  fs.appendFileSync(path.join(directory, assetPath), 'corruption');
  assert.throws(() => verifyBrowserVendor(directory), /integrity/);
  fs.unlinkSync(path.join(directory, assetPath));
  assert.throws(() => verifyBrowserVendor(directory), /ENOENT/);
});

test('coherently relabeling modified browser bytes cannot bless another artifact', t => {
  const directory = fixture(t);
  fs.appendFileSync(path.join(directory, assetPath), 'x'); rehash(directory, 'asset', assetPath);
  assert.throws(() => verifyBrowserVendor(directory), /Unreviewed browser artifact/);
});

test('asset symlink and manifest traversal reject', t => {
  const directory = fixture(t);
  fs.unlinkSync(path.join(directory, assetPath)); fs.symlinkSync(path.join(root, assetPath), path.join(directory, assetPath));
  assert.throws(() => verifyBrowserVendor(directory), /regular file/);
  edit(directory, manifestPath, m => { m.asset.path = '../../foreign.js'; });
  assert.throws(() => verifyBrowserVendor(directory), /Unreviewed browser artifact/);
});

for (const name of ['webtorrent', 'torrent-discovery', 'bittorrent-tracker', 'ip']) {
  test(`the removed Node dependency ${name} cannot silently return, including an npm alias`, t => {
    const directory = fixture(t);
    edit(directory, 'package-lock.json', lock => { lock.packages['node_modules/innocent-alias'] = { name, version: '2.0.1' }; });
    assert.throws(() => verifyBrowserVendor(directory), /Unused Node torrent package reintroduced/);
  });
}

test('removed browser packages or candidate versions reject', t => {
  const directory = fixture(t);
  edit(directory, manifestPath, m => { m.advisoryPackages = m.advisoryPackages.filter(x => x.name !== 'webtorrent'); });
  assert.throws(() => verifyBrowserVendor(directory), /Incomplete browser advisory coverage/);
  fs.copyFileSync(path.join(root, manifestPath), path.join(directory, manifestPath));
  edit(directory, manifestPath, m => { m.advisoryPackages.find(x => x.name === 'text-decoder').versions.pop(); });
  assert.throws(() => verifyBrowserVendor(directory), /Omitted browser advisory version/);
});

test('removing published notices cannot pass by updating their hash', t => {
  const directory = fixture(t);
  fs.writeFileSync(path.join(directory, noticesPath), 'MIT'); rehash(directory, 'notices', noticesPath);
  assert.throws(() => verifyBrowserVendor(directory), /Missing published license or attribution/);
});

test('source-map inventory omissions and duplicates reject after rehashing', t => {
  const directory = fixture(t);
  edit(directory, inventoryPath, i => { i.packages[0].sourceMapPaths = i.packages[1].sourceMapPaths; });
  rehash(directory, 'inventory', inventoryPath);
  assert.throws(() => verifyBrowserVendor(directory), /Unreviewed browser artifact/);
});

test('coherent inventory and manifest edits cannot narrow the reviewed advisory coverage', t => {
  const directory = fixture(t);
  edit(directory, inventoryPath, i => {
    i.packages.find(x => x.name === 'events').preRemovalAppLockVersionsNotBundleIdentity = [];
  });
  edit(directory, manifestPath, m => {
    m.advisoryPackages.find(x => x.name === 'events').versions = ['2.1.0'];
  });
  rehash(directory, 'inventory', inventoryPath);
  assert.throws(() => verifyBrowserVendor(directory), /Unreviewed browser artifact/);
});

test('the original npm runtime import cannot return', t => {
  const directory = fixture(t);
  const file = path.join(directory, loaderPath);
  fs.writeFileSync(file, fs.readFileSync(file, 'utf8').replace('../vendor/webtorrent/webtorrent.min.js?url', 'webtorrent/dist/webtorrent.min.js?url'));
  assert.throws(() => verifyBrowserVendor(directory), /Unreviewed torrent browser entry/);
});

test('lowering the audit threshold or restoring the ip exception rejects', t => {
  const directory = fixture(t);
  edit(directory, 'audit-ci.jsonc', p => { p.low = false; });
  assert.throws(() => verifyBrowserVendor(directory), /Torrent audit gate weakened/);
  edit(directory, 'audit-ci.jsonc', p => { p.low = true; p.allowlist.push('GHSA-2p57-rm9w-gvfp'); });
  assert.throws(() => verifyBrowserVendor(directory), /Torrent audit gate weakened/);
});

test('browser advisory identities require unique names and exact valid versions', () => {
  for (const value of [[], null, [{ name: 'example', versions: ['*'] }], [...packages, ...packages], [{ name: '../secret', versions: ['1.0.0'] }]]) {
    assert.throws(() => validateAdvisoryPackages(value));
  }
});

test('actual semver range matching rejects affected versions at every enforced severity', async () => {
  for (const severity of ['low', 'moderate', 'high', 'critical']) {
    const report = { example: [{ ...advisory, severity }] };
    assert.deepEqual(evaluateAdvisories(packages, report)[0].versions, ['1.0.0']);
    await assert.rejects(auditBrowserPackages(packages, async () => response(report)), /Browser component advisories/);
  }
  assert.deepEqual(evaluateAdvisories(packages, { example: [{ ...advisory, vulnerable_versions: '<1.0.0' }] }), []);
  assert.deepEqual(evaluateAdvisories(packages, { example: [{ ...advisory, severity: 'info' }] }), []);
});

test('live advisory requests query all versions at the fixed registry without redirects', async () => {
  let called = 0;
  const result = await auditBrowserPackages(packages, async (url, options) => {
    called++;
    assert.equal(url, 'https://registry.npmjs.org/-/npm/v1/security/advisories/bulk');
    assert.equal(options.redirect, 'error'); assert.equal(options.method, 'POST');
    assert.deepEqual(JSON.parse(options.body), { example: ['1.0.0', '2.0.0'] });
    assert.ok(options.signal instanceof AbortSignal);
    return response({});
  });
  assert.equal(called, 1); assert.equal(result.affected, 0); assert.equal(result.queriedPackages, 1);
});

test('network, HTTP, invalid JSON, unexpected identities and malformed advisories fail closed', async () => {
  const failures = [
    async () => { throw new Error('network unavailable'); },
    async () => ({ ok: false, status: 503 }),
    async () => ({ ok: true, text: async () => 'invalid' }),
    async () => response([]),
    async () => response({ unexpected: [] }),
    async () => response({ example: [{ ...advisory, severity: 'unknown' }] }),
    async () => response({ example: [{ ...advisory, vulnerable_versions: 'nonsense' }] }),
    async () => response({ example: [{ ...advisory, id: '123' }] }),
    async () => ({ ok: true, text: async () => ' '.repeat(8_000_001) }),
  ];
  for (const fetchImpl of failures) await assert.rejects(auditBrowserPackages(packages, fetchImpl));
});
