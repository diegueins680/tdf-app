import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import { fileURLToPath, pathToFileURL } from 'node:url';
import semver from 'semver';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const sha = bytes => crypto.createHash('sha256').update(bytes).digest('hex');
const object = value => value !== null && typeof value === 'object' && !Array.isArray(value);
const forbidden = new Set(['webtorrent', 'torrent-discovery', 'bittorrent-tracker', 'ip']);
const registry = 'https://registry.npmjs.org/-/npm/v1/security/advisories/bulk';
const normalizeNotice = text => text.replace(/\r\n/g, '\n').trim();
const readJson = file => JSON.parse(fs.readFileSync(file, 'utf8'));

function boundFile(root, relative) {
  if (typeof relative !== 'string' || path.isAbsolute(relative) || relative.split(/[\\/]/).includes('..')) {
    throw new Error('Invalid vendor file path');
  }
  const file = path.resolve(root, relative);
  const realRoot = fs.realpathSync(root);
  if (!fs.realpathSync(file).startsWith(realRoot + path.sep) || fs.lstatSync(file).isSymbolicLink() || !fs.statSync(file).isFile()) {
    throw new Error('Vendor file must be a regular file inside the repository');
  }
  return fs.readFileSync(file);
}

export function validateAdvisoryPackages(packages) {
  if (!Array.isArray(packages) || !packages.length) throw new Error('Missing browser advisory inventory');
  const result = new Map();
  for (const entry of packages) {
    if (!object(entry) || typeof entry.name !== 'string' || !/^(?:@[a-z0-9._-]+\/)?[a-z0-9._-]+$/i.test(entry.name)
      || result.has(entry.name) || !Array.isArray(entry.versions) || !entry.versions.length
      || entry.versions.some(v => typeof v !== 'string' || !semver.valid(v))) {
      throw new Error('Invalid or duplicate browser advisory identity/version');
    }
    result.set(entry.name, [...new Set(entry.versions)].sort());
  }
  return result;
}

export function evaluateAdvisories(packages, report) {
  const requested = validateAdvisoryPackages(packages);
  if (!object(report)) throw new Error('Invalid browser advisory response');
  const affected = [];
  for (const [name, advisories] of Object.entries(report)) {
    if (!requested.has(name) || !Array.isArray(advisories)) throw new Error('Unexpected browser advisory identity');
    for (const advisory of advisories) {
      if (!object(advisory) || !Number.isSafeInteger(advisory.id) || advisory.id < 0
        || typeof advisory.title !== 'string' || typeof advisory.url !== 'string'
        || !['info', 'low', 'moderate', 'high', 'critical'].includes(advisory.severity)
        || typeof advisory.vulnerable_versions !== 'string' || !semver.validRange(advisory.vulnerable_versions)) {
        throw new Error('Malformed browser advisory');
      }
      const versions = requested.get(name).filter(v => semver.satisfies(v, advisory.vulnerable_versions, { includePrerelease: true }));
      if (versions.length && advisory.severity !== 'info') affected.push({ name, versions, id: advisory.id, severity: advisory.severity, url: advisory.url });
    }
  }
  return affected;
}

export async function auditBrowserPackages(packages, fetchImpl = globalThis.fetch) {
  const payload = Object.fromEntries(validateAdvisoryPackages(packages));
  const response = await fetchImpl(registry, {
    method: 'POST', headers: { 'content-type': 'application/json' },
    body: JSON.stringify(payload), redirect: 'error', signal: AbortSignal.timeout(20_000),
  });
  if (!response.ok) throw new Error(`Browser advisory HTTP ${response.status}`);
  const text = await response.text();
  if (Buffer.byteLength(text) > 8_000_000) throw new Error('Oversized browser advisory response');
  const report = JSON.parse(text);
  const affected = evaluateAdvisories(packages, report);
  if (affected.length) throw new Error(`Browser component advisories: ${JSON.stringify(affected)}`);
  return { queriedPackages: packages.length, affected: 0, registry, coverage: 'Matching-source and previous-lock candidate versions; exact upstream build resolution remains qualified.' };
}

export function verifyBrowserVendor(root = ROOT) {
  const m = readJson(path.join(root, 'third_party/webtorrent/browser.json'));
  if (m.schemaVersion !== 1 || m.component?.name !== 'webtorrent' || m.component.version !== '2.8.5') throw new Error('Unreviewed browser component');
  if (m.asset?.path !== 'tdf-hq-ui/src/vendor/webtorrent/webtorrent.min.js'
    || m.inventory?.path !== 'third_party/webtorrent/bundled-provenance.json'
    || m.notices?.path !== 'tdf-hq-ui/public/third-party/webtorrent-NOTICES.txt'
    || m.asset.sha256 !== '98f65d27e64f5872427544e53566bba011ace28af867ac44fc8fc08c2f92bcae'
    || m.inventory.sha256 !== '82971c29f4d319e78318ff66b933ddbfdbf30bc8d8ba83739a804a488722bede') throw new Error('Unreviewed browser artifact');
  for (const item of [m.asset, m.inventory, m.notices]) {
    if (!object(item) || !/^[a-f0-9]{64}$/.test(item.sha256) || sha(boundFile(root, item.path)) !== item.sha256) throw new Error('Browser vendor integrity mismatch');
  }
  if (boundFile(root, m.asset.path).length !== m.asset.bytes || m.asset.bytes !== 218115) throw new Error('Browser asset length mismatch');
  const inventory = JSON.parse(boundFile(root, m.inventory.path));
  if (inventory.schemaVersion !== 1 || inventory.officialWebTorrent?.version !== m.component.version
    || inventory.officialWebTorrent.browserAssetSha256 !== m.asset.sha256
    || inventory.officialWebTorrent.tarballSha256 !== m.sourceArchive?.sha256
    || inventory.officialWebTorrent.sourceMapSha256 !== m.sourceArchive.sourceMapSha256
    || m.sourceArchive.url !== 'https://registry.npmjs.org/webtorrent/-/webtorrent-2.8.5.tgz'
    || m.sourceArchive.integrity !== 'sha512-oIjpuBrypApJ+RCZ8RRaHEncVSkt2cd25/I4Trb2sk9nlaEF92Dg1u8BCwqA4eJR7wIZQM95GyO7Wo4QTbrUUA=='
    || m.sourceArchive.sha256 !== '8ba103e9ec0d5950ad218d2ec26265c8ddb28f93dca98b1acb9768300828c1e3'
    || m.sourceArchive.sourceMapSha256 !== 'a48e9e5d69bc844b8ea7fbd5cc2722782b82e9f5f479b8a7451979cecefbdc2b') throw new Error('Browser provenance mismatch');
  const packages = validateAdvisoryPackages(m.advisoryPackages);
  if (!Array.isArray(inventory.packages) || inventory.packages.length !== 58) throw new Error('Incomplete source package inventory');
  const notices = normalizeNotice(boundFile(root, m.notices.path).toString('utf8'));
  function requireNotice(text) {
    if (typeof text !== 'string' || !normalizeNotice(text) || !notices.includes(normalizeNotice(text))) throw new Error('Missing published license or attribution');
  }
  requireNotice(inventory.officialWebTorrent.licenseText);
  const expected = new Set(['webtorrent']);
  const mappedSources = new Set();
  for (const item of inventory.packages) {
    if (expected.has(item.name) || !Array.isArray(item.matchingPublishedCandidates) || !item.matchingPublishedCandidates.length
      || !Array.isArray(item.sourceMapPaths) || !item.sourceMapPaths.length) throw new Error('Missing source or license evidence');
    expected.add(item.name);
    for (const source of item.sourceMapPaths) {
      if (typeof source !== 'string' || mappedSources.has(source)) throw new Error('Duplicate source-map path');
      mappedSources.add(source);
    }
    for (const candidate of item.matchingPublishedCandidates) {
      for (const notice of candidate.licenseNoticeFiles) requireNotice(notice.text);
      for (const statement of candidate.readmeLicenseStatements) requireNotice(statement.text);
    }
    for (const header of item.embeddedAttributionHeaders) requireNotice(header.text);
    const versions = new Set([...item.matchingPublishedCandidates.map(x => x.version), ...item.preRemovalAppLockVersionsNotBundleIdentity]);
    const actual = packages.get(item.name);
    if (!actual || actual.length !== versions.size || actual.some(v => !versions.has(v))) throw new Error('Omitted browser advisory version');
  }
  if (mappedSources.size !== 83 || expected.size !== packages.size || [...expected].some(name => !packages.has(name))
    || JSON.stringify(packages.get('webtorrent')) !== '["2.8.5"]') throw new Error('Incomplete browser advisory coverage');
  if (!Array.isArray(inventory.additionalComponents) || inventory.additionalComponents.length !== 2) throw new Error('Missing additional source components');
  for (const component of inventory.additionalComponents) {
    for (const source of component.sourceMapPaths) {
      if (typeof source !== 'string' || mappedSources.has(source)) throw new Error('Duplicate additional source-map path');
      mappedSources.add(source);
    }
    if (component.licenseNoticeText) requireNotice(component.licenseNoticeText);
  }
  requireNotice('@author Mathias Rasmussen <mathiasvr@gmail.com>');
  if (mappedSources.size !== 99) throw new Error('Incomplete additional source coverage');
  const lock = readJson(path.join(root, 'package-lock.json'));
  if (lock.lockfileVersion !== 3 || !object(lock.packages)) throw new Error('Expected npm v3 lock');
  for (const [location, entry] of Object.entries(lock.packages)) {
    const name = entry.name || location.split('node_modules/').at(-1);
    if (forbidden.has(name)) throw new Error(`Unused Node torrent package reintroduced: ${name}`);
  }
  const policy = readJson(path.join(root, 'audit-ci.jsonc'));
  if (policy.low !== true || policy['skip-dev'] !== true || !Array.isArray(policy.allowlist)
    || policy.allowlist.some(x => JSON.stringify(x).includes('GHSA-2p57-rm9w-gvfp'))) throw new Error('Torrent audit gate weakened');
  const source = fs.readFileSync(path.join(root, 'tdf-hq-ui/src/utils/torrentAudio.ts'), 'utf8');
  if (!source.includes("import webTorrentBundleUrl from '../vendor/webtorrent/webtorrent.min.js?url';")
    || source.includes("from 'webtorrent/dist/")) throw new Error('Unreviewed torrent browser entry');
  return { component: m.component, assetSha256: m.asset.sha256, sourcePackages: 58, advisoryPackages: m.advisoryPackages, coverage: m.advisoryCoverage };
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  try {
    if (process.argv.slice(2).some(x => x !== '--audit')) throw new Error('Usage: check-browser-vendor.mjs [--audit]');
    const result = verifyBrowserVendor();
    const advisory = process.argv.includes('--audit') ? await auditBrowserPackages(result.advisoryPackages) : 'NOT_RUN_STATIC_CHECK_ONLY';
    console.log(JSON.stringify({ ...result, advisory }));
  } catch (error) { console.error(error.message); process.exitCode = 1; }
}
