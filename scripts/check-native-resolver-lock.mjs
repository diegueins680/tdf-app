import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import semver from 'semver';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const read = file => JSON.parse(fs.readFileSync(file, 'utf8'));

export function checkNativeResolverLock(root = ROOT) {
  const lock = read(path.join(root, 'package-lock.json'));
  const installed = read(path.join(root, 'node_modules/unrs-resolver/package.json'));
  const resolver = lock.packages?.['node_modules/unrs-resolver'];
  assert.equal(lock.lockfileVersion, 3, 'Expected the workspace npm v3 lock');
  assert.ok(resolver && semver.valid(resolver.version), 'Missing locked native resolver');
  assert.equal(resolver.version, installed.version, 'Installed resolver differs from the lock');
  const bindings = installed.optionalDependencies;
  assert.ok(bindings && typeof bindings === 'object' && !Array.isArray(bindings)
    && Object.keys(bindings).length > 0, 'Missing published optional resolver bindings');
  assert.deepEqual(resolver.optionalDependencies, bindings, 'Locked resolver declarations are incomplete');
  for (const [name, version] of Object.entries(bindings)) {
    assert.match(name, /^@unrs\/resolver-binding-[a-z0-9-]+$/, 'Unexpected optional resolver package');
    assert.ok(semver.valid(version), 'Resolver bindings must use exact versions');
    const entry = lock.packages[`node_modules/${name}`];
    assert.ok(entry, `Missing locked resolver binding: ${name}`);
    assert.equal(entry.version, version, `Wrong locked resolver binding version: ${name}`);
    assert.equal(entry.optional, true, `Resolver binding must remain optional: ${name}`);
    assert.ok(typeof entry.resolved === 'string', `Missing resolver archive: ${name}`);
    assert.equal(new URL(entry.resolved).protocol, 'https:', `Untrusted resolver archive transport: ${name}`);
    assert.match(entry.integrity ?? '', /^sha512-[A-Za-z0-9+/]{86}==$/, `Missing resolver integrity: ${name}`);
  }
  return { resolverVersion: resolver.version, lockedOptionalBindings: Object.keys(bindings).length };
}

export function probeNativeResolver(root = ROOT) {
  const env = { ...process.env, SKIP_UNRS_RESOLVER_FALLBACK: '1' };
  delete env.NAPI_RS_NATIVE_LIBRARY_PATH;
  delete env.NAPI_RS_FORCE_WASI;
  const result = spawnSync(process.execPath, ['-e',
    "const r = require('unrs-resolver'); if (typeof r.ResolverFactory !== 'function') throw new Error('Missing resolver factory'); console.log('native resolver ready');",
  ], { cwd: root, env, encoding: 'utf8', timeout: 20_000 });
  assert.equal(result.status, 0, `Native resolver unavailable with runtime downloads disabled: ${result.error?.message ?? result.stderr}`);
  assert.equal(result.stdout.trim(), 'native resolver ready', 'Native resolver probe did not execute');
  return { nativeResolverLoaded: true, runtimeFallback: 'disabled', platform: process.platform, arch: process.arch };
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    assert.ok(process.argv.slice(2).every(arg => arg === '--probe'), 'Usage: check-native-resolver-lock.mjs [--probe]');
    const result = checkNativeResolverLock();
    console.log(JSON.stringify({ ...result, ...(process.argv.includes('--probe') ? probeNativeResolver() : {}) }));
  } catch (error) { console.error(error.message); process.exitCode = 1; }
}
