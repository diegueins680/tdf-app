import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { checkNativeResolverLock } from '../check-native-resolver-lock.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const read = file => JSON.parse(fs.readFileSync(file, 'utf8'));
function fixture(t) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-native-lock-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  fs.mkdirSync(path.join(directory, 'node_modules/unrs-resolver'), { recursive: true });
  for (const file of ['package-lock.json', 'node_modules/unrs-resolver/package.json']) {
    fs.copyFileSync(path.join(root, file), path.join(directory, file));
  }
  return directory;
}
function edit(directory, file, action) {
  const target = path.join(directory, file); const value = read(target);
  action(value); fs.writeFileSync(target, JSON.stringify(value));
}
const firstBinding = lock => Object.keys(lock.packages['node_modules/unrs-resolver'].optionalDependencies)[0];

test('every published optional native binding has an exact locked archive', () => {
  const result = checkNativeResolverLock();
  assert.equal(result.resolverVersion, '1.11.1');
  assert.equal(result.lockedOptionalBindings, 19);
});

test('a missing platform entry rejects even when this host does not use it', t => {
  const directory = fixture(t);
  edit(directory, 'package-lock.json', lock => { delete lock.packages[`node_modules/${firstBinding(lock)}`]; });
  assert.throws(() => checkNativeResolverLock(directory), /Missing locked resolver binding/);
});

test('removing a binding and its declaration cannot hide the missing dependency', t => {
  const directory = fixture(t);
  edit(directory, 'package-lock.json', lock => {
    const name = firstBinding(lock);
    delete lock.packages[`node_modules/${name}`];
    delete lock.packages['node_modules/unrs-resolver'].optionalDependencies[name];
  });
  assert.throws(() => checkNativeResolverLock(directory), /declarations are incomplete/);
});

test('unlocked or mismatched parent resolver rejects', t => {
  const directory = fixture(t);
  edit(directory, 'package-lock.json', lock => { lock.packages['node_modules/unrs-resolver'].version = '1.11.0'; });
  assert.throws(() => checkNativeResolverLock(directory), /differs from the lock/);
  edit(directory, 'package-lock.json', lock => { delete lock.packages['node_modules/unrs-resolver']; });
  assert.throws(() => checkNativeResolverLock(directory), /Missing locked native resolver/);
});

for (const [field, value] of Object.entries({ version: '1.11.0', optional: false, resolved: '', integrity: '' })) {
  test(`incomplete binding ${field} rejects`, t => {
    const directory = fixture(t);
    edit(directory, 'package-lock.json', lock => { lock.packages[`node_modules/${firstBinding(lock)}`][field] = value; });
    assert.throws(() => checkNativeResolverLock(directory));
  });
}

test('a removed installed declaration set or version range rejects', t => {
  const directory = fixture(t);
  edit(directory, 'node_modules/unrs-resolver/package.json', module => { module.optionalDependencies = {}; });
  assert.throws(() => checkNativeResolverLock(directory), /Missing published optional/);
  const declarations = { '@unrs/resolver-binding-darwin-x64': '^1.11.1' };
  edit(directory, 'node_modules/unrs-resolver/package.json', module => { module.optionalDependencies = declarations; });
  edit(directory, 'package-lock.json', lock => { lock.packages['node_modules/unrs-resolver'].optionalDependencies = declarations; });
  assert.throws(() => checkNativeResolverLock(directory), /exact versions/);
});

test('malformed and non-HTTPS archive locations reject', t => {
  const directory = fixture(t);
  for (const resolved of ['https://', 'file:///tmp/native.tgz', 'http://registry.example/native.tgz']) {
    edit(directory, 'package-lock.json', lock => { lock.packages[`node_modules/${firstBinding(lock)}`].resolved = resolved; });
    assert.throws(() => checkNativeResolverLock(directory));
  }
});
