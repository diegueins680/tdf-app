import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { checkFiles, verifyLock } from '../check-dependency-security.mjs';
const policy = JSON.parse(fs.readFileSync(new URL('../../formal/system/dependency-security.json', import.meta.url)));
const lock = (name, version) => ({ lockfileVersion: 3, packages: { [`node_modules/parent/node_modules/${name}`]: { version } } });

for (const [name, series] of Object.entries(policy.packages)) {
  for (const minimum of Object.values(series)) {
    test(`${name} ${minimum}: accepts floor and rejects previous vulnerable release`, () => {
      assert.equal(verifyLock(lock(name, minimum), policy), 1);
      const parts = minimum.split('.').map(Number);
      if (parts[2]) parts[2]--; else { parts[1]--; parts[2] = 999; }
      assert.throws(() => verifyLock(lock(name, parts.join('.')), policy), /floor|unreviewed/);
    });
  }
}
test('unreviewed major, prerelease and missing versions fail closed', () => {
  for (const version of ['99.0.0', '1.1.21-rc.1', undefined]) assert.throws(() => verifyLock(lock('brace-expansion', version), policy), /[Uu]nreviewed/);
});
test('removed dependencies do not require retaining a vulnerable package', () => {
  assert.equal(verifyLock({ lockfileVersion: 3, packages: {} }, policy), 0);
});
test('malformed lock cannot silently report success', () => {
  for (const value of [null, {}, ...[[], 42, 'invalid', true].map(packages => ({ lockfileVersion: 3, packages }))]) assert.throws(() => verifyLock(value, policy), /package lock/);
});
test('requested Mobile coverage cannot pass with absent Mobile lock', () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-dependency-policy-'));
  try {
    fs.mkdirSync(path.join(root, 'formal/system'), { recursive: true });
    fs.writeFileSync(path.join(root, 'formal/system/dependency-security.json'), JSON.stringify(policy));
    fs.writeFileSync(path.join(root, 'package-lock.json'), JSON.stringify(lock('tmp', '0.2.7')));
    assert.equal(checkFiles(root).length, 1);
    assert.throws(() => checkFiles(root, true), /ENOENT/);
  } finally { fs.rmSync(root, { recursive: true, force: true }); }
});

test('package aliases and nested aliases retain their actual package security floor', () => {
  for (const location of ['node_modules/request-client', 'node_modules/parent/node_modules/request-client']) {
    const value = { lockfileVersion: 3, packages: { [location]: { name: 'axios', version: '1.0.0' } } };
    assert.throws(() => verifyLock(value, policy), /floor/);
    value.packages[location].version = '1.20.0';
    assert.equal(verifyLock(value, policy), 1);
    value.packages[location].name = 42;
    assert.throws(() => verifyLock(value, policy), /identity/);
  }
});
test('malformed package entry and unbounded version integers fail', () => {
  assert.throws(() => verifyLock({ lockfileVersion: 3, packages: { 'node_modules/axios': null } }, policy), /entry/);
  assert.throws(() => verifyLock(lock('axios', '1.99999999999999999999.0'), policy), /Unreviewed/);
});

test('present malformed policy series cannot silently disable a floor', () => {
  for (const series of [null, false, 0, '', [], {}, { 1: '2.0.0' }, { 1: '1.20.0-rc' }]) {
    assert.throws(() => verifyLock(lock('axios', '1.0.0'), { schemaVersion: 1, packages: { axios: series } }), /policy|version/);
  }
  assert.throws(() => verifyLock(lock('axios', '1.0.0'), { schemaVersion: 1, packages: {} }), /policy/);
});
