import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import ts from 'typescript';

const source = readFileSync(new URL('./storage.ts', import.meta.url), 'utf8');
const { outputText } = ts.transpileModule(source, {
  compilerOptions: {
    module: ts.ModuleKind.CommonJS,
    target: ts.ScriptTarget.ES2020,
  },
});
const storageModule = { exports: {} };
new Function('module', 'exports', outputText)(storageModule, storageModule.exports);
const { parseStoredAuthUser } = storageModule.exports;

test('parseStoredAuthUser returns null for malformed storage JSON', () => {
  assert.equal(parseStoredAuthUser('{not-json'), null);
});

test('parseStoredAuthUser returns the stored auth user for valid JSON', () => {
  const user = {
    token: 'token-123',
    username: 'diego',
    issuedAt: 1,
    expiresAt: Date.now() + 1_000,
    remember: true,
    rotateAt: Date.now() + 2_000,
    partyId: 1,
    roles: ['admin'],
    modules: ['CRM'],
  };

  assert.deepEqual(parseStoredAuthUser(JSON.stringify(user)), user);
});
