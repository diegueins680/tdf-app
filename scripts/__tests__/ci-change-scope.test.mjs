import assert from 'node:assert/strict';
import test from 'node:test';

import { allChangeScopes, classifyChangedFiles } from '../ci-change-scope.mjs';

test('UI-only changes do not compile backend or mobile', () => {
  assert.deepEqual(classifyChangedFiles(['tdf-hq-ui/src/pages/BookingsPage.tsx']), {
    repo: true,
    ui: true,
    mobile: false,
    backend: false,
    contracts: false,
    migrations: false,
  });
});
test('ordinary backend changes avoid UI, mobile, contracts and migrations', () => {
  assert.deepEqual(classifyChangedFiles(['tdf-hq/src/TDF/Server.hs']), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: false,
    migrations: false,
  });
});

test('OpenAPI changes validate both generated clients', () => {
  assert.deepEqual(classifyChangedFiles(['tdf-hq/docs/openapi/api.yaml']), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: true,
    migrations: false,
  });
});

test('schema model changes run backend and migration checks', () => {
  assert.deepEqual(classifyChangedFiles(['tdf-hq/src/TDF/ModelsExtra.hs']), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: false,
    migrations: true,
  });
});

test('production migration runner changes run backend and migration checks', () => {
  assert.deepEqual(classifyChangedFiles(['tdf-hq/production-entrypoint.sh']), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: false,
    migrations: true,
  });
});

test('root dependency changes cover every JavaScript consumer', () => {
  assert.deepEqual(classifyChangedFiles(['package-lock.json']), {
    repo: true,
    ui: true,
    mobile: true,
    backend: false,
    contracts: true,
    migrations: false,
  });
});

test('pipeline changes fail safe across every scope', () => {
  assert.deepEqual(classifyChangedFiles(['.github/workflows/ci.yml']), allChangeScopes());
});

test('backend artifact builds can force backend validation', () => {
  assert.deepEqual(classifyChangedFiles(['README.md'], { forceBackend: true }), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: false,
    migrations: false,
  });
});
