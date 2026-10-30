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

for (const source of [
  'tdf-hq-ui/src/components/notificationTarget.ts',
  'scripts/generate-notification-navigation.mjs',
]) {
  test(`notification navigation drift is checked when only ${source} changes`, () => {
    assert.deepEqual(classifyChangedFiles([source]), {
      repo: true,
      ui: source.startsWith('tdf-hq-ui/'),
      mobile: true,
      backend: false,
      contracts: false,
      migrations: false,
    });
  });
}

test('public booking HTTP concurrency harness selects backend validation', () => {
  assert.deepEqual(classifyChangedFiles(['scripts/test-public-booking-http-concurrency.sh']), {
    repo: true,
    ui: false,
    mobile: false,
    backend: true,
    contracts: false,
    migrations: false,
  });
});

test('music catalog search runner alone selects real PostgreSQL backend validation', () => {
  assert.deepEqual(classifyChangedFiles(['scripts/test-music-catalog-search.mjs']), {
    repo: true, ui: false, mobile: false, backend: true, contracts: false, migrations: false,
  });
});

test('music source projection runner alone selects real PostgreSQL backend validation', () => {
  assert.deepEqual(classifyChangedFiles(['scripts/test-music-source-projection.mjs']), {
    repo: true, ui: false, mobile: false, backend: true, contracts: false, migrations: false,
  });
});

test('music track search runner alone selects real PostgreSQL backend validation', () => {
  assert.deepEqual(classifyChangedFiles(['scripts/test-music-track-search.mjs']), {
    repo: true, ui: false, mobile: false, backend: true, contracts: false, migrations: false,
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

test('Mobile pins and contract definition changes select generated API validation', () => {
  for (const file of ['tdf-mobile', 'tdf-hq/src/TDF/API.hs',
    'tdf-hq/src/TDF/API/Social.hs', 'tdf-hq/src/TDF/DTO.hs',
    'tdf-hq/src/TDF/DTO/Social.hs', 'tdf-hq/docs/openapi/social-v2.yaml',
    'scripts/check-generated-api.mjs', 'scripts/__tests__/generated-api-conformance.test.mjs']) {
    assert.equal(classifyChangedFiles([file]).contracts, true, file);
  }
});

test('every event operations HTTP runner selects backend validation', () => {
  for (const file of [
    'scripts/test-event-operations-http.sh',
    'scripts/test-event-operations-http-ci.sh',
    'scripts/run-event-operations-http-harness.sh',
    'scripts/__tests__/event-operations-http-runner.test.mjs',
  ]) {
    assert.deepEqual(classifyChangedFiles([file]), {
      repo: true, backend: true, ui: false, mobile: false, contracts: false, migrations: false,
    });
  }
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

test('merch checkout expiry runner and SQL assertions select migration validation', () => {
  for (const file of [
    'scripts/test-artist-merch-storefronts-migration.sh',
    'tdf-hq/test/integration/merch_checkout_expiry_assertions.sql',
  ]) {
    assert.equal(classifyChangedFiles([file]).migrations, true, file);
  }
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

test('ledger runtime and verification changes select the real PostgreSQL backend lane', () => {
  for (const file of [
    'scripts/lib/production-release.mjs',
    'scripts/render-production-schema-verification.mjs',
    'scripts/test-ledger-posting.sh',
    'scripts/test-provider-retry-runtime.sh',
    'scripts/verify-payment-intent-expiry.py',
    'scripts/__tests__/ledger-posting-upgrade.py',
    'scripts/__tests__/ledger-posting-snapshot.py',
    'scripts/__tests__/ledger-posting-namespace.py',
    'scripts/__tests__/ledger-startup-namespace.mjs',
  ]) {
    assert.equal(classifyChangedFiles([file]).backend, true, file);
  }
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
  for (const file of ['.github/workflows/ci.yml', 'scripts/ci-job-admission.mjs',
    'scripts/__tests__/ci-job-admission.test.mjs']) {
    assert.deepEqual(classifyChangedFiles([file]), allChangeScopes(), file);
  }
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

test('declared lifecycle models and their bindings select migrated-schema checks', () => {
  for (const file of [
    'docs/revenue-platform/formal-model.yaml',
    'docs/music-directory/formal-model.yaml',
    'formal/system/state-machine-bindings.json',
    'scripts/check-state-machines.py',
    'scripts/test-state-machines.py',
  ]) {
    assert.equal(classifyChangedFiles([file]).migrations, true, file);
  }
  assert.equal(classifyChangedFiles(['docs/revenue-platform/operator-runbooks.md']).migrations, false);
});
