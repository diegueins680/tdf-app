import test from 'node:test';
import assert from 'node:assert/strict';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

test('admits named local and explicitly opted-in CI fixtures', () => {
  for (const value of [
    'postgresql://127.0.0.1/local_test',
    'postgres://localhost:5432/local_test',
    'postgresql://user:pass@localhost:5432/orders_test',
    'postgres://user:pa%40ss@127.0.0.1:5432/orders-local_test',
  ]) {
    assert.ok(disposablePostgresUrl(value));
  }
  assert.ok(disposablePostgresUrl('postgresql://postgres:postgres@postgres:5432/tdf_hq_automatic_migration_test', { ci: true }));
});
for (const value of [
  'postgresql://127.0.0.1/local_test?host=remote.example',
  'postgresql://127.0.0.1/local_test?dbname=production',
  'postgresql://127.0.0.1/local_test?service=production',
  'postgresql://localhost/local_test#ignored',
  'postgresql://remote.example/local_test',
  'postgresql://localhost:secret@db.example/orders_test',
  'postgresql://127.0.0.1:5432@db.example/orders_test',
  'postgresql://localhost:0/orders_test',
  'postgresql://localhost:65536/orders_test',
  ' postgresql://localhost/orders_test',
  'postgresql://localhost/production',
  'postgresql://localhost/path/local_test',
  'postgresql://localhost/%70roduction_test',
  'https://localhost/local_test',
  'host=localhost dbname=production',
]) test(`rejects unsafe fixture connection form: ${value}`, () => {
  assert.throws(() => disposablePostgresUrl(value, { ci: true }));
});
test('CI service name is not a local default', () => {
  assert.throws(() => disposablePostgresUrl('postgresql://postgres/local_test'));
});

for (const name of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) {
  test(`rejects inherited libpq routing override ${name}`, () => {
    assert.throws(() => disposablePostgresUrl('postgresql://127.0.0.1/local_test', {
      env: { [name]: 'synthetic-override' },
    }));
  });
}
test('allows password and timeout without changing the connection destination', () => {
  assert.ok(disposablePostgresUrl('postgresql://127.0.0.1/local_test', {
    env: { PGPASSWORD: 'synthetic', PGCONNECT_TIMEOUT: '2' },
  }));
});

test('confirmation shell runner rejects unsafe routing before any database or Stack call', async () => {
  const { mkdtemp, writeFile, rm } = await import('node:fs/promises');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { spawnSync } = await import('node:child_process');
  const folder = await mkdtemp(join(tmpdir(), 'tdf-confirmation-routing-'));
  try {
    for (const command of ['psql', 'stack']) {
      await writeFile(join(folder, command), '#!/bin/sh\necho FORBIDDEN_CALL >&2\nexit 99\n', { mode: 0o700 });
    }
    for (const dsn of [
      'postgresql://remote.example/tdf_ticket_confirmation_worker_test',
      'postgresql://127.0.0.1/production',
      'postgresql://127.0.0.1/tdf_ticket_confirmation_worker_test?host=remote.example',
    ]) {
      const result = spawnSync('bash', ['scripts/test-ticket-confirmation.sh'], {
        encoding: 'utf8', env: { PATH: folder + ':' + process.env.PATH, TICKET_CONFIRMATION_TEST_DSN: dsn },
      });
      assert.notEqual(result.status, 0);
      assert.doesNotMatch(result.stderr, /FORBIDDEN_CALL/);
    }
    for (const variable of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) {
      const result = spawnSync('bash', ['scripts/test-ticket-confirmation.sh'], {
        encoding: 'utf8', env: { PATH: folder + ':' + process.env.PATH,
          TICKET_CONFIRMATION_TEST_DSN: 'postgresql://127.0.0.1/tdf_ticket_confirmation_worker_test', [variable]: 'synthetic' },
      });
      assert.notEqual(result.status, 0);
      assert.doesNotMatch(result.stderr, /FORBIDDEN_CALL/);
    }
  } finally { await rm(folder, { recursive: true, force: true }); }
});

test('ticket payment runner validates routing before creating its fixture database', async () => {
  const { mkdtemp, writeFile, rm } = await import('node:fs/promises');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { spawnSync } = await import('node:child_process');
  const folder = await mkdtemp(join(tmpdir(), 'tdf-ticket-gate-routing-'));
  try {
    for (const command of ['createdb', 'dropdb', 'psql', 'stack']) {
      await writeFile(join(folder, command), '#!/bin/sh\necho DATABASE_COMMAND_REACHED >&2\nexit 77\n', { mode: 0o700 });
    }
    const clean = { ...process.env, PATH: folder + ':' + process.env.PATH, CI: 'false' };
    for (const name of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) delete clean[name];
    for (const overrides of [
      { PGHOST: 'remote.example' },
      { PGHOST: 'localhost:secret@remote.example' },
      { PGHOST: 'postgres' },
      { PGHOST: '127.0.0.1', PGSERVICE: 'synthetic-remote' },
      { PGHOST: '127.0.0.1', PGHOSTADDR: '192.0.2.1' },
    ]) {
      const result = spawnSync('sh', ['scripts/test-ticket-payment-gate.sh'], {
        encoding: 'utf8', env: { ...clean, ...overrides },
      });
      assert.notEqual(result.status, 0);
      assert.doesNotMatch(result.stderr, /DATABASE_COMMAND_REACHED/);
    }
    const accepted = spawnSync('sh', ['scripts/test-ticket-payment-gate.sh'], {
      encoding: 'utf8', env: { ...clean, PGHOST: '127.0.0.1' },
    });
    assert.equal(accepted.status, 77);
    assert.match(accepted.stderr, /DATABASE_COMMAND_REACHED/);
  } finally { await rm(folder, { recursive: true, force: true }); }
});
