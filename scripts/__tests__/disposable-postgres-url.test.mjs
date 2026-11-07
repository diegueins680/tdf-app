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

test('invoice shell runner owns its database and refuses routing overrides before creation', async () => {
  const { mkdtemp, writeFile, readFile, rm } = await import('node:fs/promises');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { spawnSync } = await import('node:child_process');
  const folder = await mkdtemp(join(tmpdir(), 'tdf-invoice-routing-'));
  const trace = join(folder, 'trace.jsonl');
  try {
    for (const command of ['createdb', 'dropdb', 'stack']) {
      await writeFile(join(folder, command), `#!${process.execPath}
const fs = require('node:fs');
fs.appendFileSync(process.env.INVOICE_STUB_TRACE, JSON.stringify({
  command: ${JSON.stringify(command)}, args: process.argv.slice(2),
  connection: process.env.TDF_SESSION_ISSUE_TEST_DB,
  host: process.env.PGHOST, port: process.env.PGPORT,
  user: process.env.PGUSER, password: process.env.PGPASSWORD
}) + '\\n');
if (${JSON.stringify(command)} === 'stack') {
  console.log(process.env.INVOICE_STUB_OUTPUT);
  process.exit(Number(process.env.INVOICE_STUB_STACK_EXIT || 0));
}
if (${JSON.stringify(command)} === 'createdb') process.exit(Number(process.env.INVOICE_STUB_CREATE_EXIT || 0));
`, { mode: 0o700 });
    }
    const clean = { ...process.env, PATH: folder + ':' + process.env.PATH, CI: 'false',
      PGHOST: '127.0.0.1', PGPORT: '5432', PGUSER: 'synthetic-user',
      PGPASSWORD: "synthetic 'password host=remote.example", TDF_STACK_FLAGS: '',
      TDF_SESSION_ISSUE_TEST_DB: 'dbname=production', INVOICE_STUB_TRACE: trace,
      INVOICE_STUB_OUTPUT: '2 examples, 0 failures', INVOICE_STUB_STACK_EXIT: '0',
      INVOICE_STUB_CREATE_EXIT: '0' };
    for (const name of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) delete clean[name];
    const run = async (overrides = {}) => {
      await writeFile(trace, '');
      const result = spawnSync('sh', ['scripts/test-session-invoice-issue.sh'], {
        encoding: 'utf8', env: { ...clean, ...overrides },
      });
      const lines = (await readFile(trace, 'utf8')).trim();
      return { result, calls: lines ? lines.split('\n').map((line) => JSON.parse(line)) : [] };
    };
    for (const overrides of [
      { PGHOST: 'remote.example' }, { PGHOST: 'localhost host=remote.example' },
      { PGHOST: 'postgres' }, { PGHOST: '/tmp/foreign-socket' },
      { PGHOSTADDR: '192.0.2.1' }, { PGSERVICE: 'production' },
      { PGSERVICEFILE: '/tmp/foreign-service.conf' },
      { PGPORT: '0' }, { PGPORT: '65536' }, { PGPORT: 'abc' },
      { PGPORT: '-1' }, { PGPORT: '5432 host=remote.example' },
    ]) {
      const { result, calls } = await run(overrides);
      assert.notEqual(result.status, 0);
      assert.deepEqual(calls, [], 'unsafe routing must fail before any database/Stack command');
    }
    const failedCreate = await run({ INVOICE_STUB_CREATE_EXIT: '73' });
    assert.equal(failedCreate.result.status, 73);
    assert.deepEqual(failedCreate.calls.map(({ command }) => command), ['createdb']);
    const ownedNames = new Set();
    for (const overrides of [
      {}, { INVOICE_STUB_OUTPUT: '\u001b[32m2 examples, 0 failures\u001b[0m' },
      { INVOICE_STUB_OUTPUT: '\u001b[1;32m12 examples, 0 failures\u001b[0m' },
      { PGHOST: 'localhost' }, { PGHOST: 'postgres', CI: 'true' },
      { PGPORT: '1' }, { PGPORT: '65535' },
      { PGUSER: 'synthetic role host=remote.example' },
      { PGHOST: '', PGPORT: '', PGUSER: '', PGPASSWORD: '' },
      { INVOICE_STUB_OUTPUT: '0 examples, 0 failures' },
      { INVOICE_STUB_OUTPUT: '1 example, 0 failures' },
      { INVOICE_STUB_OUTPUT: 'no summary' },
      { INVOICE_STUB_OUTPUT: '\u001b[32m0 examples, 0 failures\u001b[0m' },
      { INVOICE_STUB_OUTPUT: '\u001b[32m1 example, 0 failures\u001b[0m' },
      { INVOICE_STUB_OUTPUT: '\u001b[33m2 examples, 0 failures, 1 pending\u001b[0m' },
      { INVOICE_STUB_OUTPUT: '\u001b[31m2 examples, 1 failure\u001b[0m' },
      { INVOICE_STUB_OUTPUT: '\u001b[33m2 examples, 0 failures, 1 pending\u001b[0m', INVOICE_STUB_STACK_EXIT: '1' },
      { INVOICE_STUB_OUTPUT: '\u001b[31m2 examples, 1 failure\u001b[0m', INVOICE_STUB_STACK_EXIT: '1' },
      { INVOICE_STUB_OUTPUT: '2 examples, 1 failure', INVOICE_STUB_STACK_EXIT: '1' },
      { INVOICE_STUB_OUTPUT: '1 example, 0 failures, 1 pending', INVOICE_STUB_STACK_EXIT: '1' },
    ]) {
      const { result, calls } = await run(overrides);
      const fails = overrides.INVOICE_STUB_STACK_EXIT === '1'
        || overrides.INVOICE_STUB_OUTPUT === '0 examples, 0 failures'
        || overrides.INVOICE_STUB_OUTPUT === '1 example, 0 failures'
        || overrides.INVOICE_STUB_OUTPUT === 'no summary'
        || overrides.INVOICE_STUB_OUTPUT === '\u001b[32m0 examples, 0 failures\u001b[0m'
        || overrides.INVOICE_STUB_OUTPUT === '\u001b[32m1 example, 0 failures\u001b[0m'
        || overrides.INVOICE_STUB_OUTPUT === '\u001b[33m2 examples, 0 failures, 1 pending\u001b[0m'
        || overrides.INVOICE_STUB_OUTPUT === '\u001b[31m2 examples, 1 failure\u001b[0m';
      assert.equal(result.status === 0, !fails, result.stderr);
      assert.equal(result.stdout, `${overrides.INVOICE_STUB_OUTPUT || clean.INVOICE_STUB_OUTPUT}\n`,
        'raw Stack output, including ANSI SGR bytes, remains untouched');
      assert.deepEqual(calls.map(({ command }) => command), ['createdb', 'stack', 'dropdb']);
      const database = calls[0].args[0];
      assert.match(database, /^tdf_session_issue_test_[0-9]+$/);
      assert.ok(!ownedNames.has(database), 'each invocation owns a distinct database name');
      ownedNames.add(database);
      assert.deepEqual(calls[0].args, [database]);
      assert.deepEqual(calls[2].args, ['--if-exists', database]);
      assert.equal(calls[1].connection, `dbname=${database}`);
      for (const call of calls) {
        assert.equal(call.host, overrides.PGHOST || clean.PGHOST);
        assert.equal(call.port, overrides.PGPORT || '5432');
        assert.equal(call.user, overrides.PGUSER === '' ? 'postgres' : (overrides.PGUSER || clean.PGUSER));
        assert.equal(call.password, overrides.PGPASSWORD === '' ? 'postgres' : clean.PGPASSWORD);
      }
      assert.ok(calls[1].args.includes('--test-arguments=--match "Session invoice issuance on PostgreSQL" --fail-on=pending'));
    }
  } finally { await rm(folder, { recursive: true, force: true }); }
});
